;;; wandbox.el --- Wandbox API Library for Emacs

;; Copyright (C) 2013-2014 KOBAYASHI Shigeru

;; Author: KOBAYASHI Shigeru (kosh) <shigeru.kb@gmail.com>
;; URL: https://github.com/kosh04/emacs-wandbox
;; Version: 0.4.1
;; Package-Requires: ((emacs "24") (json "1.3"))
;; Keywords: c, programming, tools
;; Created: 2013/11/22
;; License: MIT License (see LISENCE)

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; wandbox.el is wandbox (online compiler) interface.
;; You can compile and run code snippets by using wandbox API.

;; Wandbox Home: http://melpon.org/wandbox/

;;; Example:

;; ## Use Interactive
;;
;; M-x wandbox-compile-file   - Compile with file contents
;; M-x wandbox-compile-region - Compile marked region
;; M-x wandbox-compile-buffer - Compile current buffer
;;
;; ## Use on Emacs Lisp
;;
;; (wandbox :compiler "gcc-head" :options "warning" :code "main(){}")
;; (wandbox :lang "C" :compiler-option "-lm" :file "/path/to/prog.c" :save t)
;; (wandbox :lang "perl" :code "while (<>) { print uc($_); }" :stdin "hello")
;; (wandbox :lang "ruby" :code "p ARGV" :runtime-option '("1" "2" "3"))
;;
;; (add-to-list 'wandbox-profiles '(:name "User Profile" :compiler "clang-head"))
;; (wandbox-compile :name "User Profile" :code "...")

;;; Change Log:

;; 2014/12/09 ver 0.4.1  add Testing and Cask
;; 2014/12/06 ver 0.4.0  profiles are generated from /wandbox/api/list.json
;; 2014/08/05 ver 0.3.5  permalink api available (add :save option).
;; 2014/01/25 ver 0.3.0  gist snippet available.
;; 2014/01/10 ver 0.2.1  commit to github. add wandbox-eval-with.
;; 2013/12/30 ver 0.2    keyword arguments and profile available.
;; 2013/11/22 ver 0.1    first commit.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'url-http)
(require 'json)

(defvar wandbox-profiles nil
  "Wandbox copmiler profiles (set of plist).")

(defvar wandbox-response-keywords
  '(
    ;;"compiler_output"
    ;;"compiler_error"
    "compiler_message"
    ;;"program_output"
    ;;"program_error"
    "program_message"
    "status"
    "signal"
    ;;"permlink"
    "url"
    )
  "Wandbox compile api response keywords.")

(defvar wandbox-compilers nil
  "List compilers available in wandbox.")

(defvar wandbox-precompiled-hook nil
  "Hook run before post wandbox.
Return value will be merged into the old profile.")

(defvar wandbox-permalink-action #'browse-url
  "Specify function to execute when you run `wandbox-compile' with permalink option (:save).")

(eval-when (compile load eval)
  (defun wandbox-fetch (src)
    "Fetch SRC contains (filename or url)."
    (with-temp-buffer
      (if (string-match "http[s]?://" src)
          (url-insert-file-contents src)
          (insert-file-contents src))
      (buffer-string)))

  (defun wandbox-json-read (string)
    "Decode json object in STRING."
    (let ((json-key-type 'string))
      (json-read-from-string string)))

  (defun wandbox-json-load (src)
    "Encode json object from SRC."
    (wandbox-json-read (wandbox-fetch src)))

  (defun wandbox-fetch-compilers ()
    "Fetch compilers available in wandbox."
    (wandbox-json-load "http://melpon.org/wandbox/api/list.json"))
  )

(defun wandbox-merge-plist (&rest args)
  "Merge all the given ARGS into a new plist."
  (let ((result (car args)))
    (dolist (plist (cdr args))
      (loop for (key value) on plist by #'cddr
            do (plist-put result key value)))
    result))

(defsubst wandbox--log (format &rest args)
  (let ((msg (apply #'format format args)))
    (setq msg (replace-regexp-in-string "\n" "" msg  nil t)) ; one-line
    (message "Wandbox: %s" msg)))

(defun wandbox-make-profiles ()
  "Generate profiles from `wandbox-compilers'."
  (mapcar (lambda (compiler)
            `(:lang ,(cdr (assoc "language" compiler))
              :name ,(cdr (assoc "display-name" compiler))
              :compiler ,#1=(cdr (assoc "name" compiler))
              :options ,(let ((opts (wandbox-default-compiler-options #1#)))
                          (if (not (string= opts ""))
                              opts))
              :ext ,(let ((cmd (cdr (assoc "display-compile-command" compiler))))
                      ;; match "gcc prog.c" -> "c"
                      ;; match "mcs -out:prog.exe prog.cs" -> "cs"
                      (if (string-match "\\s-\\<prog\\.\\([A-Za-z0-9]+\\)\\>" cmd)
                          (match-string 1 cmd)))))
          (wandbox-list-compilers)))

(defun wandbox-buffer-profile ()
  "Returns embbedded wandbox parameters in current buffer.
For example:
// hello.c
// #wandbox compiler: gcc-head
// #wandbox compiler-option: -lm
It returns
\(:compiler \"gcc-head\" :compiler-option \"-lm\"\)"
  (let ((params nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#wandbox \\([-a-z]+\\): \\(.+\\)$" nil t)
        (let ((key (intern (concat ":" (buffer-substring-no-properties
                                        (match-beginning 1)
                                        (match-end 1)))))
              (val (buffer-substring-no-properties
                    (match-beginning 2) 
                    (match-end 2))))
          (setq params (plist-put params key val)))))
    params))

(defun wandbox-fetch-as-profile (src)
  "Return wandbox profile, contains (:code *SRC ...)."
  (with-temp-buffer
    (insert-file-contents src)
    (plist-put (wandbox-buffer-profile) :code (buffer-string))))

(defun wandbox-list-compilers ()
  "Return compilers available in wandbox."
  (unless wandbox-compilers
    (setq wandbox-compilers
          (eval-when-compile
            (wandbox-fetch-compilers))))
  wandbox-compilers)

(defun wandbox-compiler-names ()
  "Return compiler names available in wandbox."
  (mapcar (lambda (x) (cdr (assoc "name" x)))
          (wandbox-list-compilers)))

(defun wandbox-compiler-exist-p (name)
  "Return non-nil if NAME is available compiler name in wandbox."
  (member name (wandbox-compiler-names)))

(defun wandbox-default-compiler-options (compiler)
  "Return the COMPILER default option."
  (labels ((mapcan (fn list &rest more-list)
             (apply #'nconc (apply #'mapcar fn list more-list)))
           (join (list separator)
             (mapconcat #'identity list separator)))
    (join (mapcan (lambda (o)
                    (let ((x (or (cdr (assoc "default" o)) "")))
                      (cond ((stringp x) (list x))
                            ((eq x t) (list (cdr (assoc "name" o))))
                            (t nil))))
                  (dolist (x (append (wandbox-list-compilers) nil)) ; array->list
                    (if (member (cons "name" compiler) x)
                        (return (cdr (assoc "switches" x))))))
          ",")))

(defun* wandbox-build-request-data (&rest profile &allow-other-keys)
  "Build JSON data to post to Wandox API.
PROFILE is property list. e.g. (:compiler COMPILER-NAME :options OPTS ...)"
  (let ((lang (plist-get profile :lang))
        (name (plist-get profile :name))
        (file (plist-get profile :file)))
    (setq profile
          (wandbox-merge-plist profile
                               (if lang (wandbox-find-profile :lang lang))
                               (if name (wandbox-find-profile :name name))
                               (if (and file (file-exists-p file))
                                   (wandbox-fetch-as-profile file)))))
  (dolist (f wandbox-precompiled-hook)
    (setq profile (wandbox-merge-plist profile (apply f profile))))

  (labels ((join-as-string (list separator)
             (mapconcat #'(lambda (x) (format "%s" x)) list separator))
           (val (x)
             (or (plist-get profile x) ""))
           (bool (x)
             (if (plist-get profile x) t :json-false))
           (raw (x)
             (let ((v (val x)))
               (if (consp v) (join-as-string v "\n") v))))
    (unless (wandbox-compiler-exist-p (val :compiler))
      (error "Unknown compiler: %s" (val :compiler)))
    (let ((alist `(("compiler" . ,(val :compiler))
                   ("options"  . ,(val :options))
                   ("code"     . ,(val :code))
                   ("stdin"    . ,(val :stdin))
                   ("compiler-option-raw" . ,(raw :compiler-option))
                   ("runtime-option-raw"  . ,(raw :runtime-option))
                   ("save" . ,(bool :save))
                   )))
      (prog1
          (json-encode alist)
        ;; debug log
        (labels ((truncate (str)
                   (if (< 20 (length str)) (format "%.20s..." str) str)))
          (dolist (key '("code" "stdin"))
            (setf #1=(cdr (assoc key alist)) (truncate #1#)))
          (wandbox--log "build request: %s" (json-encode alist))
          ))
      )))

(defun wandbox-format-url-buffer (status)
  (declare (special url-http-end-of-headers))
  (let ((buf (current-buffer)))
    (unwind-protect
         (progn
           (when (equal (car status) :error)
             (error "Return HTTP error: %s" (cdr status)))
           (let* ((http-body (decode-coding-string
                              (buffer-substring (1+ url-http-end-of-headers) (point-max))
                              'utf-8-unix))
                  (alist (wandbox-json-read http-body))
                  (url (cdr (assoc "url" alist))))
             (wandbox--log "recieve: %s" http-body)
             (when (and url wandbox-permalink-action)
               (funcall wandbox-permalink-action url))
             (with-output-to-temp-buffer "*Wandbox Output*"
               (dolist (res wandbox-response-keywords)
                 (when (assoc res alist)
                   (princ (format "[%s]" (car (assoc res alist))))
                   (terpri)
                   (princ (cdr (assoc res alist)))
                   (terpri)
                   (terpri)))))
           (wandbox--log "Compile...done"))
      (kill-buffer buf))))

(defun* wandbox-post (json &key (sync nil))
  "Request compile api with JSON data."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data json))
    (if sync
        (wandbox-json-load #1="http://melpon.org/wandbox/api/compile.json")
        (progn
          (url-retrieve #1# 'wandbox-format-url-buffer)
          t))))

;;;###autoload
(defun* wandbox-compile (&rest profile
                         &key
                         compiler options code stdin
                         compiler-option runtime-option
                         lang name file
                         (save nil)
                         (sync nil)
                         &allow-other-keys)
  "Compile CODE as COMPILER's source code.
If NAME specified, select compiler template from `wandbox-profiles'.
If FILE specified, compile FILE contents instead of code."
  (wandbox-post (apply #'wandbox-build-request-data profile) :sync sync))

;;;###autoload
(defalias 'wandbox #'wandbox-compile)

;; see also: http://developer.github.com/v3/gists/
(defun wandbox-fetch-gist (id)
  "Fetch a single gist from ID."
  (let ((url (format "https://api.github.com/gists/%d" id)))
    (wandbox-json-load url)))

(defun* wandbox-option-gist (&key gist gist-file &allow-other-keys)
  (when gist
    (let* ((data (wandbox-fetch-gist gist))
           (fileinfo (if gist-file
                         (cdr (assoc gist-file (cdr (assoc "files" data))))
                         (nth 0 (cdr (assoc "files" data)))))
           ;; NOTE: gist snippet may not have language (as plain/text)
           (lang (cdr (assoc "language" fileinfo)))
           (content (cdr (assoc "content" fileinfo)))
           (profile (wandbox-find-profile :lang lang)))
      (plist-put profile :code content))))

(defun* wandbox-option-code (&key code-before code code-after &allow-other-keys)
  (let ((profile nil))
    (plist-put profile :code (concat code-before code code-after))))

(add-to-list 'wandbox-precompiled-hook #'wandbox-option-gist t)
(add-to-list 'wandbox-precompiled-hook #'wandbox-option-code t)

(defun wandbox-find-profile (key item)
  "Find the profile match KEY and ITEM."
  ;; (find item wandbox-profiles
  ;;       :key (lambda (x) (plist-get x key))
  ;;       :test #'string-equal)
  (when (stringp item)
    (dolist (x wandbox-profiles)
      (let ((val (plist-get x key)))
        (if (and val (string-equal (downcase val) (downcase item)))
            (return (copy-sequence x)))))))

(defun* wandbox-read-profile (&optional (key :name))
  "Read a profile setting.
Completion list is generate from matchs KEY."
  (let* ((completion-ignore-case t)
         (items (mapcar (lambda (x) (plist-get x key))
                        wandbox-profiles))
         (name (completing-read "Profile: " items)))
    (wandbox-find-profile key name)))

;;;###autoload
(defun wandbox-compile-file (filename)
  "Compile FILENAME contents.
Compiler profile is determined by file extension."
  (interactive "fFile to wandbox: ")
  (let ((profile (or (wandbox-find-profile :ext (file-name-extension filename))
                     (wandbox-read-profile))))
    (apply #'wandbox-compile :file filename profile)))

;;;###autoload
(defun wandbox-compile-region (from to)
  "Compile specified region."
  (interactive "r")
  (let ((profile (or (wandbox-buffer-profile)
                     (wandbox-find-profile :ext (file-name-extension (buffer-file-name)))
                     (wandbox-read-profile)))
        (code (buffer-substring-no-properties from to)))
    (apply #'wandbox-compile :code code profile)))

;;;###autoload
(defun wandbox-compile-buffer ()
  "Compile current buffer."
  (interactive)
  (wandbox-compile-region (point-min) (point-max)))

;;;###autoload
(defmacro* wandbox-eval-with ((&rest options) &body form)
  "Evaluate FORM as S-expression."
  (declare (indent 1))
  (let ((print-circle t))
    `(wandbox-compile :name "CLISP"
                      :code ,(prin1-to-string `(print (progn ,@form)))
                      ,@options)))

(defun wandbox-tweet (url)
  (browse-url (concat "https://twitter.com/intent/tweet?text=Wandbox&url="
                      (url-hexify-string url))))

;; [Tweet This]
;; (setq wandbox-permalink-function #'wandbox-tweet)

;; eval-when (load eval)
(unless wandbox-profiles
  (setq wandbox-profiles (wandbox-make-profiles)))

(provide 'wandbox)

;;; wandbox.el ends here
