;;; wandbox.el --- Wandbox interface for Emacs

;; Copyright (C) 2013-2014 KOBAYASHI Shigeru

;; Author: KOBAYASHI Shigeru (kosh) <shigeru.kb@gmail.com>
;; URL: https://github.com/kosh04/emacs-wandbox
;; Version: 0.3.5
;; Created: 2013/11/22
;; Keywords: c, tools
;; License: MIT Lisense (see LISENCE)

;; This file is NOT part of GNU Emacs.

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

(defvar wandbox-profiles
  '(
    (:lang "C++" :name "gcc HEAD" :compiler "gcc-head" :options "warning,boost-1.55,sprout,gnu++1y" :ext "cc")
    (:lang "C++" :name "clang HEAD" :compiler "clang-head" :options "warning,boost-1.55,sprout,gnu++1y" :ext "cc")
    (:lang "C++" :name "gcc" :compiler "gcc-4.8.2" :options "warning,boost-1.55,sprout,gnu++1y" :ext "cc")
    (:lang "C++" :name "clang" :compiler "clang-3.3" :options "warning,boost-1.55,sprout,gnu++1y" :ext "cc")
    (:lang "C++" :name "CPP" :compiler "gcc-4.8.2-pp" :options "cpp-p" :ext "cpp")
    (:lang "C" :name "C" :compiler "gcc-4.8.2-c" :options "warning,c11" :ext "c")
    (:lang "C#" :name "C#" :compiler "mcs-3.2.0" :ext "cs")
    (:lang "D" :name "D" :compiler "gdc-head" :ext "d")
    (:lang "Haskell" :name "Haskell" :compiler "ghc-7.6.3" :options "haskell-warning" :ext "hs")
    (:lang "Perl" :name "perl-devel HEAD" :compiler "perl-head" :ext "pl")
    (:lang "Perl" :name "perl-devel" :compiler "perl-5.19.2" :ext "pl")
    (:lang "Perl" :name "perl" :compiler "perl-5.18.0" :options "perl5.18.0" :ext "pl")
    (:lang "Python" :name "python HEAD" :compiler "python-head" :ext "py")
    (:lang "Python" :name "python2.7 HEAD" :compiler "python-2.7-head" :ext "py")
    (:lang "Python" :name "python3.3" :compiler "python-3.3.2" :ext "py")
    (:lang "Python" :name "python2.7" :compiler "python-2.7.3")
    (:lang "Python" :name "PyPy" :compiler "pypy-2.1" :ext "py")
    (:lang "Ruby" :name "Ruby" :compiler "ruby-head" :ext "rb")
    (:lang "Ruby" :name "Ruby2.0" :compiler "ruby-2.0.0-p247" :ext "rb")
    (:lang "Ruby" :name "Ruby1.9" :compiler "ruby-1.9.3-p0" :ext "rb")
    (:lang "Ruby" :name "MRuby" :compiler "mruby-head" :ext "rb")
    (:lang "Erlang" :name "Erlang" :compiler "erlang-maint" :ext "erl")
    (:lang "Rust" :name "Rust" :compiler "rust-head" :ext "rs")
    (:lang "Shell" :name "Bash" :compiler "bash" :ext "sh")
    (:lang "SQL" :name "SQLite" :compiler "sqlite-3.8.1" :ext "sql")
    (:lang "Lua" :name "Lua" :compiler "lua-5.2.2" :ext "lua")
    (:lang "PHP" :name "PHP" :compiler "php-5.5.6" :ext "php")
    (:lang "Lazy K" :name "Lazy K" :compiler "lazyk" :ext "lazy") ; :lang ?
    (:lang "Common Lisp" :name "CLISP" :compiler "clisp-2.49.0" :ext "lisp")
    (:lang "Pascal" :name "Free Pascal" :compiler "fpc-2.6.2" :ext "pas")
    (:lang "Java" :name "OpenJDK" :compiler "java7-openjdk" :ext "java")
    (:lang "Groovy" :name "Groovy" :compiler "groovy-2.2.1" :ext "groovy")
    (:lang "JavaScript" :name "node" :compiler "node-0.10.24" :ext "js")
    (:lang "JavaScript" :name "SpiderMonkey" :compiler "mozjs-24.2.0" :ext "js")
    (:lang "CoffeeScript" :name "coffee" :compiler "coffee-script-head" :ext "coffee")
    (:lang "CoffeeScript" :name "coffee1.7" :compiler "coffee-script-1.7.1" :ext "coffee")
    (:lang "CoofeeScript" :name "coffee1.6" :compiler "coffee-script-1.6.3" :ext "coffee")
    )
  "Wandbox copmiler profiles (set of property list)")

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
    ;;"permlink" ; perhaps "permalink" ?
    "url"
    ))

(defvar wandbox-compilers nil)

(defvar wandbox-precompiled-hook nil
  "Hook run before post wandbox.
Return value will be merged into the old profile.")

(defvar wandbox-permalink-action #'browse-url
  "Specify function to execute when you run `wandbox-compile' with permalink option (:save).")

(defun wandbox-fetch (src)
  (with-temp-buffer
    (if (string-match "http[s]?://" src)
        (url-insert-file-contents src)
        (insert-file-contents src))
    (buffer-string)))

(defun wandbox-merge-plist (&rest args)
  (let ((result (car args)))
    (dolist (plist (cdr args))
      (loop for (key value) on plist by #'cddr
            do (plist-put result key value)))
    result))

(defun wandbox-json-read (string)
  (let ((json-key-type 'string))
    (json-read-from-string string)))

(defun wandbox-json-load (src)
  (wandbox-json-read (wandbox-fetch src)))

(defun wandbox-fetch-compilers ()
  (wandbox-json-load "http://melpon.org/wandbox/api/list.json"))

(defun wandbox-list-compilers ()
  (unless wandbox-compilers
    (setq wandbox-compilers (wandbox-fetch-compilers)))
  wandbox-compilers)

(defun wandbox-compiler-names ()
  "Return the available compiler list."
  (mapcar (lambda (x) (cdr (assoc "name" x)))
          (wandbox-list-compilers)))

(defun wandbox-compiler-exist-p (name)
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
                                   `(:code ,(wandbox-fetch file))))))
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
          (message "Wandbox build request: %s" (json-encode alist))))
      )))

(defun wandbox-format-url-buffer (status)
  (declare (special url-http-end-of-headers))
  (let ((buf (current-buffer)))
    (unwind-protect
         (progn
           (when (equal (car status) :error)
             (error "return error: %s" (cdr status)))
           (let* ((http-body (decode-coding-string
                              (buffer-substring (1+ url-http-end-of-headers) (point-max))
                              'utf-8-unix))
                  (alist (wandbox-json-read http-body))
                  (url (cdr (assoc "url" alist))))
             (message "Wandbox recieve message: %s" http-body)
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
           (message "Compile...done"))
      (kill-buffer buf))))

(defun* wandbox-post (json &key (sync nil))
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data json))
    (if sync
        (wandbox-json-load #1="http://melpon.org/wandbox/api/compile.json")
        (progn
          (url-retrieve #1# 'wandbox-format-url-buffer)
          t))))

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

(defalias 'wandbox #'wandbox-compile)

;; see also: http://developer.github.com/v3/gists/
(defun wandbox-fetch-gist (id)
  "Get a single gist from ID."
  (let ((url (format "https://api.github.com/gists/%d" id))
        (json-key-type 'string))
    (json-read-from-string (wandbox-fetch url))))

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
  ;; (find item wandbox-profiles
  ;;       :key (lambda (x) (plist-get x key))
  ;;       :test #'string-equal)
  (when (stringp item)
    (dolist (x wandbox-profiles)
      (let ((val (plist-get x key)))
        (if (and val (string-equal (downcase val) (downcase item)))
            (return (copy-sequence x)))))))

(defun* wandbox-read-profile (&optional (key :name))
  (let* ((completion-ignore-case t)
         (items (mapcar (lambda (x) (plist-get x key))
                        wandbox-profiles))
         (name (completing-read "Profile: " items)))
    (wandbox-find-profile key name)))

(defun wandbox-compile-file (filename)
  "Compile FILENAME contents.
Compiler profile is determined by file extension."
  (interactive "fFile to wandbox: ")
  (let ((profile (or (wandbox-find-profile :ext (file-name-extension filename))
                     (wandbox-read-profile))))
    (apply #'wandbox-compile :file filename profile)))

(defun wandbox-compile-region (from to)
  "Compile specified region."
  (interactive "r")
  (let ((profile (wandbox-read-profile))
        (code (buffer-substring-no-properties from to)))
    (apply #'wandbox-compile :code code profile)))

(defun wandbox-compile-buffer ()
  "Compile current buffer."
  (interactive)
  (wandbox-compile-region (point-min) (point-max)))

(defmacro* wandbox-eval-with ((&rest options) &body form)
  "Evaluate FORM as S-expression."
  (let ((print-circle t))
    `(wandbox-compile :name "CLISP"
                      :code ,(prin1-to-string `(print (progn ,@form)))
                      ,@options)))

(put 'wandbox-eval-with 'lisp-indent-function 1)

(defun wandbox-tweet (url)
  (browse-url (concat "https://twitter.com/intent/tweet?text=Wandbox&url="
                      (url-hexify-string url))))

;; [Tweet This]
;; (setq wandbox-permalink-function #'wandbox-tweet)

(provide 'wandbox)

;;; wandbox.el ends here
