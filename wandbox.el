;;; wandbox.el --- Wandbox client

;; Copyright (C) 2013-2017 KOBAYASHI Shigeru

;; Author: KOBAYASHI Shigeru (kosh) <shigeru.kb@gmail.com>
;; URL: https://github.com/kosh04/emacs-wandbox
;; Version: 0.6.4
;; Package-Requires: ((emacs "24") (request "0.3.0") (s "1.10.0"))
;; Keywords: tools
;; Created: 2013/11/22
;; License: MIT License

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; wandbox.el is wandbox (online compiler) client tool.
;; You can compile and run code snippets by using wandbox API.

;; Wandbox Home: https://wandbox.org

;; Example
;; -------
;;
;; ## Use Interactive
;;
;; M-x wandbox                 - Alias `wandbox-compile-buffer'
;; M-x wandbox-compile-file    - Compile with file contents
;; M-x wandbox-compile-region  - Compile marked region
;; M-x wandbox-compile-buffer  - Compile current buffer
;; M-x wandbox-insert-template - Insert template snippet
;; M-x wandbox-list-compilers  - Display copilers list
;;
;; Note: if `#wandbox param: value` token found on selected file/buffer,
;; wandbox-compile-file/buffer compiles using those params.
;;
;; ## Use on Emacs Lisp
;;
;; (wandbox :compiler "gcc-head" :options "warning" :code "main(){}")
;; (wandbox :lang "C" :compiler-option "-lm" :file "/path/to/prog.c" :save t)
;; (wandbox :lang "perl" :code "while (<>) { print uc($_); }" :stdin "hello")
;; (wandbox :lang "ruby" :code "p ARGV" :runtime-option '("1" "2" "3"))
;; (wandbox :profiles [(:name "php HEAD") (:name "php")] :code "<? echo phpversion();")
;;
;; (add-to-list 'wandbox-profiles '(:name "User Profile" :compiler "clang-head"))
;; (wandbox-compile :name "User Profile" :code "...")
;;
;; (wandbox-add-server "fetus" "https://wandbox.fetus.jp")
;; (wandbox :code "<?php echo phpversion();" :name "HHVM" :server-name "fetus")

;;; Change Log:

;; 2017-06-02 ver 0.6.4  fix prog.cpp profile as C++
;; 2017-04-26 ver 0.6.3  template API available / show details in the compiler list
;; 2017-03-23 ver 0.6.2  change URL
;; 2016-05-04 ver 0.6.1  customize variables
;; 2016-04-01 ver 0.6.0  use `request' library / selectable wandbox servers
;; 2016-01-25 ver 0.5.1  trim unnecessary Package-Requires
;; 2015-09-05 ver 0.5.0  multiple compile, list compilers, markdown colorize (optional)
;; 2015/02/15 ver 0.4.3  `wandbox' can call interactively
;; 2015/01/09 ver 0.4.2  add `#wandbox' buffer profile
;; 2014/12/09 ver 0.4.1  add Testing and Cask
;; 2014/12/06 ver 0.4.0  profiles are generated from /wandbox/api/list.json
;; 2014/08/05 ver 0.3.5  permalink api available (add :save option).
;; 2014/01/25 ver 0.3.0  gist snippet available.
;; 2014/01/10 ver 0.2.1  commit to github. add wandbox-eval-with.
;; 2013/12/30 ver 0.2    keyword arguments and profile available.
;; 2013/11/22 ver 0.1    first commit.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'json)
(require 'tabulated-list)
(require 's)
(require 'request)

(defgroup wandbox nil
  "Wandbox client for Emacs."
  :prefix "wandbox-"
  :link '(emacs-commentary-link "wandbox")
  :link '(url-link "https://github.com/kosh04/emacs-wandbox")
  :group  'tools)

(defcustom wandbox-response-keywords
  '("compiler_message"
    "program_message"
    "status"
    "signal"
    "url")
  "List of compile api response keywords."
  :group 'wandbox
  :type '(repeat (choice
                  (const "status" :tag "Exit code")
                  (const "signal" :tag "Signal message")
                  (const "compiler_output"  :tag "stdout at compiling")
                  (const "compiler_error"   :tag "stderr at compiling")
                  (const "compiler_message" :tag "merged messages compiler_output and compiler_error")
                  (const "program_output"  :tag "stdout at runtime")
                  (const "program_error"   :tag "stderr at runtime")
                  (const "program_message" :tag "merged messages program_output and program_error")
                  (const "permlink" :tag "/permlink/:link")
                  (const "url" :tag "URL to display on browser"))))

(defcustom wandbox-user-profiles
  '((:lang "C++" :compiler "gcc-head" :ext "cc")
    (:lang "C++" :compiler "gcc-head" :ext "cpp"))
  "User-defined copmiler settings."
  :group 'wandbox
  :type  '(list (plist
                 ;; FIXME: how to select key as `keyword'
                 :key-type   (symbol :tag "Compile parameter")
                 :value-type (choice (string)
                                     (boolean :tag "(:save only)")))))

(define-obsolete-variable-alias 'wandbox-profiles
  'wandbox-user-profiles "wandbox-0.6.4")

(defcustom wandbox-precompiled-hook nil
  "Hook run before post wandbox.
Return value will be merged into the old profile."
  :group 'wandbox
  :type  'hook
  :options '(wandbox-option-gist wandbox-option-code))

(defcustom wandbox-permalink-action #'browse-url
  "Function when you run `wandbox-compile' with permalink option (:save)."
  :group 'wandbox
  :type  'function)

(defcustom wandbox-output-buffer "*Wandbox Output*"
  "Output result buffer name."
  :group 'wandbox
  :type  'string)

(cl-eval-when (compile load eval)

  (defun wandbox-fetch (src)
    "Fetch SRC contains (filename or url)."
    (with-temp-buffer
      (if (string-match "https?://" src)
          (url-insert-file-contents src)
          (insert-file-contents src))
      (buffer-string)))

  (defsubst wandbox--json-decode (string)
    "Decode json object in STRING."
    (let ((json-key-type 'string))
      (json-read-from-string string)))

  (defsubst wandbox--json-load (src)
    "Encode json object from SRC."
    (wandbox--json-decode (wandbox-fetch src)))

  (defsubst wandbox--default-compiler-options (compiler compilers)
    "Return the COMPILER default options.

Example: compiler \"gcc-4.8.2-c\" switches will be \"warning,c11\"."
    (let ((switches
           ;; $[name=compiler].switches
           (cl-loop for c across compilers
                    if (member (cons "name" compiler) c)
                    do (cl-return (cdr (assoc "switches" c))))))
      (s-join "," (cl-loop for s across switches
                           unless (eq #1=(cdr (assoc "default" s)) :json-false)
                           collect (if (stringp #1#) #1# (cdr (assoc "name" s)))))))

  (defsubst wandbox--template-names (compilers)
    (let (names)
      (cl-loop for c across compilers
               do (cl-loop for template across (cdr (assoc "templates" c))
                           do (cl-pushnew template names :test #'string-equal)))
      (nreverse names)))

  (defsubst wandbox--command-to-ext (cmd)
    "Extract prog.ext from CMD (display-compiler-command)."
    (cl-some (lambda (re)
               (if (string-match re cmd)
                   (match-string 1 cmd)))
             (list (rx (+ space) #1=(: word-start "prog." (submatch (+ alnum)) word-end) eos)
                   (rx #1#))))

  (defsubst wandbox--make-profiles (compilers)
    "Generate profiles from `wandbox-server-compilers' as COMPILERS."
    (cl-labels ((make-profile (compiler)
                `(:lang ,(cdr (assoc "language" compiler))
                  :name ,(cdr (assoc "display-name" compiler))
                  :compiler ,#1=(cdr (assoc "name" compiler))
                  :options ,(let ((opts (wandbox--default-compiler-options #1# compilers)))
                              (if (not (string= opts ""))
                                  opts))
                  :ext ,(let ((cmd (cdr (assoc "display-compile-command" compiler))))
                          (if cmd (wandbox--command-to-ext cmd))))))
      (mapcar #'make-profile compilers)))

  (cl-defstruct (wandbox-server
                 (:constructor wandbox-create-server
                  (name location &aux
                        (api/list     (format "%s/api/list.json" location))
                        (api/compile  (format "%s/api/compile.json" location))
                        (api/template (format "%s/api/template/%%s" location))
                        (compilers (wandbox--json-load api/list))
                        (templates (wandbox--template-names compilers))
                        (profiles  (wandbox--make-profiles compilers)))))
    "Server information."
    (name :type string :read-only t)       ; unique name
    (location :type string :read-only t)   ; API BaseURL
    (api/list    :type url :read-only t)
    (api/compile :type url :read-only t)
    (api/template :type url :read-only t)
    (compilers :type list :read-only t)
    (templates :type list :read-only t)
    (profiles :type list))

) ;; end eval-when (compile load eval)

(defvar wandbox-servers
  (list
   (eval-when-compile
     (wandbox-create-server "melpon" "https://wandbox.org")))
  "Available wandbox server list.")

(defcustom wandbox-default-server-name "melpon"
  "Wandbox server name to use by default."
  :group 'wandbox
  :type  'string)

(cl-defun wandbox-find-server (name &key (if-does-not-exist :skip))
  (or (cl-find name wandbox-servers :key #'wandbox-server-name :test #'string=)
      (cl-case if-does-not-exist
        (:error (error "Server %s not found" name))
        (:skip nil)
        (otherwise nil))))

(defun wandbox-default-server ()
  "Return the default wandbox server."
  (wandbox-find-server wandbox-default-server-name :if-does-not-exist :error))

;;;###autoload
(defun wandbox-add-server (name location)
  "Register wandbox server LOCATION as named NAME."
  (or (wandbox-find-server name)
      (add-to-list 'wandbox-servers (wandbox-create-server name location))))

(defsubst wandbox--merge-plist (&rest args)
  "Merge all the given ARGS into a new plist (destructive)."
  (let ((result (car args)))
    (dolist (plist (cdr args))
      (cl-loop for (key value) on plist by #'cddr
               do (plist-put result key value)))
    result))

(defsubst wandbox--pick (plist &rest keys)
  "Return a new PLIST, filterd to only have values KEYS."
  (let (obj)
    (dolist (key keys)
      (let ((val (plist-get plist key)))
        (when val
          (setq obj (plist-put obj key val)))))
    obj))

(defvar wandbox--verbose nil)

(defsubst wandbox--log (format &rest args)
  "Display log message."
  (cl-labels ((truncate (obj)
                (cl-typecase obj
                  (string (s-truncate 20 obj))
                  (list (if (listp (car obj))
                            ;; assoc
                            (cl-loop for (key . value) in obj
                                     collect (cons key (truncate value)))
                            obj))
                  (t obj))))
    (when wandbox--verbose
      (let ((msg (apply #'format format (mapcar #'truncate args))))
        (setq msg (s-replace "\n" "" msg)) ; one-line
        (message "Wandbox: %s" msg)))))

(defsubst wandbox--buffer-profile ()
  "Find embedded wandbox parameters in current buffer.

For example:

// hello.c
// #wandbox compiler: gcc-head
// #wandbox compiler-option: -lm
int main() { ... }

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

(defsubst wandbox--merge-profile (profile &rest functions)
  "Merge PROFILE by fold FUNCTIONS."
  (let ((p (copy-sequence profile)))
    (dolist (f functions p)
      (setq p (wandbox--merge-plist p (apply f p))))))

(defun wandbox-profiles (&optional server)
  "List available profiles."
  (append
   wandbox-user-profiles
   (wandbox-server-profiles (or server (wandbox-default-server)))))

(cl-defun wandbox-build-request-data-raw (&rest profile
                                          &key compiler options code stdin save
                                               compiler-option runtime-option
                                               server
                                           &allow-other-keys)
  (cl-labels ((join-as-string (list separator)
                (mapconcat #'(lambda (x) (format "%s" x)) list separator))
              (val  (x) (or x ""))
              (bool (x) (if x t :json-false))
              (raw  (x) (if (consp x) (join-as-string x "\n") (val x)))
              (compiler-exist-p (server name)
                (let ((compiler-names
                       (mapcar (lambda (x) (cdr (assoc "name" x)))
                               (wandbox-server-compilers server))))
                  (member name compiler-names))))
    (cl-check-type server wandbox-server)
    (unless (compiler-exist-p server compiler)
      (error "Unknown compiler: %s" compiler))
    `(("compiler" . ,(val compiler))
      ("options"  . ,(val options))
      ("code"     . ,(val code))
      ("stdin"    . ,(val stdin))
      ("compiler-option-raw" . ,(raw compiler-option))
      ("runtime-option-raw"  . ,(raw runtime-option))
      ("save" . ,(bool save)))))

(cl-defun wandbox-build-request-data (&rest profile
                                      &key (server (wandbox-default-server))
                                      &allow-other-keys)
  "Build JSON data to post to Wandox API.
PROFILE is property list. e.g. (:compiler COMPILER-NAME :options OPTS ...)"
  (let ((other-spec (wandbox--pick
                     profile ;; copy specified options
                     :compiler :options :stdin :save
                     :compiler-option :runtime-option)))
    ;; NOTE: Order to merge profile
    ;; 1. expand :file (:gist) to :code
    ;; 2. expand :name, :lang
    ;; 3. expand buffer-profile
    ;; 4. function args :compiler, :options, ...
    (setq profile (apply #'wandbox--merge-profile profile wandbox-precompiled-hook))
    (setq profile (wandbox--merge-profile
                   profile
                   ;; 1.
                   (cl-function
                    (lambda (&key file &allow-other-keys)
                      (when file
                        `(:code ,(wandbox-fetch file)
                          ,@(wandbox-find-profile :ext (file-name-extension file))))))
                   ;; 2.
                   (cl-function
                    (lambda (&key name lang &allow-other-keys)
                      (let ((profiles (wandbox-profiles)))
                        (cond
                         (name (or (wandbox-find-profile :name name profiles)
                                   (error "Not found :name %s" name)))
                         (lang (or (wandbox-find-profile :lang lang profiles)
                                   (error "Not found :lang %s" lang)))))))
                   ;; 3.
                   (cl-function
                    (lambda (&key code &allow-other-keys)
                      (when code (with-temp-buffer
                                   (insert code)
                                   (wandbox--buffer-profile)))))
                   ;; 4.
                   (cl-function
                    (lambda (&rest _)
                      (declare (ignore _))
                      other-spec))))
    (apply #'wandbox-build-request-data-raw :server server profile)))

(defsubst wandbox--setup-markdown-font-lock ()
  "Use markdown font lock in `wandbox-output-buffer' (in-progress)."
  (when (require 'markdown-mode nil t)
    (with-current-buffer wandbox-output-buffer
      (setq-local font-lock-defaults '(gfm-font-lock-keywords))
      (font-lock-mode t))))

(defsubst wandbox--ansi-color-apply-buffer (buffer)
  (with-current-buffer buffer
    (let (buffer-read-only)
      (ansi-color-apply-on-region (point-min) (point-max)))))

(defsubst wandbox--dump (request-response)
  "Print result from REQUEST-RESPONSE."
  (wandbox--setup-markdown-font-lock)
  (cl-labels ((println (&optional (fmt "") &rest args)
                (princ (apply #'format fmt args))
                (terpri)))
    (let* ((data (request-response-data request-response))
           (settings (request-response-settings request-response))
           (param (wandbox--json-decode (plist-get settings :data)))
           (compiler (cdr (assoc "compiler" param))))
      (println "## %s" compiler)
      (dolist (key wandbox-response-keywords)
        (let ((val (cdr (assoc key data))))
          (when val
            (println "* [%s]" key)
            (println "%s" val))))
      (println))))

(cl-defun wandbox-post (server profile &key sync (callback #'identity))
  "Send a compile request to wandbox SERVER.
JSON data for http post is build from PROFILE."
  (cl-labels ((onsuccess (&key data response &allow-other-keys)
                (wandbox--log "recieve: %S" data)
                (let ((url (cdr (assoc "url" data))))
                  (when (and url (functionp wandbox-permalink-action))
                    (funcall wandbox-permalink-action url)))
                (with-output-to-temp-buffer wandbox-output-buffer
                  (wandbox--dump response))
                (wandbox--ansi-color-apply-buffer wandbox-output-buffer)
                t)
              (onerror (&key error-thrown &allow-other-keys)
                (message "HTTP error: %S" error-thrown))
              (parser ()
                (decode-coding-region (point-min) (point-max) 'utf-8)
                (let ((json-key-type 'string))
                  (json-read))))
    (let* ((url (wandbox-server-api/compile server))
           (param (apply #'wandbox-build-request-data :server server profile))
           (data (json-encode param)))
      (wandbox--log "send request: %S" param)
      (setq data (encode-coding-string data 'raw-text))
      (funcall callback
               (request url
                        :type "POST"
                        :data data
                        :headers '(("Content-Type" . "application/json"))
                        :parser #'parser
                        :success (if sync #'ignore #'onsuccess)
                        :error #'onerror
                        :sync sync)))))

(defun wandbox-post* (server profile profiles)
  "Send multiple request to wandbox SERVER."
  (let* ((type (type-of profiles))
         (response* (cl-map type #'(lambda (p)
                                     (let ((param (wandbox--merge-plist profile p)))
                                       (wandbox-post server param :sync t)))
                            profiles)))
    (with-output-to-temp-buffer wandbox-output-buffer
      (cl-mapc #'wandbox--dump response*))
    (cl-map type #'request-response-data response*)))

;;;###autoload
(cl-defun wandbox-compile (&rest profile
                           &key
                           compiler options code stdin
                           compiler-option runtime-option
                           lang name file
                           (profiles [])
                           (save nil)
                           (sync nil)
                           (server-name wandbox-default-server-name)
                           &allow-other-keys
                           &aux (server (wandbox-find-server server-name :if-does-not-exist :error)))
  "Compile CODE as COMPILER's program code on Wandbox.

URL `https://wandbox.org' is online compiler service
for multi programming language (C/C++, Python, PHP, Common Lisp, etc).

Options:

* LANG is language name. (e.g. \"Ruby\")
* NAME is select compiler template from `wandbox-user-profiles' or default compiler settings.
* FILE is file contents instead of CODE.
* SYNC is synchronous mode. (debug)
* SERVER-NAME is server name. (default `wandbox-default-server-name')

List of available server values, see `wandbox-list-compilers'."
  (if (<= 1 (length profiles))
      (wandbox-post* server profile profiles)
      (wandbox-post server profile :sync sync :callback #'request-response-data)))

;; see also: http://developer.github.com/v3/gists/
(defun wandbox-fetch-gist (id)
  "Fetch a single gist from ID."
  (let ((url (format "https://api.github.com/gists/%d" id)))
    (wandbox--json-load url)))

(cl-defun wandbox-option-gist (&key gist gist-file &allow-other-keys)
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

(cl-defun wandbox-option-code (&key code-before code code-after &allow-other-keys)
  (let ((profile nil))
    (plist-put profile :code (concat code-before code code-after))))

(unless wandbox-precompiled-hook
  (add-to-list 'wandbox-precompiled-hook #'wandbox-option-gist t)
  (add-to-list 'wandbox-precompiled-hook #'wandbox-option-code t))

(cl-defun wandbox-find-profile (key item &optional
                                    (profiles (wandbox-profiles)))
  "Find the profile match KEY and ITEM."
  ;; (find item wandbox-user-profiles
  ;;       :key (lambda (x) (plist-get x key))
  ;;       :test #'string-equal)
  (when (stringp item)
    (cl-dolist (p profiles)
      (let ((val (plist-get p key)))
        (if (and val (string-equal (downcase val) (downcase item)))
            (cl-return (copy-sequence p)))))))

(cl-defun wandbox-read-profile (&optional (key :name))
  "Read a profile setting.
Completion list is generate from matchs KEY."
  (let* ((completion-ignore-case t)
         (items (mapcar (lambda (x) (plist-get x key))
                        (wandbox-profiles)))
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
  "Compile specified region (FROM TO)."
  (interactive "r")
  (let ((profile (or (wandbox--buffer-profile)
                     (and (buffer-file-name)
                          (wandbox-find-profile :ext (file-name-extension (buffer-file-name))))
                     (wandbox-read-profile)))
        (code (buffer-substring-no-properties from to)))
    (apply #'wandbox-compile :code code profile)))

;;;###autoload
(defun wandbox-compile-buffer ()
  "Compile current buffer."
  (interactive)
  (wandbox-compile-region (point-min) (point-max)))

;;;###autoload
(defun wandbox (&rest args)
  "Run wandbox."
  (interactive)
  (if (called-interactively-p 'any)
      (if (use-region-p)
          (call-interactively #'wandbox-compile-region)
          (call-interactively #'wandbox-compile-buffer))
    (apply #'wandbox-compile args)))

;;;###autoload
(cl-defmacro wandbox-eval-with ((&rest options) &body form)
  "Evaluate FORM as S-expression."
  (declare (indent 1))
  (let ((print-circle t))
    `(wandbox-compile :name "CLISP"
                      :code ,(prin1-to-string
                              `(let ((*print-circle* t))
                                 (format t "~{~s~^ ;~%~}"
                                         (multiple-value-list (progn ,@form)))))
                      ,@options)))

;;;###autoload
(defun wandbox-eval-last-sexp ()
  "Evaluate last S-expression at point."
  (interactive)
  (let ((form (buffer-substring (progn (backward-sexp) (point))
                                (progn (forward-sexp) (point)))))
    (wandbox-compile :name "CLISP"
                     :code (format
                            "(let ((*print-circle* t))
                               (format t \"~{~s~^ ;~%%~}\" (multiple-value-list %s)))"
                            form))))

(defun wandbox-tweet (url)
  "Post a text URL to twitter."
  (browse-url (concat "https://twitter.com/intent/tweet?text=Wandbox&url="
                      (url-hexify-string url))))

;; [Tweet This]
;; (setq wandbox-permalink-action #'wandbox-tweet)

;;;###autoload
(defun wandbox-select-server (name)
  "Switch to selected NAME wandbox server."
  (interactive (list (completing-read "Select server: "
                       (mapcar #' wandbox-server-name wandbox-servers))))
  (let ((s (wandbox-find-server name :if-does-not-exist :error)))
    (setq wandbox-default-server-name name)
    (message "Set server [%s] %s" name (wandbox-server-location s))))

;;;; Wandbox menu mode

(defvar wandbox-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'wandbox-menu-select)
    map)
  "Local keymap for `wandbox-menu-mode' buffers.")

(defun wandbox-menu-select ()
  "Show details about compiler information at point."
  (interactive)
  (let ((compiler (tabulated-list-get-id)))
    (with-output-to-temp-buffer "*Wandbox Compiler Info*"
      (let ((json-encoding-pretty-print t))
        (princ (json-encode compiler))))))

(define-derived-mode wandbox-menu-mode tabulated-list-mode "Wandbox Compilers"
  "Display available complers list.
\\<wandbox-menu-mode-map>
\\{wandbox-menu-mode-map}"
  (let ((server (wandbox-default-server)))
    (setq tabulated-list-format [("Language" 12 t)
                                 ("Name"     15 t)
                                 ("Compiler" 20)
                                 ("Version"  30)
                                 ("Command"  30)
                                 ("Option"   15)])
    (setq tabulated-list-padding 2)
    (setq tabulated-list-sort-key '("Language" . nil))
    (setq tabulated-list-entries
          (let ((compilers (wandbox-server-compilers server)))
            (mapcar (lambda (item)
                      (prog1
                          (list item (vector
                                      (cdr (assoc "language" item))
                                      (cdr (assoc "display-name" item))
                                      (cdr (assoc "name" item))
                                      (cdr (assoc "version" item))
                                      (cdr (assoc "display-compile-command" item))
                                      (wandbox--default-compiler-options
                                       (cdr (assoc "name" item)) compilers)))))
                    compilers)))
    (tabulated-list-init-header)
    (message "%s" (wandbox-server-location server))
    (hl-line-mode)))

;;;###autoload
(defun wandbox-list-compilers (&optional popup)
  "Display compilers list."
  (interactive "P")
  (with-current-buffer (get-buffer-create "*Wandbox Compilers*")
    (wandbox-menu-mode)
    (tabulated-list-print)
    (funcall (if popup 'pop-to-buffer 'switch-to-buffer) (current-buffer))))

(defsubst wandbox--template (name)
  (let* ((url-fmt (wandbox-server-api/template (wandbox-default-server)))
         (url (format url-fmt name)))
    (cdr (assoc "code" (wandbox--json-load url)))))

;;;###autoload
(defun wandbox-insert-template (name)
  "Insert template snippet named NAME."
  (interactive (list
                (completing-read "Insert template: "
                  (wandbox-server-templates (wandbox-default-server)) nil t)))
  (save-excursion
    (insert (or (wandbox--template name)
                (error "template `%s' not found" name)))))

;;;###autoload
(defun wandbox-customize (&optional other-window)
  "Customize wandbox variablis."
  (interactive "P")
  (customize-group 'wandbox other-window))

(provide 'wandbox)

;;; wandbox.el ends here
