;;; wandbox.el --- Wandbox interface for Emacs

;; Author: KOBAYASHI Shigeru (kosh) <shigeru.kb@gmail.com>
;; URL: https://github.com/kosh04/emacs-wandbox
;; Version: 0.2.1
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
;; (wandbox-compile :compiler "gcc-head" :options "warning" :code "main(){}")
;; (wandbox-compile :name "C" :compiler-option "-lm" :file "/path/to/prog.c")
;; (wandbox-compile :name "Perl" :code "while (<>) { print uc($_); }" :stdin "hello")
;; (wandbox-compile :name "Ruby2.0" :code "p ARGV" :runtime-option '("1" "2" "3"))
;;
;; (add-to-list 'wandbox-profiles '(:name "User Profile" :compiler "clang-head"))
;; (wandbox-compile :name "User Profile" :code "...")

;;; Change Log:

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
    (:name "C++" :compiler "gcc-head" :options "warning,boost-1.55,sprout,gnu++1y" :ext "cc")
    (:name "CPP" :compiler "gcc-4.8.2-pp" :options "cpp-p" :ext "cpp")
    (:name "C" :compiler "gcc-4.8.2-c" :options "warning,c11" :ext "c")
    (:name "C#" :compiler "mcs" :ext "cs")
    (:name "D" :compiler "gdc-head" :ext "d")
    (:name "Haskell" :compiler "ghc" :ext "hs")
    (:name "Perl" :compiler "perl-5.18.0" :options "perl5.18.0" :ext "pl")
    (:name "Python2.7" :compiler "python-2.7.3" :ext "py")
    (:name "Python3.3" :compiler "python-3.3.2" :ext "py")
    (:name "Ruby1.9" :compiler "ruby-1.9.3-p0" :ext "rb")
    (:name "Ruby2.0" :compiler "ruby-2.0.0-p247" :ext "rb")
    (:name "MRuby" :compiler "mruby-head" :ext "rb")
    (:name "Erlang" :compiler "erlang-maint" :ext "erl")
    (:name "Rust" :compiler "rust-head" :ext "rs")
    (:name "Bash" :compiler "bash" :ext "sh")
    (:name "SQL" :compiler "sqlite-3.8.1" :ext "sql")
    (:name "Lua" :compiler "lua-5.2.2" :ext "lua")
    (:name "PHP" :compiler "php-5.5.6" :ext "php")
    (:name "Lazy K" :compiler "lazyk" :ext "lazy")
    (:name "CLISP" :compiler "clisp-2.49.0" :ext "lisp")
    (:name "Pascal" :compiler "fpc-2.6.2" :ext "pas")
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
    ))

(defun wandbox--fetch-url (url)
  (declare (special url-http-end-of-headers))
  (with-current-buffer (url-retrieve-synchronously url)
    (unwind-protect
        (buffer-substring (1+ url-http-end-of-headers) (point-max))
      (kill-buffer (current-buffer)))))

(defvar wandbox-compiler-alist nil)

(defun wandbox-compiler-alist ()
  (unless wandbox-compiler-alist
    (setq wandbox-compiler-alist
          (json-read-from-string
           (wandbox--fetch-url "http://melpon.org/wandbox/api/list.json"))))
  wandbox-compiler-alist)

(defun wandbox-compiler-list ()
  "Return the available compiler list."
  (mapcar (lambda (x) (cdr (assoc 'name x)))
          (wandbox-compiler-alist)))

(defun wandbox-default-compiler-options (compiler)
  "Return the COMPILER default option."
  (labels ((mapcan (fn list &rest more-list)
             (apply #'nconc (apply #'mapcar fn list more-list)))
           (join (list separator)
             (mapconcat #'identity list separator)))
    (join (mapcan (lambda (o)
                    (let ((x (or (cdr (assoc 'default o)) "")))
                      (cond ((stringp x) (list x))
                            ((eq x t) (list (cdr (assoc 'name o))))
                            (t nil))))
                  (catch 'switches
                    (dotimes (i (length (wandbox-compiler-alist)))
                      (let ((x (elt (wandbox-compiler-alist) i)))
                        (if (member `(name . ,compiler) x)
                            (throw 'switches (cdr (assoc 'switches x))))))))
          ",")))

(defun wandbox-build-request-data (profile)
  "Build JSON data to post to Wandox API.
PROFILE is property list. e.g. (:compiler COMPILER-NAME :options OPTS ...)"
  (labels ((join-as-string (list separator)
             (mapconcat #'(lambda (x) (format "%s" x)) list separator))
           (val (x)
             (or (plist-get profile x) ""))
           (raw (x)
             (let ((v (val x)))
               (if (consp v) (join-as-string v "\n") (or v "")))))
    (unless (member (val :compiler) (wandbox-compiler-list))
      (error "Unknown compiler: %s" (val :compiler)))
    (let ((alist `(("compiler" . ,(val :compiler))
                   ("options"  . ,(val :options))
                   ("code"     . ,(val :code))
                   ("stdin"    . ,(val :stdin))
                   ("compiler-option-raw" . ,(raw :compiler-option))
                   ("runtime-option-raw"  . ,(raw :runtime-option)))))
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
           (let* ((http-body (buffer-substring (1+ url-http-end-of-headers) (point-max)))
                  (alist (let ((json-key-type 'string))
                           (json-read-from-string
                            (decode-coding-string http-body 'utf-8-unix)))))
             (with-output-to-temp-buffer "*Wandbox Output*"
               (dolist (res wandbox-response-keywords)
                 (when (assoc res alist)
                   (princ (format "<<< %s >>>" (car (assoc res alist))))
                   (terpri)
                   (princ (cdr (assoc res alist)))
                   (terpri))))))
      (kill-buffer buf))))

(defun wandbox-post (json)
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data json))
    (url-retrieve "http://melpon.org/wandbox/api/compile.json"
                  'wandbox-format-url-buffer))
  t)

(defun* wandbox-compile (&rest profile
                         &key
                         compiler options code stdin
                         compiler-option runtime-option
                         name file
                         &allow-other-keys)
  "Compile CODE as COMPILER's source code.
If NAME specified, select compiler template from `wandbox-profiles'.
If FILE specified, compile FILE contents instead of code."
  (when name
    (setq profile (append profile (wandbox-find-profile :name name))))
  (when (and file (file-exists-p file))
    (setq profile (plist-put profile :code (with-temp-buffer
                                             (insert-file-contents file)
                                             (buffer-string)))))
  (wandbox-post (wandbox-build-request-data profile)))

;; (defalias 'wandbox 'wandbox-compile)

(defun wandbox-find-profile (key item)
  ;; (find item wandbox-profiles
  ;;       :key (lambda (x) (plist-get x key))
  ;;       :test #'string-equal)
  (dolist (x wandbox-profiles)
    (if (string-equal (downcase (plist-get x key))
                      (downcase item))
        (return x))))

(defun* wandbox-read-profile (&optional (key :name))
  (let* ((items (mapcar (lambda (x) (plist-get x key))
                        wandbox-profiles))
         (name (completing-read "Profile: " items)))
    (wandbox-find-profile :name name)))

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
  `(wandbox-compile :name "CLISP"
                    :code (prin1-to-string '(print (progn ,@form)))
                    ,@options))

(put 'wandbox-eval-with 'lisp-indent-function 2)

(provide 'wandbox)

;;; wandbox.el ends here
