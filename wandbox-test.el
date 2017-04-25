;;; test-wandbox.el

;;; TODO:
;; - integrate profiles (plist/keyword) and response data (alist/string)

;;; Code:

(require 'ert)
(require 'json)
(require 'wandbox)
(require 'cl-lib)

(when (getenv "TRAVIS")
  (load "tls-patch")
  (require 'request)
  (setq request-backend 'url-retrieve))

;; test assoc
(defun wandbox-test-alist-subsetp (aitem alist)
  (cl-check-type aitem list)
  (cl-check-type alist list)
  (cl-loop for (key . value) in aitem
           always (equal value (cdr (assoc key alist)))))

(defun wandbox-test-alist-equal (x y)
  (and (wandbox-test-alist-subsetp x y)
       (wandbox-test-alist-subsetp y x)))

;; test plist
(defun wandbox-test-plist-subsetp (pitem plist)
  (cl-check-type pitem list)
  (cl-check-type plist list)
  (cl-loop for (key value) on pitem by #'cddr
           always (equal value (cl-getf plist key))))

(defun wandbox-test-plist-equal (x y)
  (and (wandbox-test-plist-subsetp x y)
       (wandbox-test-plist-subsetp y x)))

(defalias 'alist-subsetp 'wandbox-test-alist-subsetp)
(defalias 'alist-equal   'wandbox-test-alist-equal)
(defalias 'plist-subsetp 'wandbox-test-plist-subsetp)
(defalias 'plist-equal   'wandbox-test-plist-equal)

(ert-deftest wandbox-test-alist-subsetp ()
  "Test alist-subsetp for test."
  (should (alist-subsetp '()
                         '()))
  (should (alist-subsetp '()
                         '((a . 10))))
  (should (alist-subsetp '((a . 10))
                         '((a . 10))))
  (should (alist-subsetp '(("a" . 10))
                         '(("a" . 10))))
  (should (alist-subsetp '((a . 10))
                         '((a . 10) (b . 10))))
  (should (alist-subsetp '((a . 10))
                         '((b . 10) (a . 10) (c . 30))))
  (should-not (alist-subsetp '((a . 10))
                             '()))
  (should-not (alist-subsetp '((a . 10) (b . 10))
                             '((a . 10))))
  t)

(ert-deftest wandbox-test-alist-equal ()
  "Test alist-equal for test."
  (should (alist-equal '() '()))
  (should (alist-equal '((b . 20) (c . 30) (a . 10))
                       '((a . 10) (b . 20) (c . 30))))
  (should-not (alist-equal '((a . 10) (b . 20) (c . 30))
                           '((a . 10) (b . 20))))
  t)

(ert-deftest wandbox-test-plist-subsetp ()
  "Test plist-subsetp for test."
  (should (plist-subsetp '()
                         '()))
  (should (plist-subsetp '()
                         '(:a 1)))
  (should (plist-subsetp '(:a 1)
                         '(:a 1)))
  (should (plist-subsetp '(:a 1)
                         '(:a 1 :b 2)))
  (should-not (plist-subsetp '(:a 1)
                             '()))
  (should-not (plist-subsetp '(:a 1)
                             '(:a 2)))
  (should-not (plist-subsetp '(:a 1 :b 2)
                             '(:a 1)))
  t)

(ert-deftest wandbox-test-plist-equal ()
  "Test plist-equal for test."
  (should (plist-equal '()
                       '()))
  (should (plist-equal '(:a 1 :b 2 :c 3)
                       '(:a 1 :b 2 :c 3)))
  (should (plist-equal '(:a 1 :b 2 :c 3)
                       '(:c 3 :b 2 :a 1)))
  (should-not (plist-equal '(:a 1)
                           '(:a 2)))
  (should-not (plist-equal '(:a 1 :b 2)
                           '(:a 1)))
  t)

(ert-deftest wandbox--merge-plist ()
  "Test merge-plist."
  (should (plist-equal (wandbox--merge-plist)
                       nil))
  (should (plist-equal (wandbox--merge-plist '(:a 1))
                       '(:a 1)))
  (should (plist-equal (wandbox--merge-plist '(:a 1 :b 2)
                                             '(:a 9)
                                             '(:c 3))
                       '(:a 9 :b 2 :c 3)))
  t)

(ert-deftest wandbox--pick ()
  "Test plist pick util."
  (should (plist-equal (wandbox--pick '())
                       nil))
  (should (plist-equal (wandbox--pick '(:a 1 :b 2 :c 3 :d 4))
                       nil))
  (should (plist-equal (wandbox--pick '(:a 1 :b 2 :c 3 :d 4) :a :b :z)
                       '(:a 1 :b 2)))
  t)

(ert-deftest wandbox-server ()
  "Test wandbox-server struct."
  (let ((s (wandbox-default-server)))
    (should (wandbox-server-p s))
    (should (string= (wandbox-server-name s) "melpon"))
    (should (vectorp (wandbox-server-compilers s)))
    (should (listp (wandbox-server-profiles s))))
  t)

(ert-deftest wandbox-servers ()
  (should (cl-every #'wandbox-server-p wandbox-servers)))

(ert-deftest wandbox-build-request-data ()
  "Test request data."
  (should (alist-equal
           (wandbox-build-request-data
            :server (wandbox-default-server)
            :compiler "gcc-head"
            :options "warning"
            :code "main(){}")
           '(("compiler" . "gcc-head")
             ("options" . "warning")
             ("code" . "main(){}")
             ("stdin" . "")
             ("compiler-option-raw" . "")
             ("runtime-option-raw" . "")
             ("save" . :json-false))))
  (should (alist-subsetp '(("compiler" . "ruby-head"))
                         (wandbox-build-request-data :name "ruby HEAD")))
  (should (alist-subsetp '(("compiler" . "mruby-head"))
                         (wandbox-build-request-data :name "mruby HEAD")))
  (should-error
   (wandbox-build-request-data :compiler "unknown")))

(ert-deftest wandbox-user-profile ()
  "Test user setting profile."
  (should "TODO"))

(ert-deftest wandbox-find-profile ()
  (should (plist-subsetp '(:lang "C++"
                           :name "gcc HEAD"
                           :compiler "gcc-head")
                         (wandbox-find-profile :compiler "gcc-head"))))

(ert-deftest wandbox-build-request-data ()
  "Test request data."
  (should (alist-subsetp '(("compiler" . "gcc-head-c")
                           ("options" . "warning,gnu11,cpp-no-pedantic")
                           ("code" . "main(){}"))
                         (wandbox-build-request-data
                          :lang "C"
                          :code "main(){}"
                          :server (wandbox-default-server)))))

(ert-deftest wandbox-test-buffer-profile ()
  "Test buffer-profile."
  (let ((s (wandbox-default-server)))
    (should (alist-subsetp '(("compiler" . "clang-3.3-c")
                             ("compiler-option-raw" . "-lm"))
                           (wandbox-build-request-data :file "./test/sample.c" :server s)))
    (should (alist-subsetp '(("compiler" . "gcc-head")
                             ("compiler-option-raw" . "-lm"))
                           (wandbox-build-request-data :file "./test/sample.c"
                                                       :compiler "gcc-head"
                                                       :server s)))))

(ert-deftest wandbox-compile ()
  "Test compile."
  (should (alist-equal
           '(("status" . "0"))
           (wandbox :compiler "gcc-head" :code "int main(){}" :sync t)))
  ;; test stdio (input stdin, output stderr)
  (should (alist-equal
           '(("status" . "0")
             ("program_message" . "HELLO\nWORLD")
             ("program_error" . "HELLO\nWORLD"))
           (wandbox :lang "perl" :code "while (<>) { print STDERR uc($_); }" :stdin "hello\nworld" :sync t))))

(ert-deftest wandbox-eval-with ()
  "Test eval with common-lisp."
  (should (alist-equal
           '(("status" . "0")
             ("program_output" . "(0 1 4 9 16 25 36 49 64 81)\n")
             ("program_message" . "(0 1 4 9 16 25 36 49 64 81)\n"))
           (wandbox-eval-with (:sync t)
             (loop for i from 0 to 9 collect (* i i))))))

(ert-deftest wandbox-compile-from-gist ()
  "Test gist snippet."
  (should (alist-equal
           '(("status" . "0")
             ("program_output" . "Hello World!\n")
             ("program_message" . "Hello World!\n"))
           (wandbox :name "CLISP" :gist 219882 :stdin "Uryyb Jbeyq!" :sync t))))

(ert-deftest wandbox-user-profile ()
  "Test user defined profile."
  (let ((wandbox-profiles nil)
        (f (lambda ()
             (wandbox :name "gcc-latest" :code "main(){ return 0; }" :sync t))))
    (should-error (funcall f))
    (add-to-list 'wandbox-profiles '(:name "gcc-latest" :compiler "gcc-head"))
    (should (alist-equal '(("status" . "0"))
                         (funcall f)))))

(ert-deftest wandbox-compile-multiple ()
  "Test multiple compile."
  (should (cl-every #'(lambda (result)
                        (alist-equal '(("status" . "0")) result))
                    (wandbox :profiles [(:name "gcc HEAD")
                                        (:name "gcc")
                                        (:name "clang HEAD")
                                        (:name "clang")]
                             :code "int main(){ return 0; }"))))

(ert-deftest wandbox-charset ()
  "Test character set."
  (should (alist-equal
           '(("status" . "0")
             ("program_output"  . "(LATIN_CAPITAL_LETTER_G LATIN_CAPITAL_LETTER_J U90E8)\n")
             ("program_message" . "(LATIN_CAPITAL_LETTER_G LATIN_CAPITAL_LETTER_J U90E8)\n"))
           (wandbox-eval-with (:sync t)
             (loop for c across "GJ\u90e8" collect (intern (char-name c)))))))

(ert-deftest wandbox-other-server ()
  "Test other server."
  (wandbox-add-server "fetus" "https://wandbox.fetus.jp")
  (should (wandbox-server-p (wandbox-find-server "fetus")))
  (should (alist-equal (wandbox :compiler "php-7.0.0"
                                :code "<?php echo phpversion();"
                                :server-name "fetus"
                                :sync t)
                       '(("program_message" . "7.0.0")
                         ("program_output" . "7.0.0")
                         ("status" . "0")))))

(ert-deftest wandbox--command-to-ext ()
  (cl-loop for (cmd . ext)
           in '(("gcc prog.c" . "c")
                ("mcs -out:prog.exe prog.cs" . "cs")
                ("rillc -o prog.exe prog.rill" . "rill")
                ("ocamlfind ocamlopt -thread -linkpkg prog.ml -o prog" . "ml")
                ;;("ponyc ./prog" . ?)
                )
           do (should (equal ext (wandbox--command-to-ext cmd)))))


;;; test-wandbox.el ends here
