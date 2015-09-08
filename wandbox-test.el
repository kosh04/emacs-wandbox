;;; test-wandbox.el

;;; TODO:
;; - integrate profiles (plist/keyword) and response data (alist/string)

;;; Code:

(require 'ert)
(require 'json)
(require 'wandbox)
(require 'cl-lib)

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

(ert-deftest wandbox-merge-plist ()
  "Test merge-plist."
  (should (plist-equal (wandbox-merge-plist)
                       nil))
  (should (plist-equal (wandbox-merge-plist '(:a 1))
                       '(:a 1)))
  (should (plist-equal (wandbox-merge-plist '(:a 1 :b 2)
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

(ert-deftest wandbox-build-request-data ()
  "Test request data."
  (should (alist-equal
           (wandbox-build-request-data
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
   (wandbox-build-request-data :compiler "unknown"))
  t)

(ert-deftest wandbox-compilers ()
  (should (vectorp (wandbox-compilers))) ; json array
  t)

(ert-deftest wandbox-find-profile ()
  (should (plist-subsetp '(:lang "C++"
                           :name "gcc HEAD"
                           :compiler "gcc-head")
                         (wandbox-find-profile :compiler "gcc-head")))
  t)

(ert-deftest wandbox-build-request-data ()
  "Test request data."
  (should (alist-subsetp '(("compiler" . "gcc-4.8.2-c")
                           ("options" . "warning,c11")
                           ("code" . "main(){}"))
                         (wandbox-build-request-data :lang "C"
                                                     :code "main(){}")))
  t)

(ert-deftest wandbox-test-buffer-profile ()
  "Test buffer-profile."
  (should (alist-subsetp '(("compiler" . "clang-3.3-c")
                           ("compiler-option-raw" . "-lm"))
                         (wandbox-build-request-data :file "./test/sample.c")))
  (should (alist-subsetp '(("compiler" . "gcc-head")
                           ("compiler-option-raw" . "-lm"))
                         (wandbox-build-request-data :file "./test/sample.c"
                                                     :compiler "gcc-head")))
  t)

(ert-deftest wandbox-compile ()
  "Test compile."
  (should (alist-equal
           '(("status" . "0"))
           (wandbox :compiler "gcc-head" :code "int main(){}" :sync t)))
  ;; test stdio (input stdin, output stderr)
  (should (alist-equal
           '(("status" . "0")
             ("program_message" . "HELLO")
             ("program_error" . "HELLO"))
           (wandbox :lang "perl" :code "while (<>) { print STDERR uc($_); }" :stdin "hello" :sync t)))
  ;; multiple compile
  (should (cl-every #'(lambda (result)
                        (alist-equal '(("status" . "0")) result))
                    (wandbox :profiles [(:name "gcc HEAD")
                                        (:name "gcc")
                                        (:name "clang HEAD")
                                        (:name "clang")]
                             :code "int main(){ return 0; }")))
  ;; test gist snippet
  (should (alist-equal
           '(("status" . "0")
             ("program_output" . "Hello World!\n")
             ("program_message" . "Hello World!\n"))
           (wandbox :name "CLISP" :gist 219882 :stdin "Uryyb Jbeyq!" :sync t)))
  ;; test eval-with
  (should (alist-equal
           '(("status" . "0")
             ("program_output" . "(0 1 4 9 16 25 36 49 64 81)\n")
             ("program_message" . "(0 1 4 9 16 25 36 49 64 81)\n"))
           (wandbox-eval-with (:sync t)
             (loop for i from 0 to 9 collect (* i i)))))
  t)

(ert-deftest wandbox-list-compilers ()
  (should (wandbox-list-compilers)))

;;; test-wandbox.el ends here
