;;; test-wandbox.el

;;; TODO:
;; - integrate profiles (plist/keyword) and response data (alist/string)

;;; Code:

(require 'ert)
(require 'json)
(require 'wandbox)
(require 'cl-lib)

;; test assoc
(defun wandbox-test-alist-member (x y)
  (cl-check-type x list)
  (cl-check-type y list)
  (cl-loop for (key . value) in x
           always (equal value (cdr (assoc key y)))))

(defun wandbox-test-alist-equal (x y)
  (and (alist-member x y)
       (alist-member y x)))

;; test plist
(defun wandbox-test-plist-member (pitem plist)
  (cl-check-type pitem list)
  (cl-check-type plist list)
  (cl-loop for (key value) on pitem by #'cddr
           always (equal value (cl-getf plist key))))

(defun wandbox-test-plist-equal (x y)
  (and (wandbox-test-plist-member x y)
       (wandbox-test-plist-member y x)))

;; FIXME: `plist-member' is a built-in function
(defalias 'alist-member  'wandbox-test-alist-member)
(defalias 'alist-equal   'wandbox-test-alist-equal)
(defalias 'plist-member* 'wandbox-test-plist-member)
(defalias 'plist-equal   'wandbox-test-plist-equal)

(ert-deftest wandbox-test-alist-member ()
  "Test alist-member for test."
  (should (alist-member '()
                        '()))
  (should (alist-member '()
                        '((a . 10))))
  (should (alist-member '((a . 10))
                        '((a . 10))))
  (should (alist-member '(("a" . 10))
                        '(("a" . 10))))
  (should (alist-member '((a . 10))
                        '((a . 10) (b . 10))))
  (should (alist-member '((a . 10))
                        '((b . 10) (a . 10) (c . 30))))
  (should-not (alist-member '((a . 10))
                            '()))
  (should-not (alist-member '((a . 10) (b . 10))
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

(ert-deftest wandbox-test-plist-member ()
  "Test plist-member for test."
  (should (plist-member* '()
                         '()))
  (should (plist-member* '()
                         '(:a 1)))
  (should (plist-member* '(:a 1)
                         '(:a 1)))
  (should (plist-member* '(:a 1)
                         '(:a 1 :b 2)))
  (should-not (plist-member* '(:a 1)
                             '()))
  (should-not (plist-member* '(:a 1)
                             '(:a 2)))
  (should-not (plist-member* '(:a 1 :b 2)
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
  (should-error
   (wandbox-build-request-data :compiler "unknown"))
  t)

(ert-deftest wandbox-list-compilers ()
  (should (vectorp (wandbox-list-compilers))) ; json array
  t)

(ert-deftest wandbox-find-profile ()
  (should (plist-member* '(:lang "C++" :name "gcc HEAD" :compiler "gcc-head")
                         (wandbox-find-profile :compiler "gcc-head")))
  t)

(ert-deftest wandbox-build-request-data ()
  "Test request data."
  (should (alist-member '(("compiler" . "gcc-4.8.2-c")
                          ("options" . "warning,c11")
                          ("code" . "main(){}"))
                        (wandbox-build-request-data :lang "C"
                                                    :code "main(){}")))
  t)

(ert-deftest wandbox-test-buffer-profile ()
  "Test buffer-profile."
  (should (alist-member '(("compiler" . "clang-3.3-c")
                          ("compiler-option-raw" . "-lm"))
                        (wandbox-build-request-data :file "./test/sample.c")))
  (should (alist-member '(("compiler" . "gcc-head")
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

;;; test-wandbox.el ends here
