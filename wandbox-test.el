;;; test-wandbox.el

;;; TODO:
;; - integrate profiles (plist/keyword) and response data (alist/string)

;;; Code:

(require 'ert)
(require 'json)
(require 'wandbox)
(require 'cl-lib)

(defsubst test-wandbox-json-decode (data)
  (let ((json-object-type 'plist)
        (json-key-type 'keyword))
    (json-read-from-string data)))

(defsubst test-wandbox-plist-member (pitem plist)
  (cl-check-type pitem list)
  (cl-check-type plist list)
  (cl-loop for (key value) on pitem by #'cddr
           always (equal value (cl-getf plist key))))

(defsubst test-wandbox-plist-equal (x y)
  (and (test-wandbox-plist-member x y)
       (test-wandbox-plist-member y x)))

(ert-deftest plist-member ()
  "Test plist-member for test."
  (should (test-wandbox-plist-member '()
                                     '()))
  (should (test-wandbox-plist-member '()
                                     '(:a 1)))
  (should (test-wandbox-plist-member '(:a 1)
                                     '(:a 1)))
  (should (test-wandbox-plist-member '(:a 1)
                                     '(:a 1 :b 2)))
  (should-not (test-wandbox-plist-member '(:a 1)
                                         '()))
  (should-not (test-wandbox-plist-member '(:a 1)
                                         '(:a 2)))
  (should-not (test-wandbox-plist-member '(:a 1 :b 2)
                                         '(:a 1)))
  t)

(ert-deftest plist-equal ()
  "Test plist-equal for test."
  (should (test-wandbox-plist-equal '()
                                    '()))
  (should (test-wandbox-plist-equal '(:a 1 :b 2 :c 3)
                                    '(:a 1 :b 2 :c 3)))
  (should (test-wandbox-plist-equal '(:a 1 :b 2 :c 3)
                                    '(:c 3 :b 2 :a 1)))
  (should-not (test-wandbox-plist-equal '(:a 1)
                                        '(:a 2)))
  (should-not (test-wandbox-plist-equal '(:a 1 :b 2)
                                        '(:a 1)))
  t)

(ert-deftest wandbox:request-data ()
  "Test request data."
  (should (test-wandbox-plist-equal
           (test-wandbox-json-decode
            (wandbox-build-request-data
             :compiler "gcc-head"
             :options "warning"
             :code "main(){}"))
           '(:compiler "gcc-head"
             :options "warning"
             :code "main(){}"
             :runtime-option-raw ""
             :compiler-option-raw ""
             :stdin ""
             :save :json-false)))
  (should-error
   (wandbox-build-request-data :compiler "unknown"))
  t)

(ert-deftest wandbox:profiles ()
  "Test valid profile."
  (should (vectorp (wandbox-list-compilers))) ; json array
  (should (test-wandbox-plist-member
           '(:name "gcc HEAD" :compiler "gcc-head")
           (wandbox-find-profile :compiler "gcc-head")))
  t)

(ert-deftest wandbox:response-data ()
  "Test wandbox api response data"
  (should (equal
           (wandbox :compiler "gcc-head" :code "int main(){}" :sync t)
           '(("status" . "0"))))
  ;; test stdio (input stdin, output stderr)
  (should (equal
           (wandbox :lang "perl" :code "while (<>) { print STDERR uc($_); }" :stdin "hello" :sync t)
           '(("status" . "0")
             ("program_message" . "HELLO")
             ("program_error" . "HELLO"))))
  ;; test gist snippet
  (should (equal
           (wandbox :name "CLISP" :gist 219882 :stdin "Uryyb Jbeyq!" :sync t)
           '(("status" . "0")
             ("program_output" . "Hello World!\n")
             ("program_message" . "Hello World!\n"))))
  ;; test eval-with
  (should (equal
           (wandbox-eval-with (:sync t)
             (loop for i from 0 to 9 collect (* i i)))
           '(("status" . "0")
             ("program_output" . "(0 1 4 9 16 25 36 49 64 81)\n")
             ("program_message" . "(0 1 4 9 16 25 36 49 64 81)\n"))))
  t)

(ert-deftest buffer-profile ()
  "Test buffer-profile."
  (should (test-wandbox-plist-member '(:compiler "clang-3.3-c"
                                       :compiler-option-raw "-lm")
                                     (test-wandbox-json-decode
                                      (wandbox-build-request-data :file "./test/sample.c"))))
  t)

;;; test-wandbox.el ends here
