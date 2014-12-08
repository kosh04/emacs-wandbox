;;; test-wandbox.el

;;; TODO:
;; - integrate profiles (plist/keyword) and response data (alist/string)

;;; Code:

(require 'ert)
(require 'json)
(require 'wandbox)
(require 'cl-lib)

(defconst wandbox-test-api-keys
  '(:compiler
    :options
    :code
    :runtime-option-raw
    :compiler-option-raw
    :stdin
    :save))

(defsubst test-wandbox-json-decode (data)
  (let ((json-object-type 'plist)
        (json-key-type 'keyword))
    (json-read-from-string data)))

(defsubst test-wandbox-plist-equal (x y)
  (cl-every (lambda (key)
              (let (($x (plist-get x key))
                    ($y (plist-get y key)))
                (and $x $y (equal $x $y))))
            wandbox-test-api-keys))

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
  (should (string= (plist-get (wandbox-find-profile :compiler "gcc-head") :compiler)
                   "gcc-head"))
  t)

(ert-deftest wandbox:response-data ()
  "Test wandbox api response data"
  (should (equal
           (wandbox :compiler "gcc-head" :code "int main(){}" :sync t)
           '(("status" . "0"))))
  (should (equal
           (wandbox :lang "perl" :code "while (<>) { print uc($_); }" :stdin "hello" :sync t)
           '(("status" . "0")
             ("program_output" . "HELLO")
             ("program_message" . "HELLO"))))
  (should (equal
           (wandbox :name "CLISP" :gist 219882 :stdin "Uryyb Jbeyq!" :sync t)
           '(("status" . "0")
             ("program_output" . "Hello World!\n")
             ("program_message" . "Hello World!\n"))))
  (should (equal
           (wandbox-eval-with (:sync t)
             (loop for i from 0 to 9 collect (* i i)))
           '(("status" . "0")
             ("program_output" . "\n(0 1 4 9 16 25 36 49 64 81) \n")
             ("program_message" . "\n(0 1 4 9 16 25 36 49 64 81) \n"))))
  t)

;;; test-wandbox.el ends here
