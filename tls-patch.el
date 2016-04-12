;;; tls-patch.el --- monkey patch for TLS  -*- lexical-binding: t -*-

(require 'cl-lib)
(or (require 'nadvice nil t)
    (require 'advice))

(defun tls-patch--use-tls-program (f &rest args)
  "Force use external `tls-program' (not builtin GnuTLS)."
  (cl-letf (((symbol-function 'gnutls-available-p)
             (lambda () nil)))
    (apply f args)))

(cond
 ((featurep 'nadvice)
  (advice-add 'network-stream-open-tls :around 'tls-patch--use-tls-program))
 (t
  (message "fallback advice")
  (defadvice network-stream-open-tls (around tls-patch--use-tls-program activate)
    (cl-letf (((symbol-function 'gnutls-available-p)
               (lambda () nil)))
      ad-do-it))))

(provide 'tls-patch)

;;; tls-batch.el ends here.
