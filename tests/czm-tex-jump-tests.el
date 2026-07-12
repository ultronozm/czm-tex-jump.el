;;; czm-tex-jump-tests.el --- Tests for czm-tex-jump  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'czm-tex-jump)

(ert-deftest czm-tex-jump-externaldocument-options ()
  "Recognize the second optional argument of \\externaldocument."
  (with-temp-buffer
    (insert "\\externaldocument[][nocite]{alpha}[alpha.pdf]")
    (should (equal (czm-tex-jump--external-documents)
                   '(("" "alpha"))))
    (goto-char (point-min))
    (should (re-search-forward (czm-tex-jump--regexp) nil t))
    (should (equal (match-string 3) "alpha"))))

(provide 'czm-tex-jump-tests)
;;; czm-tex-jump-tests.el ends here
