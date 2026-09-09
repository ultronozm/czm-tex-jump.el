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

(ert-deftest czm-tex-jump-cite-preserves-source-point ()
  "A bibliography jump preserves the source point and selects the entry."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "Some text before the citation.\nSee \\cite{target}.\n")
      (let ((source (current-buffer))
            (source-window (selected-window))
            (start (point))
            (bib (generate-new-buffer " *czm-bib-test*")))
        (unwind-protect
            (progn
              (with-current-buffer bib
                (insert "% Bibliography\n@article{target,\n}\n"))
              (cl-letf (((symbol-function 'czm-tex-util-get-bib-files)
                         (lambda () '("test.bib")))
                        ((symbol-function 'find-file-noselect)
                         (lambda (&rest _) bib))
                        ((symbol-function 'find-file-other-window)
                         (lambda (&rest _) (switch-to-buffer-other-window bib))))
                (czm-tex-jump-cite "target"))
              (should (eq (current-buffer) bib))
              (should (looking-at "@article{target,"))
              (should (= (window-point source-window) start))
              (should (= (with-current-buffer source (point)) start)))
          (kill-buffer bib))))))

(ert-deftest czm-tex-jump-cite-missing-preserves-point ()
  "An unsuccessful citation lookup leaves point in the source buffer."
  (with-temp-buffer
    (insert "Some text.\nSee \\cite{missing}.\n")
    (let ((start (point)))
      (cl-letf (((symbol-function 'czm-tex-util-get-bib-files)
                 (lambda () nil)))
        (czm-tex-jump-cite "missing"))
      (should (= (point) start)))))

(ert-deftest czm-tex-jump-cite-local-bibitem ()
  "A local citation still moves point to its bibitem."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "\\bibitem{target} Entry.\nSee \\cite{target}.\n")
      (czm-tex-jump-cite "target")
      (should (= (point) (point-min))))))

(ert-deftest czm-tex-jump-external-ref-preserves-source-point ()
  "An external label jump preserves point in the source window."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "\\externaldocument{other}\nSome text.\nSee \\ref{target}.\n")
      (let ((source-window (selected-window))
            (start (point))
            (external (generate-new-buffer " *czm-external-test*")))
        (unwind-protect
            (progn
              (with-current-buffer external
                (insert "Some text.\n\\label{target}\n"))
              (cl-letf (((symbol-function 'find-buffer-visiting)
                         (lambda (&rest _) external)))
                (czm-tex-jump-ref "target"))
              (should (eq (current-buffer) external))
              (should (looking-at (regexp-quote "\\label{target}")))
              (should (= (window-point source-window) start)))
          (kill-buffer external))))))

(ert-deftest czm-tex-jump-cite-missing-in-bib-preserves-source ()
  "A missing bibliography entry leaves the source window selected."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "Some text.\nSee \\cite{missing}.\n")
      (let ((source (current-buffer))
            (start (point))
            (bib (generate-new-buffer " *czm-bib-test*")))
        (unwind-protect
            (progn
              (with-current-buffer bib
                (insert "@article{other,\n}\n"))
              (cl-letf (((symbol-function 'czm-tex-util-get-bib-files)
                         (lambda () '("test.bib")))
                        ((symbol-function 'find-file-noselect)
                         (lambda (&rest _) bib)))
                (czm-tex-jump-cite "missing"))
              (should (eq (window-buffer (selected-window)) source))
              (should (eq (current-buffer) source))
              (should (= (point) start)))
          (kill-buffer bib))))))

(provide 'czm-tex-jump-tests)
;;; czm-tex-jump-tests.el ends here
