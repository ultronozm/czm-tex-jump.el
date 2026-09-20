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

(ert-deftest czm-tex-jump-included-document ()
  "Follow nested inputs from the master and a narrowed child buffer."
  (let* ((dir (make-temp-file "czm-tex-jump-" t))
         (master (expand-file-name "main.tex" dir))
         (child (expand-file-name "child.tex" dir))
         (nested (expand-file-name "nested.tex" dir))
         (reftex-save-parse-info nil)
         (reftex-enable-partial-scans nil)
         (enable-local-variables nil)
         buffers)
    (unwind-protect
        (save-window-excursion
          (dolist (entry `((,master . "\\documentclass{article}\n\\begin{document}\n\\input{child}\n\\end{document}\n")
                           (,child . "See \\ref{target}.\n\\input{nested}\n")
                           (,nested . "Heading.\n\\label{target}\n")))
            (with-temp-file (car entry) (insert (cdr entry))))
          (setq buffers (mapcar #'find-file-noselect (list master child nested)))
          (dolist (buffer buffers)
            (with-current-buffer buffer
              (setq-local TeX-master master)))
          (switch-to-buffer (car buffers))
          (czm-tex-jump-ref "target")
          (should (equal buffer-file-name nested))
          (should (looking-at (regexp-quote "\\label{target}")))
          ;; Search live contents, even after RefTeX has cached its scan.
          (replace-match "\\label{edited}" t t)
          (switch-to-buffer (cadr buffers))
          (let ((indirect (clone-indirect-buffer " *czm-input-test*" nil)))
            (push indirect buffers)
            (switch-to-buffer indirect)
            (narrow-to-region (point-min) (line-end-position))
            (let ((start (point)))
              (should (equal (czm-tex-find-definition "edited")
                             (list nested 2)))
              (should (= start (point)))
              (should (buffer-narrowed-p))
              (czm-tex-jump-ref "edited")
              (should (equal buffer-file-name nested))
              (should (looking-at (regexp-quote "\\label{edited}"))))))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest czm-tex-jump-cite-from-included-file ()
  "Find the master's bibliography from a narrowed indirect child buffer."
  (let* ((dir (make-temp-file "czm-tex-cite-" t))
         (master (expand-file-name "main.tex" dir))
         (child (expand-file-name "chapters/child.tex" dir))
         (bib (expand-file-name "refs.bib" dir))
         (reftex-save-parse-info nil)
         (enable-local-variables nil)
         buffers)
    (unwind-protect
        (save-window-excursion
          (make-directory (file-name-directory child))
          (dolist (entry `((,master . "\\documentclass{article}\n\\begin{document}\n\\input{chapters/child}\n\\bibliography{refs}\n\\end{document}\n")
                           (,child . "Some text.\nSee \\cite{target}.\nMore text.\n")
                           (,bib . "% Bibliography\n@article{target,\n}\n")))
            (with-temp-file (car entry) (insert (cdr entry))))
          (setq buffers (mapcar #'find-file-noselect (list master child bib)))
          (with-current-buffer (car buffers) (setq-local TeX-master t))
          (switch-to-buffer (cadr buffers))
          (setq-local TeX-master "../main")
          (let ((indirect (clone-indirect-buffer " *czm-cite-child*" nil)))
            (push indirect buffers)
            (switch-to-buffer indirect)
            (goto-char (point-min))
            (forward-line 1)
            (narrow-to-region (point) (line-end-position))
            (let ((start (point))
                  (source-window (selected-window)))
              (czm-tex-jump-cite "target")
              (should (equal buffer-file-name bib))
              (should (looking-at "@article{target,"))
              (should (= (window-point source-window) start))
              (with-current-buffer indirect
                (should (= (point) start))
                (should (buffer-narrowed-p))))))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory dir t))))

(defmacro czm-tex-jump-test--with-document (files &rest body)
  "Create FILES in a temporary directory and evaluate BODY in main.tex.
FILES is an alist of relative names and contents.  Child files use
main.tex as their master.  Clean up visiting buffers, including edits."
  (declare (indent 1) (debug t))
  `(let* ((default-directory (make-temp-file "czm-document-" t))
          (master (expand-file-name "main.tex"))
          (reftex-save-parse-info nil)
          (reftex-enable-partial-scans nil)
          (enable-local-variables nil))
     (unwind-protect
         (save-window-excursion
           (dolist (entry ,files)
             (make-directory (file-name-directory
                              (expand-file-name (car entry))) t)
             (with-temp-file (car entry) (insert (cdr entry))))
           (dolist (entry ,files)
             (with-current-buffer (find-file-noselect (car entry))
               (setq-local TeX-master master)))
           (switch-to-buffer (find-file-noselect "main.tex"))
           ,@body)
       (dolist (buffer (buffer-list))
         (when (and (buffer-file-name buffer)
                    (file-in-directory-p (buffer-file-name buffer)
                                         default-directory))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory default-directory t))))

(ert-deftest czm-tex-jump-refreshes-stale-document-files ()
  "A jump finds an input added after RefTeX cached the document."
  (czm-tex-jump-test--with-document
      '(("main.tex" . "\\documentclass{article}\n\\begin{document}\n\\input{child}\n\\end{document}\n")
        ("child.tex" . "See \\ref{new-target}.\n")
        ("new.tex" . "\\label{new-target}\n"))
    (reftex-access-scan-info)
    (goto-char (point-min))
    (search-forward "\\input{child}")
    (insert "\n\\input{new}")
    (switch-to-buffer (find-file-noselect "child.tex"))
    (let ((source (current-buffer)) (start (point)))
      (czm-tex-jump-ref "new-target")
      (should (equal (file-name-nondirectory buffer-file-name) "new.tex"))
      (should (looking-at (regexp-quote "\\label{new-target}")))
      (switch-to-buffer source)
      (czm-tex-jump-ref "missing")
      (should (eq (current-buffer) source))
      (should (= (point) start)))))

(ert-deftest czm-tex-jump-external-reference-keeps-cache ()
  "An external reference does not invalidate an existing document scan."
  (czm-tex-jump-test--with-document
      '(("main.tex" . "\\documentclass{article}\n\\externaldocument[X-]{other}\n\\begin{document}\n\\end{document}\n")
        ("other.tex" . "\\label{target}\n"))
    (reftex-access-scan-info)
    (let* ((symbol reftex-docstruct-symbol)
           (scan (symbol-value symbol)))
      (czm-tex-jump-ref "X-target")
      (should (equal (file-name-nondirectory buffer-file-name) "other.tex"))
      (should (looking-at (regexp-quote "\\label{target}")))
      (should (eq scan (symbol-value symbol))))))

(ert-deftest czm-tex-jump-cite-refreshes-bibliography ()
  "Citation jumps recover from changed and newly added declarations."
  (dolist (initial '("" "\\bibliography{old}\n"))
    (czm-tex-jump-test--with-document
        `(("main.tex" . ,(concat "\\documentclass{article}\n\\begin{document}\n"
                                  "\\input{chapters/child}\n" initial
                                  "\\end{document}\n"))
          ("chapters/child.tex" . "See \\cite{target}.\n")
          ("old.bib" . "@article{old,\n}\n")
          ("new.bib" . "@article{target,\n}\n"))
      (reftex-access-scan-info)
      (goto-char (point-min))
      (if (search-forward "\\bibliography{old}" nil t)
          (replace-match "\\bibliography{new}" t t)
        (search-forward "\\end{document}")
        (beginning-of-line)
        (insert "\\bibliography{new}\n"))
      (switch-to-buffer (find-file-noselect "chapters/child.tex"))
      (czm-tex-jump-cite "target")
      (should (equal (file-name-nondirectory buffer-file-name) "new.bib"))
      (should (looking-at "@article{target,")))))

(ert-deftest czm-tex-jump-cite-child-declaration-path ()
  "Bibliography paths in a child are relative to the master directory."
  (czm-tex-jump-test--with-document
      '(("main.tex" . "\\documentclass{article}\n\\begin{document}\n\\input{chapters/child}\n\\end{document}\n")
        ("chapters/child.tex" . "See \\cite{target}.\n\\bibliography{refs}\n")
        ("refs.bib" . "@article{target,\n}\n"))
    (switch-to-buffer (find-file-noselect "chapters/child.tex"))
    (czm-tex-jump-cite "target")
    (should (equal (file-name-nondirectory buffer-file-name) "refs.bib"))
    (should (looking-at "@article{target,"))))

(provide 'czm-tex-jump-tests)
;;; czm-tex-jump-tests.el ends here
