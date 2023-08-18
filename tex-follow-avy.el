;;; tex-follow-avy.el --- Follow references in a tex buffer using avy  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/tex-follow-avy.el
;; Package-Requires: ((emacs "25.1") (avy "0.5.0") (czm-tex-util "0.0"))
;; Keywords: tex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Follow references in a tex buffer using avy.  This is similar to
;; `reftex-view-crossref', but useful in situations where reftex
;; doesn't work (e.g., indirect buffers and org latex src blocks), and
;; comes with the convenience of avy.
;; 
;; To install, bind `tex-follow-avy' to a key in LaTeX mode.
;; When you press the key, you will be prompted to select a reference
;; to follow.  The default behavior is to jump to the reference in the
;; current buffer.  If you provide a prefix argument via C-u, then the
;; reference will be copied to the kill ring and yanked.  If you use
;; C-u C-u (or C-N for a number N), then the reference will be marked
;; for further action.
;;
;; My use-package declaration:
;; 
;; (use-package tex-follow-avy
;;   :vc (:url "https://github.com/ultronozm/tex-follow-avy.el.git"
;; 	    :rev :newest)
;;   :after latex avy
;;   :bind
;;   (:map LaTeX-mode-map
;; 	("s-r" . tex-follow-avy)))

;;; Code:

(require 'avy)
(require 'outline)
(require 'czm-tex-util)

(defcustom tex-follow-avy-spec-alist
  '(("eqref" . tex-follow-avy-ref)
    ("ref" . tex-follow-avy-ref)
    ("cite" . tex-follow-avy-cite)
    ("href" . tex-follow-avy-href))
  "Alist of TeX reference commands and functions to follow them."
  :type '(alist :key-type string :value-type function)
  :group 'tex-follow-avy)

;; TODO: modify so that it aborts properly after C-g

;;;###autoload
(defun tex-follow-avy (arg)
  "Follow a reference in the current buffer.
When prefix arg ARG is a list, copy reference to the kill ring
and yank it.  When ARG is a number, mark the reference."
  (interactive "P")
  (let* ((start (point))
	 (commands (mapcar #'car tex-follow-avy-spec-alist))
	 (regexp (format "\\(.\\\\\\(%s\\)\\)\\(\\[.*?\\]\\)?{\\([^}]+\\)}"
			 (regexp-opt commands))))
    (avy-with avy-goto-line
      (avy-jump regexp :group 1))
    (if (re-search-forward regexp nil t)
	(let ((ref (substring (match-string 0) 1))
	      (type (match-string 2))
	      (ref-name (match-string 4)))
	  (cond
	   ((and arg (listp arg))
	    (kill-new ref)
	    (goto-char start)
	    (yank))
	   ((and arg (numberp arg))
	    (push-mark (1+ (match-beginning 0)))
	    (goto-char (match-end 0))
	    (activate-mark))
	   (t
	    (funcall (cdr (assoc type (reverse tex-follow-avy-spec-alist)))
		     ref-name))))
      (message "No reference found."))))

(defun tex-follow-avy-ref (ref-name)
  "Follow reference REF-NAME in the current buffer.
Searches in the current buffer and in tex files listed in
\\externaldocument{...} commands."
  (interactive)
  (cl-flet ((search-for-label (name)
	      (save-excursion
		(goto-char (point-min))
		(when (re-search-forward
		       (format "\\\\label{%s}" (regexp-quote name)) nil t)
                  (match-beginning 0)))))
    (let (label-pos buf)
      (cond
       ;; Search current buffer, with narrowing restriction.
       ((setq label-pos
	      (search-for-label ref-name))
	(goto-char label-pos)
	(recenter)
	(when outline-minor-mode
	  (outline-show-entry)))
       ;; Search current buffer, without narrowing restriction.
       ((save-restriction
	  (widen)
	  (setq label-pos (search-for-label ref-name)))
	(clone-indirect-buffer-other-window
	 (generate-new-buffer-name (buffer-name)) t)
	(widen)
	(goto-char label-pos)
        (recenter)
	(when outline-minor-mode
	  (outline-show-entry)))
       ;; Search external documents.
       ((save-restriction
	  (widen)
	  (save-excursion
	    (goto-char (point-min))
	    (while
		(and
		 (null label-pos)
		 (re-search-forward "\\\\externaldocument{\\([^}]+\\)}" nil t))
	      (let*
		  ((filename (concat (match-string 1) ".tex"))
		   (already-open (find-buffer-visiting filename)))
		(setq buf (or already-open
			      (find-file-noselect filename)))
		(setq label-pos (with-current-buffer
				    buf
				  (save-restriction
				    (widen)
				    (search-for-label ref-name))))
		;; (unless already-open
		;;   (kill-buffer buf))
		))
	    label-pos))
	(switch-to-buffer-other-window buf)
	(if (and (>= label-pos (point-min))
		 (<= label-pos (point-max)))
	    (goto-char label-pos)
	  (clone-indirect-buffer-other-window
	   (generate-new-buffer-name (buffer-name)) t)
	  (widen)
	  (goto-char label-pos))
	(recenter)
	(when outline-minor-mode
	  (outline-show-entry)))
       (t
	(message "Label not found: %s" ref-name))))))

(cl-defun tex-follow-avy-cite (cite-name)
  "Follow citation CITE-NAME in the current buffer.
Searches in bib files listed in \\bibliography{...} commands."
  ;; function is a bit silly.  Why not just use reftex-view-crossref?
  ;; Maybe you'll later want to update this to work in non-file
  ;; buffers, with a "master" bib file?
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (re-search-forward (format "\\\\bibitem{\\(%s\\)}" (regexp-quote cite-name)) nil t)
          (progn
            (goto-char (match-beginning 0))
            (recenter)
            (when outline-minor-mode
              (outline-show-entry)))
        (condition-case err
            (let ((bibfiles (czm-tex-util-get-bib-files)))
              (dolist (bibfile bibfiles)
                (find-file-other-window bibfile)
                (goto-char (point-min))
                (when (re-search-forward (format "@[^{]+{\\(%s\\)," cite-name) nil t)
                  (goto-char (match-beginning 0))
                  (recenter)
                  (when outline-minor-mode
                    (outline-show-entry))
                  (cl-return)))
              (message "Citation not found: %s" cite-name))
          (error (format "Error message: %s\n" (error-message-string err))))))))

(defun tex-follow-avy-href (href-name)
  "Follow href HREF-NAME.
This just calls `find-file'."
  (interactive)
  (find-file href-name))

(provide 'tex-follow-avy)
;;; tex-follow-avy.el ends here
