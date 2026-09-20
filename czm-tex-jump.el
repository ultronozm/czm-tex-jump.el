;;; czm-tex-jump.el --- Jump to references in a TeX buffer using avy  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-tex-jump.el
;; Package-Requires: ((emacs "29.1") (avy "0.5.0") (embark "1.0") (czm-tex-util "0.0") (tex-parens "0.1"))
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

;; Follow references in a TeX buffer using avy.  This is similar to
;; `reftex-view-crossref', but useful in situations where reftex
;; doesn't work (e.g., indirect buffers and org latex src blocks), and
;; comes with the convenience of avy.
;; 
;; To install, bind `czm-tex-jump' to a key in LaTeX mode.  When you
;; press the key, you will be prompted to select a reference to
;; follow.  The default behavior is to jump to the reference in the
;; current buffer.  If you provide a prefix argument via C-u, then you
;; can use `embark' to perform additional actions (TODO: explain
;; more).

;;; Code:

(require 'seq)
(require 'avy)
(require 'outline)
(require 'xref)
(require 'reftex)
(require 'czm-tex-util)
(require 'embark)
(require 'tex-parens)

(defcustom czm-tex-jump-spec-alist
  '(("eqref" . czm-tex-jump-ref)
    ("ref" . czm-tex-jump-ref)
    ("cite" . czm-tex-jump-cite)
    ("href" . czm-tex-jump-href)
    ("url" . czm-tex-jump-url)
    ("externaldocument" . czm-tex-jump-externaldocument)
    ("externalcitedocument" . czm-tex-jump-externaldocument))
  "Alist of TeX reference commands and functions to follow them."
  :type '(alist :key-type string :value-type function)
  :group 'czm-tex-jump)

(defun czm-tex-jump-externaldocument (file-name)
  "Visit the LaTeX source of the external document FILE-NAME."
  (find-file (concat file-name ".tex")))

(defun czm-tex-jump--commands ()
  (mapcar #'car czm-tex-jump-spec-alist))

(defun czm-tex-jump--regexp ()
  (eval `(rx (seq  "\\"
                   (group
                    (or ,@(czm-tex-jump--commands)))
                   (group (* (seq "[" (*? nonl) "]")))
                   "{"
                   (group (one-or-more (not (any "}"))))
                   "}"))))

(defun czm-tex-jump-coolview-region (start end)
  (interactive "r")
  (deactivate-mark)
  (let* ((new-buff (clone-indirect-buffer nil nil))
         (below-window (split-window-below))
         (above-window (selected-window)))
    (run-with-timer 0.1 nil
                    (lambda ()
                      (save-selected-window
                        (select-window above-window)
                        (shrink-window-if-larger-than-buffer))))
    (switch-to-buffer new-buff)
    (narrow-to-region start end)
    below-window))

(defun czm-tex-jump-avy (&optional arg)
  "Avy command for jumping to TeX references in the current buffer."
  (interactive "P")
  (let* ((regexp (concat "\\([^z-a]" (czm-tex-jump--regexp) "\\)")))
    (avy-with avy-goto-char
      (let ((current-prefix-arg nil))
        (when (avy-jump regexp :group 1)
          (forward-char 1)
          (if arg
              (embark-act)
            (embark-dwim)))))))

(defun czm-tex-jump (arg)
  "Jump to the label corresponding to the reference selected via `avy'.
With a \\[universal-argument] prefix ARG, copy the reference to the kill
ring and yank it.  With numerical prefix ARG, move point to the end of
the reference, set the mark at the beginning, and activate the mark.

With no prefix argument, jump to the reference.  Use the current buffer
unless the reference is in another document file (in which case use a
buffer visiting said document) or outside the current restriction (in
which case use a new indirect buffer).

Push mark at previous position."
  (interactive "P")
  (let* ((start (point))
	        (commands (mapcar #'car czm-tex-jump-spec-alist))
         (regexp (eval `(rx (seq (group anychar "\\"
                                        (group
                                         (or ,@commands)))
                                 (group (* (seq "[" (*? nonl) "]")))
                                 "{"
                                 (group (one-or-more (not (any "}"))))
                                 "}")))))
    (avy-with avy-goto-line
      (avy-jump regexp :group 1))
    (if (re-search-forward regexp nil t)
	       (let ((ref (substring (match-string 0)
                              1))
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
            (push-mark start)
	           (funcall (cdr (assoc type (reverse czm-tex-jump-spec-alist)))
		                   ref-name))))
      (message "No reference found."))))

(defun czm-tex-jump--embark-target ()
  "Target the TeX reference at point."
  (let* ((commands (czm-tex-jump--commands))
         (pattern (rx-to-string
                   `(seq (group "\\"
                                (group
                                 (or ,@commands)))
                         (group (* (seq "[" (*? nonl) "]")))
                         "{"
                         (group (one-or-more (not (any "}"))))
                         "}")))
         start end)
    (save-excursion
      (when (thing-at-point-looking-at pattern)
        (setq start (match-beginning 0)
              end (match-end 0))
        `(tex-ref ,(buffer-substring-no-properties start end) ,start . ,end)))))

(defun czm-tex-jump-setup ()
  (add-to-list 'embark-target-finders 'czm-tex-jump--embark-target)
  (add-to-list 'embark-keymap-alist '(tex-ref . czm-tex-jump-embark-map)))

(defun czm-tex-jump-coolview ()
  (interactive)
  (let* ((start (point)))
    (if (re-search-forward (czm-tex-jump--regexp) nil t)
        (let ((type (match-string 1))
	             (ref-name (match-string 3)))
	         (save-restriction
            (widen)
	           (funcall (cdr (assoc type (reverse czm-tex-jump-spec-alist)))
		                   ref-name)
            (let* ((beg
                    (save-excursion
                      (tex-parens-backward-up-list)
                      (point)))
                   (end
                    (save-excursion
                      (tex-parens-up-list)
                      (point)))
                   (below-window (czm-tex-jump-coolview-region beg end)))
              (save-excursion
                (select-window below-window)
                (goto-char start)))))
      (message "No reference found."))))

(defun czm-tex-jump-view-crossref ()
  (interactive)
  (save-excursion
    (search-forward "{")
    (reftex-view-crossref)))

(defvar-keymap czm-tex-jump-embark-map
  :doc "Keymap for actions on tex references."
  :parent embark-general-map
  "RET" 'czm-tex-jump-goto
  "v" 'czm-tex-jump-view-crossref
  "c" 'czm-tex-jump-coolview)

(defun czm-tex-jump-goto (_str)
  "Jump to the tex reference at point."
  ;; Assumption: the reference name is enclosed by {...}
  (let* ((commands (czm-tex-jump--commands))
         (pattern (rx-to-string
                   `(seq (group "\\"
                                (group
                                 (or ,@commands)))
                         (group (* (seq "[" (*? nonl) "]")))
                         "{"
                         (group (one-or-more (not (any "}"))))
                         "}"))))
    (if (thing-at-point-looking-at pattern)
        (let ((type (match-string 2))
	             (ref-name (match-string 4)))
	         (funcall (cdr (assoc type (reverse czm-tex-jump-spec-alist)))
		                 ref-name))
      (message "No reference found."))))

;; (defun czm-tex-jump-goto (_str)
;;   (czm-tex-find-definition (czm-tex-identifier-at-point)))

(defun czm-tex-jump--external-documents ()
  "Return the \\externaldocument declarations of the current buffer.
The result is a list of (PREFIX FILENAME) lists, in buffer order, where
PREFIX is nil for unprefixed declarations."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (declarations)
        (while (re-search-forward
                "\\\\external\\(?:cite\\)?document\\(?:\\[\\([^]]*\\)\\]\\)?\\(?:\\[nocite\\]\\)?{\\([^}]+\\)}"
                nil t)
          (push (list (match-string 1) (match-string 2)) declarations))
        (nreverse declarations)))))

(defun czm-tex-jump--search (regexp)
  "Return the first position matching REGEXP in the current restriction."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward regexp nil t)
      (match-beginning 0))))

(defun czm-tex-jump--search-files (regexp files)
  "Return (BUFFER . POSITION) for REGEXP in FILES, using live contents."
  (seq-some
   (lambda (file)
     (with-current-buffer (or (find-buffer-visiting file)
                              (find-file-noselect file))
       (save-restriction
         (widen)
         (when-let* ((pos (czm-tex-jump--search regexp)))
           (cons (current-buffer) pos)))))
   files))

(defun czm-tex-jump--document-lookup (lookup)
  "Call LOOKUP with RefTeX scan information, refreshing once on a miss.
Non-file buffers use LOOKUP directly, without RefTeX."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (if (not buffer-file-name)
            (funcall lookup)
          ;; Attach to shared or saved scan information before deciding
          ;; whether a failed lookup needs a refresh.
          (reftex-access-scan-info -1)
          (let ((cached (and reftex-docstruct-symbol
                             (symbol-value reftex-docstruct-symbol))))
            (reftex-access-scan-info)
            (or (funcall lookup)
                (when cached
                  (reftex-access-scan-info t)
                  (funcall lookup)))))))))

(defun czm-tex-jump--external-label (name)
  "Return (BUFFER . POSITION) for NAME in an external document.
Strip xr prefixes and try later declarations first, as xr does."
  (seq-some
   (lambda (declaration)
     (let ((prefix (car declaration))
           (file (concat (cadr declaration) ".tex")))
       (when (or (null prefix) (string-prefix-p prefix name))
         (czm-tex-jump--search-files
          (format "\\\\label{%s}"
                  (regexp-quote (if prefix (substring name (length prefix)) name)))
          (list file)))))
   (reverse (czm-tex-jump--external-documents))))

(defun czm-tex-jump--document-label (name)
  "Return (BUFFER . POSITION) for NAME in included or external files."
  (czm-tex-jump--document-lookup
   (lambda ()
     (or (when buffer-file-name
           (czm-tex-jump--search-files
            (format "\\\\label{%s}" (regexp-quote name))
            (seq-filter #'stringp (reftex-all-document-files))))
         (czm-tex-jump--external-label name)))))

(defun czm-tex-jump-ref (ref-name)
  "Follow reference REF-NAME in the current TeX document.
Search the current buffer, files included by the master, and files listed in
\\externaldocument{...} commands."
  (let* ((regexp (format "\\\\label{%s}" (regexp-quote ref-name)))
         (local (czm-tex-jump--search regexp))
         (wide (or local (save-restriction
                           (widen)
                           (czm-tex-jump--search regexp))))
         (target (if wide (cons (current-buffer) wide)
                   (czm-tex-jump--document-label ref-name))))
    (if (not target)
        (message "Label not found: %s" ref-name)
      (unless (eq (car target) (current-buffer))
        (switch-to-buffer-other-window (car target)))
      (unless (<= (point-min) (cdr target) (point-max))
        (clone-indirect-buffer-other-window
         (generate-new-buffer-name (buffer-name)) t)
        (widen))
      (goto-char (cdr target))
      (recenter)
      (when outline-minor-mode
        (if local
            (condition-case nil (outline-show-entry) (error nil))
          (outline-show-entry))))))

(defun czm-tex-jump--bibliography-files ()
  "Return bibliography files, resolving TeX paths from the master.
Non-file buffers retain their local bibliography lookup."
  (if buffer-file-name
      ;; Unlike `reftex-get-bibfile-list', an absent declaration is a
      ;; lookup miss, so the caller can refresh stale scan information.
      (when (assq 'bib (symbol-value reftex-docstruct-symbol))
        (reftex-get-bibfile-list))
    (czm-tex-util-get-bib-files)))

(defun czm-tex-jump-cite (cite-name)
  "Follow citation CITE-NAME in the current buffer.
Searches in bib files declared locally or in the master TeX document.
Preserve point in the source buffer when visiting a bibliography."
  (let ((local-pos
         (save-restriction
           (widen)
           (czm-tex-jump--search
            (format "\\\\bibitem\\(\\[[^]]*\\]\\)?{\\(%s\\)}"
                    (regexp-quote cite-name))))))
    (if local-pos
        (save-restriction
          (widen)
          (goto-char local-pos)
          (recenter)
          (when outline-minor-mode
            (outline-show-entry)))
      (condition-case err
          (let ((target
                 (czm-tex-jump--document-lookup
                  (lambda ()
                    (czm-tex-jump--search-files
                     (format "@[^{]+{\\(%s\\)," (regexp-quote cite-name))
                     (czm-tex-jump--bibliography-files))))))
            (if target
                (progn
                  (switch-to-buffer-other-window (car target))
                  (unless (<= (point-min) (cdr target) (point-max))
                    (clone-indirect-buffer-other-window
                     (generate-new-buffer-name (buffer-name)) t)
                    (widen))
                  (goto-char (cdr target))
                  (recenter)
                  (when outline-minor-mode
                    (outline-show-entry)))
              (message "Citation not found: %s" cite-name)))
        (error (message "Error message: %s" (error-message-string err)))))))

(defun czm-tex-jump-href (href-name)
  "Follow href HREF-NAME.
This just calls `find-file'."
  (find-file href-name))

(defun czm-tex-jump-url (url-name)
  "Follow url URL-NAME.
This just calls `browse-url'."
  (browse-url url-name))


;;; xref stuff

(defun czm-tex-xref-backend ()
  "LaTeX xref backend."
  'czm-tex)

(defun czm-tex-identifier-at-point ()
  "Identify the LaTeX reference at point."
  ;; Adapted from czm-tex-jump-goto
  (let* ((commands '("eqref" "ref" "cite" "href" "url"))
         (pattern (rx-to-string
                   `(seq (group "\\" (group (or ,@commands)))
                         (opt (group "[" (*? nonl) "]"))
                         "{"
                         (group (one-or-more (not (any "}"))))
                         "}"))))
    (if (thing-at-point-looking-at pattern)
        (match-string 4))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql czm-tex)))
  (czm-tex-identifier-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql czm-tex)) identifier)
  (let ((location (czm-tex-find-definition identifier)))
    (when location
      (list (xref-make identifier
                       (xref-make-file-location
                        (cl-first location)
                        (cl-second location)
                        0))))))

(cl-defun czm-tex-find-definition (identifier)
  "Find the definition of IDENTIFIER in the current buffer or linked files."
  (let ((ref-location nil)
        (ref-type (thing-at-point-looking-at (regexp-opt '("\\ref" "\\cite" "\\href" "\\url")))))
    (cond
     ((and ref-type (string= ref-type "\\cite"))
      (czm-tex-find-definition-in-citations identifier))
     ((and ref-type (string= ref-type "\\href"))
      (list (czm-tex-find-definition-in-href identifier)))
     (t
      (setq ref-location (czm-tex-find-definition-in-current-buffer identifier))
      (or ref-location
          (when-let* ((target (czm-tex-jump--document-label identifier)))
            (with-current-buffer (car target)
              (save-restriction
                (widen)
                (list buffer-file-name (line-number-at-pos (cdr target)))))))))))

(defun czm-tex-find-definition-in-current-buffer (identifier)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "\\\\label{%s}" (regexp-quote identifier)) nil t)
      (list (buffer-file-name) (line-number-at-pos (match-beginning 0))))))

(defun czm-tex-find-definition-in-external-docs (identifier)
  "Find IDENTIFIER's defining label in external documents."
  (when-let* ((target (czm-tex-jump--external-label identifier)))
    (with-current-buffer (car target)
      (save-restriction
        (widen)
        (list buffer-file-name (line-number-at-pos (cdr target)))))))

(defun czm-tex-find-definition-in-citations (identifier)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "\\\\bibitem{\\(%s\\)}" (regexp-quote identifier)) nil t)
      (list (buffer-file-name) (line-number-at-pos (match-beginning 0))))))

(defun czm-tex-find-definition-in-href (identifier)
  (list identifier))

(cl-defmethod xref-backend-references ((_backend (eql czm-tex)) _name)
  ;; Similar to definitions, but you'd need to look in reverse.
  ;; This is left as an exercise.
  (message "References not implemented."))

(add-hook 'TeX-mode-hook
          (lambda ()
            (add-hook 'xref-backend-functions #'czm-tex-xref-backend nil t)))

(provide 'czm-tex-jump)
;;; czm-tex-jump.el ends here
