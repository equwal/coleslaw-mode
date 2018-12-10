-*- lexical-binding: t; -*-
;;; coleslaw-mode.el --- Coleslaw static content files.

;; Copyright (C) 2018 Spenser Truex
;; Author: Spenser Truex <spensertruexonline@example.com>
;; Created: Equwal 2018-12-09
;; Version:  1.1.0
;; Package-Requires: ((emacs "24"))
;; Keywords: lisp wp files convenience
;; URL: https://github.com/equwal/coleslaw-mode/
;; Homepage: http://truex.eu
;; This file is not part of GNU Emacs.
;; This file is free software.
;; License:
;; Licensed with the GNU GPL v3 see:
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;; coleslaw-insert-header (bound to M-;) inserts the comment block
;; depending on the type of file, and is how the major mode is selected for
;; the file.  Meant to work for all formats supported by kingcons's
;; "coleslaw" static content generator. Default enabled in .page and .post
;; files.

;;; Code:

(defvar coleslaw-mode-hook nil)
(defun coleslaw-bufftype (type)
  "Determine if the file type of the current buffer is TYPE."
  (string-equal type (subseq buffer-file-name (- (length buffer-file-name) 5))))
;;;###autoload
(defun coleslaw-insert-header (format)
  "Spawn a skeleton as specified by default for a coleslaw file type.
Automatically changes the mode.  FORMAT is filled into the
skeleton and used to select the mode"
  (interactive "Sformat: ")
  (beginning-of-buffer)
  (if (bufftype ".post")
      (progn (insert (concatenate
                      'string
                      ";;;;;\
                      format
                      "\
             (beginning-of-buffer)
             (forward-line)
             (move-end-of-line 1))
    (when (bufftype ".page")
      (insert (concatenate
               'string
               ";;;;;\
               format
               "\
      (beginning-of-buffer)
      (forward-line)
      (move-end-of-line)
      (cond ((string-equal (symbol-name format) "md")
             (markdown-mode))
            ((string-equal (symbol-name format) "cl-who")
             (lisp-mode))
            ((string-equal (symbol-name format) "html")
             (html-mode))
            ((string-equal (symbol-name format) "rst")
             (markdown-mode))))))
(defvar coleslaw-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-;") 'coleslaw-insert-header)
    map)
  "Keymap for COLESLAW major mode.")
;;;###autoload
(defun coleslaw-mode ()
  "Mode for editing coleslaw site generation files."
  (interactive)
  (kill-all-local-variables)
  (add-hook 'coleslaw-mode-hook 'flyspell-mode)
  (add-hook 'coleslaw-mode-hook 'markdown-live-preview-mode)
  (add-hook 'coleslaw-mode-hook 'markdown-mode)
  ;(add-hook 'coleslaw-mode-hook 'markdown-mode)
  (setq minor-mode 'coleslaw-mode)
  (setq mode-name "COLESLAW")
  (run-hooks 'coleslaw-mode-hook)
  (use-local-map coleslaw-mode-map))
(add-to-list 'auto-mode-alist '("\\.page\\'" . coleslaw-mode))
(add-to-list 'auto-mode-alist '("\\.post\\'" . coleslaw-mode))
(provide 'coleslaw-mode)
;; Should not to require these in case cl-who or otherwise is wanted, once it is implemented.
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files")
(autoload 'markdown-preview-eww "view markdown in w3m web browser.")

(provide 'coleslaw-mode)

;;; coleslaw-mode.el ends here