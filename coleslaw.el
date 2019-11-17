;;; coleslaw.el --- Coleslaw static content files. -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Spenser Truex
;; Author: Spenser Truex <web@spensertruex.com>
;; Created: 2019-06-16
;; Version: 0.2.5
;; Package-Requires: ((emacs "24.5"))
;; Keywords: lisp wp files convenience
;; URL: https://github.com/equwal/coleslaw/
;; Homepage: https://spensertruex.com/coleslaw-mode
;; This file is not part of GNU Emacs, but you want to use  GNU Emacs to run it.
;; This file is very free software.
;; License:
;; Licensed with the GNU GPL v3 see:
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Please add (coleslaw-setup) to your init file for the author's mode
;; selections. Bind `coleslaw-dispatch' to a key to make coleslaw discover the
;; mode.

;; For the coleslaw static content generator, a minor mode which inserts the
;; header, selects the major mode, and generally makes writing static content
;; easier.

;; Consider also installing the coleslaw-snippet package to generate your snippets.
;;; Code:

(require 'cl-lib)
(defvar coleslaw-mode-hook nil "Coleslaw-mode, for editing static web content.")
(defvar coleslaw-separator ";;;;;"
  "The string used between the coleslaw headers as in the example:
;;;;;
title: Example
format: cl-who
date: 2019-06-15
;;;;;
Where the separator is \";;;;;\".")
(defvar coleslaw-types '(".page" ".post")
  "Those file types which coleslaw will try to auto insert into.")

(defvar coleslaw-modes nil
  (concat
   "Modes based on the regex (special characters quoted)"
   (regexp-quote coleslaw-separator)
   "
  format: FORMAT
" (regexp-quote coleslaw-separator) "
  headers in the coleslaw file. A simple default choice is:
  (setq coleslaw-modes
        '((\"md\" . (markdown-mode))
          (\"cl-who\" . (lisp-mode))
          (\"html\" . (html-mode))
          (\"rst\" . (rst-mode))))
  in your init file."))

(add-hook 'coleslaw-mode-hook 'coleslaw-dispatch)
(defvar coleslaw-formats (list "md" "cl-who" "rst" "html" "org")
  "The format header values that coleslaw will allow to be auto-inserted.")

(defun coleslaw--valid-format (str)
  "Determine if the STR is permissible for a format: header in Coleslaw."
  (when (stringp str)
    (cl-some (lambda (x)
               (string-equal x str))
             coleslaw-formats)))
(defun coleslaw--url-charp (char)
  "Is the CHAR legal in a static URL according to RFC3986?
See Section 2."
  (cl-find char (concat "abcdefghijklmnopqrstuvwxyz"
                        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                        "0123456789-_~.%")))
(defun coleslaw--urlp (str)
  "Is the STR of valid URL characters?"
  (cl-every #'coleslaw--url-charp str))

(defun coleslaw--bufftypep ()
  "Confirm that the current buffer is a coleslaw type.
Uses `coleslaw-types'."
  (cl-some #'(lambda (type)
               (string= type (coleslaw--bufftype buffer-file-name)))
           coleslaw-types))
(defun coleslaw--bufftype (type)
  "Determine if the file type of the current buffer is TYPE.
Type should be something like \".page\"."
  (string= type
           (cl-subseq buffer-file-name
                      (cl-search "." buffer-file-name
                                 :from-end t))))

(defun coleslaw--insist (predicate prompt1 prompt2)
  "Insist based on a minibuffer response that is PREDICATE.
Use PROMPT1 the first time, then show PROMPT2 until correct."
  (let ((res (read-from-minibuffer (if prompt1
                                       (concat prompt1 " ")
                                     (concat prompt2 " ")))))
    (if (funcall predicate res)
        res
      (coleslaw--insist predicate nil prompt2))))
(defun coleslaw--field (predicate field-prompt &optional fail-prompt)
  "Insistently insert a field using PREDICATE to confirm.
FIELD-PROMPT is the first prompt. FAIL-PROMPT is the second. If
FAIL-PROMPT is nil then a reasonable one will be made up."
  (concat field-prompt " "
          (coleslaw--insist predicate field-prompt
                            (if fail-prompt
                                (and field-prompt
                                     (concat fail-prompt " "))
                              (concat "INVALID! "
                                      field-prompt " ")))
          "\n"))
(defmacro coleslaw--skeleton-when (condition &rest code)
  "Return the empty string on false CONDITION.
Otherwise the CODE is executed while concatenating."
    `(if ,condition
         (concat ,@code)
       ""))
;;;###autoload
(defun coleslaw-insert-header-conditionally ()
  "Insert the header, but only on files that match.
Uses `coleslaw-types' for matches. Good for hooking into
`find-file-hook'. Bind `coleslaw-insert-header' instead for
interactive use."
  (interactive)
  (when (coleslaw--bufftypep)
    (coleslaw-insert-header)))
;;;###autoload
(defun coleslaw-insert-header  ()
  "Insert the skeleton for as specified by default for a coleslaw file type."
  (interactive)
  (goto-char (point-min))
  (skeleton-insert
   '(nil coleslaw-separator "\n"
         (coleslaw--field #'identity "title:")
         (coleslaw--field #'coleslaw--valid-format
                          "format:"
                          "format be one of `coleslaw-formats':")
         (coleslaw--skeleton-when
          (y-or-n-p "Insert tags? ")
          (coleslaw--field #'(lambda (s)
                               (or (coleslaw--urlp s)
                                   (cl-some #'(lambda (s)
                                                ;; multiple tags
                                                (eql ?\  s))
                                            s)))
                           "tags:"
                           "Invalid character for a tag. tags:"))
         (coleslaw--skeleton-when (coleslaw--bufftype ".post")
          (coleslaw--field #'coleslaw--urlp "url: " "Bad URL:"))
         (coleslaw--skeleton-when
          (and (coleslaw--bufftype ".post")
               (y-or-n-p "Insert excerpt? "))
          (coleslaw--field #'identity "excerpt:"))
         "date: "
         (format-time-string "%Y-%m-%d" (current-time))
         "\n" coleslaw-separator "\n") 0)
  ;; Calls magic-mode-alist on the header.
  (set-auto-mode))

(defun coleslaw--mode-regex (mode)
  "Build a regex that match a format: header for MODE."
  (concat coleslaw-separator
          "\\(\n\\|.\\)+"
          "format:[\t ]"
          mode
          "\\(\n\\|.\\)+"
          coleslaw-separator))
;;;###autoload
(defun coleslaw-setup ()
  "Setup your coleslaw like the author suggests (conservative edits only).
strongly recommended!  Enable auto insertion for .page and .post
files, enable such basic editing modes as the mode function
`markdown-mode', the mode function `lisp-mode', the mode function
`html-mode', or the mode function `rst-mode' based on the format
header field.  Conservative additions only."
  (interactive)
  (setq coleslaw-modes
        '(("md" . markdown-mode)
          ("cl-who" . lisp-mode)
          ("html" . html-mode)
          ("rst" . rst-mode)))
  (cl-loop for mode in coleslaw-modes
           do (add-to-list 'magic-mode-alist
                           (cons (coleslaw-mode-regex (car mode))
                                 (cdr mode))))
  (add-hook 'find-file-hook
            'coleslaw-insert-header-conditionally))
(provide 'coleslaw)
;;; coleslaw.el ends here
