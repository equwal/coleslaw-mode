;;; coleslaw.el --- Coleslaw static content files. -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Spenser Truex
;; Author: Spenser Truex <web@spensertruex.com>
;; Created: 2019-06-16
;; Version: 0.3.0
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

;; For the coleslaw static content generator, a minor mode which inserts the
;; header, selects the major mode, and generally makes writing static content
;; easier.

;;; Code:

(require 'cl-lib)

(defmacro coleslaw--custom-default (var default doc &rest defcustom-keys)
  "Put default values for VAR as DEFAULT.
Use the same syntax as a normal `defcustom' DOC and
DEFCUSTOM-KEYS are used for it."
  (let ((def (gensym)))
    `(let ((,def ,default))
       (progn (unless (boundp ',var) (setq ,var ,def))
              (defcustom ,var ,def ,doc ,@defcustom-keys)))))

(coleslaw--custom-default coleslaw-modes
      '(("md" . (markdown-mode))
        ("cl-who" . (lisp-mode))
        ("html" . (html-mode sgml-mode))
        ("rst" . (rst-mode)))
      "Format types and modes from the 'format: ' header.
Each mode in the mode list is tried in order. If none match, then
the mode will not be changed from the `normal-mode' (probably
`fundamental-mode'). Coleslaw will NOT PERMIT a `format:' header
which is not in this list, so you may choose to use `(\"plain\" .
nil)' or similar for formats where no mode change is needed."
      :tag "Format headers and types alist"
      :type '(alist :key-type string
                    :value-type (list symbol))
      :group 'coleslaw
      :local t
      :package-version "0.2.6")
(coleslaw--custom-default coleslaw-types
  '(".page" ".post")
  "Those file types which coleslaw will try to auto insert into."
  :tag "File type extensions (.page, etc.) to use"
  :type '(list string)
  :local t
  :package-version "0.2.6"
  :group 'coleslaw)

(coleslaw--custom-default coleslaw-separator ";;;;;"
  "The string used between the coleslaw headers as in the example:
;;;;;
title: Example
format: cl-who
date: 2019-06-15
;;;;;
Where the separator is \";;;;;\"."
  :tag "The separator for the header"
  :type 'string
  :group 'coleslaw
  :local t
  :package-version "0.2.6")

(defun coleslaw--valid-format (str)
  "Determine if the STR is permissible for a format: header in Coleslaw."
  (when (stringp str)
    (cl-some (lambda (x)
               (string-equal x str))
             (cl-mapcar #'car coleslaw-modes))))
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
               (string= type (coleslaw--bufftype)))
           coleslaw-types))
(defun coleslaw--bufftype ()
  "Return the file type of the current buffer.
Should be something starting with a dot like \".page\"."
  (cl-subseq buffer-file-name
             (cl-search "." buffer-file-name
                        :from-end t)))

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

(defun coleslaw-insert-header ()
  "Insert the coleslaw header in the current buffer.
use (setq auto-insert t)"
  (interactive)
  (let ((prev-prompt (when (boundp 'auto-insert-prompt)
                       auto-insert-prompt)))
    (when (boundp 'auto-insert-prompt)
      (setq auto-insert-prompt
            "Perform Coleslaw header insertion [y/n]? "))
    (goto-char (point-min))
    (skeleton-insert
     '(nil coleslaw-separator "\n"
           (coleslaw--field #'identity "title:")
           (coleslaw--field #'coleslaw--valid-format
                            "format:"
                            "format must be in `coleslaw-modes':")
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
           (coleslaw--skeleton-when (string= ".post" (coleslaw--bufftype))
                                    (coleslaw--field #'coleslaw--urlp
                                                     "url: "
                                                     "Bad URL:"))
           (coleslaw--skeleton-when
            (and (string= ".post" (coleslaw--bufftype))
                 (y-or-n-p "Insert excerpt? "))
            (coleslaw--field #'identity "excerpt:"))
           "date: "
           (format-time-string "%Y-%m-%d" (current-time))
           "\n" coleslaw-separator "\n") 0)
    ;; Calls magic-mode-alist on the header.
    (set-auto-mode)
    (when (boundp 'auto-insert-prompt)
      (setq auto-insert-prompt prev-prompt))))

(defun coleslaw-insert-header-conditionally ()
  "Insert the header, but only on files that match.
Uses `coleslaw-types' for matches. Good for hooking into
`find-file-hook'. Bind `coleslaw-insert-header' instead for
interactive use."
  (interactive)
  (display-buffer-same-window (current-buffer) nil)
  (when (and (coleslaw--bufftypep)
             (= (buffer-size (current-buffer)) 0))
    ;; KLUDGE: Sometimes the buffer might not be showing the file before
    ;; coleslaw-insert-header is called. We can ensure that is happens by
    ;; attempting to display the buffer before some code elsewhere does.
    (coleslaw-insert-header)))

(defun coleslaw--mode-regex (mode)
  "Build a regex that match a format: header for MODE."
  (concat coleslaw-separator
          "\\(\n\\|.\\)+"
          "format:[\t ]"
          mode
          "\\(\n\\|.\\)+"
          coleslaw-separator))
(defun coleslaw-select-first-mode (modes)
  "The first defined mode function in MODES.
If there isn't one then NIL."
  (when modes
    ;; NOTE: Trying to return `normal-mode' in the else place can lead to
    ;; infinite recursion; use NIL.
    (or (and (boundp (car modes))
             (car modes))
        (coleslaw-select-first-mode (cdr modes)))))

(cl-loop for mode in coleslaw-modes
         do (add-to-list 'magic-mode-alist
                         (cons (coleslaw--mode-regex (car mode))
                               (coleslaw-select-first-mode (cdr mode)))))
(add-hook 'find-file-hook
          'coleslaw-insert-header-conditionally)

(provide 'coleslaw)

;;; coleslaw.el ends here
