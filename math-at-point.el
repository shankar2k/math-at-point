;;; math-at-point.el --- Compute math at point using calc-eval  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Shankar Rao

;; Author: Shankar Rao <shankar.rao@gmail.com>
;; URL: https://github.com/~shankar2k/math-at-point
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: calc, matching, latex

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provide the function math-at-point that evaluates the
;; arithmetic expression at point using calc-eval, displays the result in the
;; minibuffer, copies the result into the kill ring, and can optionally be
;; inserted into the buffer after the expression.
;;
;; See documentation on https://github.com/shankar2k/math-at-point.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; History:

;; Version 0.2 (2021-05-08):

;; - Added function `math-at-point-latex' for calculating LaTeX expressions at
;;   point
;; - Autodetect LaTeX expressions using org-inside-LaTeX-fragment-p in
;;   ``math-at-point''
;; - Allow whitespace before or after "=" sign when insert is enabled
;; - Allow insert when there is "=" sign without a result after it

;; Version 0.1 (2021-05-06):

;; - Initial version

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'calc)
(require 'org)

;;;; Variables

(rx-define map-unsigned-number
  (or (and (one-or-more digit)
           (optional (and "." (zero-or-more digit))))
      (and "." (one-or-more digit))))

(defvar map-simple-math-regexp
  (rx (and (optional (in "-" "+"))       ; optional sign in front of expression
           (zero-or-more (and map-unsigned-number
                              (zero-or-more blank) ; some blanks
                              (in "+" "-" "*" "/" "^") ; an arithmetic operation
                              (zero-or-more blank))) ; some blanks
           map-unsigned-number))
   "Regular expression for a simple algebraic expression.
involving decimal numbers, the operations +,-,*, /, and ^, and an
arbitrary amount of whitespace, all on one line.")

(defvar map-whole-string-math-regexp
  (concat (rx line-start (zero-or-more blank))
          map-simple-math-regexp
          (rx (zero-or-more blank) line-end))
  "Regular expression for a simple math expression matching a whole string.

The simple algebraic expression must not contain parens, but can
have an arbitrary amount of whitespace at the beginning and end.
Used by ``map--zero-out-balanced-parens''.")

(defvar map-number-regexp
  (rx (and (optional "-") map-unsigned-number))
  "Regular expression for a decimal number.")

(defvar map-latex-begin-regexp
  (rx "\\" "begin"
      (group "{"
             (minimal-match (zero-or-more not-newline))
             "}" ))
  "Regular expression for a LaTeX \begin{} delimiter.")

(defvar map-insert-regexp
  (rx (group (zero-or-more blank)
                       "="
                       (zero-or-more blank))
      (optional (regexp map-number-regexp)))
  "Regexp to match equal sign after expression where result will be inserted.")


;;;; Functions

(defun map--balanced-paren-positions (str)
  "Return a list of positions of all balanced parens in STR.
The list is in reverse order, and has the property for any two
balanced paren pairs A and B, if A is contained in B then A's
position will occur before B's position in the list."
  (let (parens end)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (and (search-forward "(" nil t)
                  (condition-case _err
                      (setq end (scan-sexps (1- (point)) 1))
                    (scan-error nil)))
        (push (cons (match-beginning 0) end) parens)))
    parens))

(defun map--zero-out-balanced-parens (str)
  "Replace all math expressions delimited by parens in STR with \"0\"s.

This function is used to convert into a math expression with
balanced parens into a simple math expression without balanced
parens that can be matched with a regular expression.

For example, for the string:

\" 6.23+(3.789/(5-4)) + 6.4*(2 - (5+3) *736.83 ) /2000\",

this function returns the string:

\" 6.23+0000000000000 + 6.4*00000000000000000000 /2000\"."
  (cl-loop for (beg . end) in (map--balanced-paren-positions str)
           ;; the indices beg and end of a pair of balanced parens returned by
           ;; map--balanced-paren-positions are actually one character to the
           ;; right of each paren. Thus, to get the start and end indices of
           ;; the contents of the parens, we can use beg as is, and we have to
           ;; subtract 2 from end.
           do (when (string-match-p map-whole-string-math-regexp
                                    (substring str beg (- end 2)))
                  (setf (substring str (1- beg) (1- end))
                        (make-string (- end beg) ?0)))
           finally return str))

(defun map--latex-eq-regexp (rdelim)
  "Regular expression used to split LaTeX expression and its termination.

The termination is either an equal-sign, an end of line, of the
right delimiter RDELIM."
  (rx (group (minimal-match (zero-or-more (not "="))))
      (or "=" line-end (literal rdelim))))

(defun map--latex-rdelim (ldelim)
  "Return the LaTeX fragment right delimiter that pairs with LDELIM."
  (cond ((string-match map-latex-begin-regexp ldelim)
         (concat "\\end" (substring ldelim (match-beginning 1) (match-end 1))))
        ((string-equal "\\(" ldelim) "\\)")
        ((string-equal "\\\[" ldelim) "\\\]")
        ((string-equal "$$" ldelim) "$$")
        ((string-equal "$" ldelim) "$")
        (t             'error)))

(defun map--display-result (m-string lang insert p subexp)
  "Evaluate M-STRING using ``calc-eval'' and display result in  minibuffer.

The result is also copied into the kill ring so that it can be
pasted with ``yank''.

LANG is the format that ``calc-eval'' expects M-STRING to be
in. Currently only 'flat and 'latex are supported.

If INSERT is true, then insert the evaluation result as position
P in the buffer, prefixed by \"=\". If there was already a
previous result, then replace it. If SUBEXP is nonzero, jump to
the end of the subexpression of depth SUBEXP before inserting the
result."
  (let* ((calc-language lang)
     (result (save-match-data (calc-eval m-string))))
    (if insert
        (progn
          (unless (or (null subexp) (zerop subexp))
            (goto-char (match-end subexp)))
          (if (looking-at map-insert-regexp)
              (replace-match (concat "\\1" result))
            (insert "=" result)))
      (goto-char p))
    (kill-new result)
    (message "Result %s => %s" m-string result)))


;;;; Commands

;;;###autoload
(defun math-at-point-simple (&optional insert)
  "Evaluate the simple math expression at point with `calc-eval'.

A simple math expression consists of decimal numbers, and the
operations +, -, *, /, and ^, and can be interspersed with
whitespace. A simple math expression cannot contain parens. The
whole expression must on the current line.

The result is displayed in the minibuffer and copied into the
kill ring so that it can be pasted with ``yank''. If the point is
not inside a simple math expression, then instead run
``quick-calc''.

If optional prefix argument INSERT is provided,
then insert the evaluation result after the expression, prefixed
by \"=\". If there was already a previous result, then replace
it."
  (interactive "P")
  (let ((p (point)))
    (beginning-of-line)
    (cl-loop while (re-search-forward map-simple-math-regexp
                                      (line-end-position) t)
             when (<= (match-beginning 0) p (match-end 0))
             return (map--display-result (match-string 0) 'flat insert p 0)
             finally do (goto-char p) (quick-calc insert))))

;;;###autoload
(defun math-at-point (&optional insert)
  "Evaluate the math expression at point with `calc-eval'.

A math expression consists of decimal numbers, the operations +,
-, *, /, ^, and parentheses, and can be interspersed with
whitespace. The whole expression must be fully contained in the
current line. If the point is inside a LaTeX math fragment, then
the math expression can also contain LaTeX syntax.

The result is displayed in the minibuffer and copied into the
kill ring (so that it can be pasted with ``yank''). If the point
is not within a math expression, then instead run `quick-calc'.

If optional prefix argument INSERT is provided, then insert the
evaluation result after the expression, prefixed by \"=\". If
there was already a previous result, then replace it."
  (interactive "P")
  (let ((latex-params (org-inside-LaTeX-fragment-p)))
    (if latex-params
        (math-at-point-latex insert latex-params)
      (math-at-point-expression insert))))

;;;###autoload
(defun math-at-point-expression (&optional insert)
  "Evaluate the math expression at point with `calc-eval'.

A math expression consists of decimal numbers, the operations +,
-, *, /, ^, and parentheses, and can be interspersed with
whitespace. The whole expression must be fully contained in the
current line.

The result is displayed in the minibuffer and copied into the
kill ring (so that it can be pasted with ``yank''). If the point
is not within a math expression, then instead run `quick-calc'.

If optional prefix argument INSERT is provided, then insert the
evaluation result after the expression, prefixed by \"=\". If
there was already a previous result, then replace it."
  (interactive "P")
  (let* ((l-beg (line-beginning-position))
         (rel-p (- (point) l-beg))
         (this-line (thing-at-point 'line t))
         (zero-line (map--zero-out-balanced-parens this-line)))
    (cl-loop for pos = 0 then (match-end 0)
             while (and (string-match map-simple-math-regexp zero-line pos)
                        (< pos (length this-line)))
             when (<= (match-beginning 0) rel-p (match-end 0))
             return (let* ((m-beg (match-beginning 0))
                           (m-end (match-end 0)))
                      (when insert
                        (goto-char (+ l-beg m-end)))
                      (map--display-result (substring this-line m-beg m-end)
                                           'flat insert (point) 0))
             finally do (quick-calc insert))))


;;;###autoload
(defun math-at-point-latex (&optional insert params)
    "Evaluate the LaTeX math expression at point with `calc-eval'.

The result is displayed in the minibuffer and copied into the
kill ring (so that it can be pasted with ``yank''). If the point
is not within a LaTeX math expression, then instead run `quick-calc'.

If optional prefix argument INSERT is provided, then insert the
evaluation result after the expression, prefixed by \"=\". If
there was already a previous result, then replace it.

Optional argument PARAMS should contains a cons cell with the
left delimiter of the LaTeX fragment and its position. If PARAMS
isn't provided, it is set to the output
of (org-inside-LaTeX-fragment-p)."
  (interactive "P")
  (unless params
    (setq params (org-inside-LaTeX-fragment-p)))
  (when params
    (let* ((p      (point))
           (ldelim (car params))
           (lpos   (max (line-beginning-position)
                        (+ (cdr params) (length ldelim))))
           (rdelim (map--latex-rdelim ldelim))
           (eq-rx  (map--latex-eq-regexp rdelim))
           (rpos   (min (line-end-position)
                        (progn (goto-char lpos)
                               (search-forward rdelim nil t)))))
      (when (<= p rpos)
        (goto-char lpos)
        (cl-loop while (and (re-search-forward eq-rx rpos t)
                            (not (string-empty-p (match-string 1))))
                 when (<= (match-beginning 1) p (match-end 1))
                 return (map--display-result (match-string-no-properties 1)
                                             'latex insert p 1)
                 finally do (goto-char p) (quick-calc insert))))))

;;;; Footer

(provide 'math-at-point)

;;; math-at-point.el ends here
