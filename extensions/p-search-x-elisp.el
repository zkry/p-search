;;; p-search-x-elisp.el --- A p-search candidate generator for emacs lisp symbols  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; Version: 0.9.0
;; Keywords: tools, lisp, help
;; Homepage: https://github.com/zkry/p-search

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

;; This library implements a `p-search' candidate generator for Emacs
;; Lisp symbols (functions, variables).  To use it with `p-search' by
;; default, you can either add the symbol
;; `p-search-x-elisp-candidate-generator' to your
;; `p-search-default-command-behavior' or to
;; `p-search-session-presets'.  You may also add the candidate
;; generator interactively.

;;; Code:

(require 'p-search)


;;; Properties and Fields

(p-search-def-field 'elisp-type 'category)

(defun p-search-x-elisp--lighter (_)
  "Return Elisp Candidate Generator lighter."
  "ELISP")

(defun p-search-x-elisp--name (id)
  "Compute the name for a symbol ID."
  (pcase-let ((`(,symbol ,type) id))
    (format "%s: %s" type symbol)))
(p-search-def-property 'elisp 'name #'p-search-x-elisp--name)

(defun p-search-x-elisp--fields (id)
  "Compute fields for item ID."
  (pcase-let ((`(_ ,type) id))
    (list (cons 'elisp-type (symbol-name type)))))
(p-search-def-property 'elisp 'fields #'p-search-x-elisp--fields)

(defun p-search-x-elisp--content (id)
  "Get documentation string for ID."
  (pcase-let ((`(,symbol ,type) id))
    (or (pcase type
          ('function
           (documentation symbol))
          ('variable
           (format "%s"
                   (or (get symbol 'variable-documentation)
                       (format "Variable `%s' not documented." symbol)))))
        "")))
(p-search-def-property 'elisp 'content #'p-search-x-elisp--content)


;;; Special Functions

(defun p-search-x-elisp--goto-doc (id)
  "Go to documentation for ID.

Use either `describe-function' or `describe-variable'."
  (pcase id
    (`(,symbol variable)
     (describe-variable symbol))
    (`(,symbol 'function)
     (describe-function symbol))))
(p-search-def-function 'elisp 'p-search-goto-document #'p-search-x-elisp--goto-doc)


;;; Candidate Generator

(defun p-search-x-elisp--candidate-generator (args)
  "Generate elisp `p-search' candidates, based on ARGS.

ARGS should contain the symbol `symbol-type' specifying which
types of symbols are included.  This should be one of the
following keywords.

 - `:functions' create only documents for functions.
 - `:variables' create only documents for variables.
 - `:all' create documents for both functions and variables."
  (let* ((type (alist-get 'symbol-type args)))
    (let (docs)
      (mapc (lambda (symbol)
                (when (and (functionp symbol) (not (eql type :variables)))
                  (push (p-search-documentize (list 'elisp (list symbol 'function))) docs))
                (when (and (symbolp symbol)
                           (not (eql type :functions))
                           (get symbol 'variable-documentation))
                  (push (p-search-documentize (list 'elisp (list symbol 'variable))) docs)))
              obarray)
      docs)))

(defconst p-search-x-elisp-candidate-generator
  (p-search-candidate-generator-create
   :id 'p-search-x-elisp-candidate-generator
   :input-spec '((symbol-type . (p-search-infix-choices
                                 :key "t"
                                 :description "Symbol Type"
                                 :choices (:all :functions :variables)
                                 :default-value :all)))
   :name "ELISP"
   :function #'p-search-x-elisp--candidate-generator
   :lighter-function #'p-search-x-elisp--lighter)
  "Elisp symbol candidate generator for `p-search'.

In use, there is a single argument, `symbol-type', which should
be one of the following keywords:

 - `:functions' create only documents for functions.
 - `:variables' create only documents for variables.
 - `:all' create documents for both functions and variables.")

(add-to-list 'p-search-candidate-generators p-search-x-elisp-candidate-generator)

(provide 'p-search-x-elisp)
;;; p-search-x-elisp.el ends here
