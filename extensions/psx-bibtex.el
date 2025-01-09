;;; psx-bibtex.el --- P-Search BibTex Candidate Generator  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; Keywords: tools, tex

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

;;

;;; Code:

(require 'p-search)
(require 'parsebib)
(require 'cl)

(defun psx-bibtex--lighter (config)
  "Describe (briefly) BibTex Candidate generator CONFIG."
  (format "BIBTEX:%s" (alist-get 'file input-options-alist)))

(p-search-def-property 'bibtex 'id #'cl-first)
(p-search-def-property 'bibtex 'name #'cl-first)
(p-search-def-property 'bibtex 'content #'cl-third)

(defun psx-bibtex--candidate-generator (args)
  "Generate BibTeX candidates from ARGS."
  (let-alist args
    (let ((entries (parsebib-parse .file))
          documents)
      (maphash (lambda (key entry)
                 (push (p-search-document-extend
                        (p-search-documentize (list 'bibtex (list key .file (alist-get "abstract" entry "" nil #'string=))))
                        (cons 'bib key)
                        (list (cons 'author (string-split (or (assoc-string "author" entry t) "") " and "))
                              (cons 'title (or (assoc-string "title" entry t) ""))
                              (cons 'keywords (string-split (or (assoc-string "keywords" entry t) "") ", "))))
                       documents))
               entries)
      documents)))

(defvar psx-bibtex-candidate-generator
  (p-search-candidate-generator-create
   :id 'psx-bibtex-candidate-generator
   :name "BIBTEX"
   :input-spec '((file . (p-search-infix-file
                          :key "f"
                          :description "BibTeX Filename")))
   :function #'psx-bibtex--candidate-generator
   :lighter-function #'psx-bibtex--lighter))

(add-to-list 'p-search-candidate-generators psx-bibtex-candidate-generator)

(provide 'psx-bibtex)
;;; psx-bibtex.el ends here
