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


;;; Customization

(defgroup psx-bibtex nil
  "Customization for `p-search' bibtex candidate generator."
  :group 'p-search
  :group 'bibtex
  :prefix "psx-bibtex-")

(defcustom psx-bibtex-open-entry-function nil
  "Function to open a bibtex entry.

This function should take two arguments, FILENAME and ENTRY-KEY,
and will be called as follows.

\(apply psx-bibtex-open-entry-function filename entry-key)"
  :type '(choice (const :tag "Don't Open" nil)
                 (function :tag "Custom Function"))
  :group 'psx-bibtex)


;;; Property Definitions

(defun psx-bibtex--lighter (config)
  "Describe (briefly) BibTex Candidate generator CONFIG."
  (format "BIBTEX:%s"
          (mapconcat
           #'file-name-nondirectory
           (if (listp (alist-get 'files config))
               (alist-get 'files config)
             (list (alist-get 'files config)))
           "/")))

(defun psx-bibtex--name (id)
  "Extract BibTeX Entry name from ID."
  (pcase-let* ((`(_ ,key _) id))
    key))
(p-search-def-property 'bibtex 'name #'psx-bibtex--name)

(defun psx-bibtex--content (id)
  "Get entry content (i.e., abstract) from BibTeX entry ID."
  (pcase-let* ((`(_ _ ,entry-data) id))
    (alist-get "abstract" entry-data "" nil #'string-equal-ignore-case)))
(p-search-def-property 'bibtex 'content #'psx-bibtex--content)

(defun psx-bibtex--filename (id)
  "Get entry filename from ID."
  (pcase-let* ((`(,file _ _) id))
    file))
(p-search-def-property 'bibtex 'file-name #'psx-bibtex--filename)

(defun psx-bibtex--fields (id)
  "Get various searchable fields from BibTeX entry ID."
  (pcase-let* ((`(,file _ ,entry-data) id))
    (let ((author (alist-get "author" entry-data nil nil #'string-equal-ignore-case))
          (title (cl-some (lambda (key)
                            (alist-get key entry-data nil nil #'string-equal-ignore-case))
                          '("title" "booktitle" "journaltitle" "journal")))
          (keywords (alist-get "keywords" entry-data nil nil #'string-equal-ignore-case))
          fields)
      (when author
        (push (cons 'author (mapcar #'string-trim (string-split author " and "))) fields))
      (when title
        (push (cons 'title title) fields))
      (when keywords
        (push (cons 'keywords (mapcar #'string-trim (string-split keywords ","))) fields))
      fields)))
(p-search-def-property 'bibtex 'fields #'psx-bibtex--fields)

(defun psx-bibtex--goto-entry (id)
  "Go to BibTeX entry ID.

Use `psx-bibtex-open-entry-function' if set, otherwise, do
nothing."
  (when psx-bibtex-open-entry-function
    (pcase-let* ((`(,filename ,entry-key _) id))
      (apply psx-bibtex-open-entry-function filename entry-key))))
(p-search-def-function 'bibtex 'p-search-goto-document #'psx-bibtex--goto-entry)


;;; Candidate Generator

(defun psx-bibtex--candidate-generator (args)
  "Generate BibTeX candidates from ARGS.

ARGS should be an alist containing the following keys:

 - `FILES' a file or list of files (latter useful for presets)."
  (let-alist args
    (let ((files-list (if (listp .files) .files (list .files)))
          documents)
      (dolist (file files-list)
        (let ((entries (parsebib-parse file)))
          (maphash (lambda (key entry)
                     (push (p-search-documentize (list 'bibtex (list file key entry))) documents))
                   entries)))
      (nreverse documents))))

(defconst psx-bibtex-candidate-generator
  (p-search-candidate-generator-create
   :id 'psx-bibtex-candidate-generator
   :name "BIBTEX"
   :input-spec '((files . (p-search-infix-file
                           :key "f"
                           :description "BibTeX Filename")))
   :function #'psx-bibtex--candidate-generator
   :lighter-function #'psx-bibtex--lighter)
  "BibTeX Candidate generator for `p-search'.

In use, there is a single argument, `files', which should be a
file name or list of file names.")

(add-to-list 'p-search-candidate-generators psx-bibtex-candidate-generator)

(provide 'psx-bibtex)
;;; psx-bibtex.el ends here
