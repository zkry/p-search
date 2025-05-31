;;; p-search-x-bibtex.el --- A BibTeX candidate generator for p-search  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; Version: 0.9.0
;; Keywords: tools, tex
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

;; This package implements a BibTeX candidate generator for
;; `p-search'.  Of note, it is designed to be used interactively, or
;; as part of `p-search-default-command-behavior'.  When used with the
;; latter, there is a single, mandatory option of `files', which can
;; be set to a single file name or a list of file names, which will be
;; read in turn.
;;
;; Additionally, bibliography entries can be opened internally or
;; externally.  This is disabled by default, but can be configured by
;; setting `p-search-x-bibtex-open-entry-function' to a function which takes
;; a filename and an entry key, and opens the bibentry as you choose.

;;; Code:

(require 'p-search)
(require 'parsebib nil t)
(require 'cl-lib)
(require 'compat)

(declare-function parsebib-parse "parsebib.el")


;;; Customization

(defgroup p-search-x-bibtex nil
  "Customization for `p-search' bibtex candidate generator."
  :group 'p-search
  :group 'bibtex
  :prefix "p-search-x-bibtex-")

(defcustom p-search-x-bibtex-open-entry-function nil
  "Function to open a bibtex entry.

This function should take two arguments, FILENAME and ENTRY-KEY,
and will be called as follows.

    (apply p-search-x-bibtex-open-entry-function filename entry-key)"
  :type '(choice (const :tag "Don't Open" nil)
                 (function :tag "Custom Function"))
  :group 'p-search-x-bibtex)


;;; Property Definitions

(defun p-search-x-bibtex--lighter (config)
  "Describe (briefly) BibTex Candidate generator CONFIG."
  (format "BIBTEX:%s"
          (mapconcat
           #'file-name-nondirectory
           (if (listp (alist-get 'files config))
               (alist-get 'files config)
             (list (alist-get 'files config)))
           "/")))

(defun p-search-x-bibtex--name (id)
  "Extract BibTeX Entry name from ID."
  (pcase-let* ((`(_ ,key _) id))
    key))
(p-search-def-property 'bibtex 'name #'p-search-x-bibtex--name)

(defun p-search-x-bibtex--content (id)
  "Get entry content (i.e., abstract) from BibTeX entry ID."
  (pcase-let* ((`(_ _ ,entry-data) id))
    (alist-get "abstract" entry-data "" nil #'string-equal-ignore-case)))
(p-search-def-property 'bibtex 'content #'p-search-x-bibtex--content)

(defun p-search-x-bibtex--filename (id)
  "Get entry filename from ID."
  (pcase-let* ((`(,file _ _) id))
    file))
(p-search-def-property 'bibtex 'file-name #'p-search-x-bibtex--filename)

(defun p-search-x-bibtex--fields (id)
  "Get various searchable fields from BibTeX entry ID."
  (pcase-let* ((`(,_file _ ,entry-data) id))
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
(p-search-def-property 'bibtex 'fields #'p-search-x-bibtex--fields)

(defun p-search-x-bibtex--goto-entry (id)
  "Go to BibTeX entry ID.

Use `p-search-x-bibtex-open-entry-function' if set, otherwise, do
nothing."
  (when p-search-x-bibtex-open-entry-function
    (pcase-let* ((`(,filename ,entry-key _) id))
      (apply p-search-x-bibtex-open-entry-function filename entry-key))))
(p-search-def-function 'bibtex 'p-search-goto-document #'p-search-x-bibtex--goto-entry)


;;; Candidate Generator

(defun p-search-x-bibtex--candidate-generator (args)
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

(defconst p-search-x-bibtex-candidate-generator
  (p-search-candidate-generator-create
   :id 'p-search-x-bibtex-candidate-generator
   :name "BIBTEX"
   :input-spec '((files . (p-search-infix-files
                           :key "f"
                           :description "BibTeX Filenames"
                           :prompt "BibTeX Files: ")))
   :function #'p-search-x-bibtex--candidate-generator
   :lighter-function #'p-search-x-bibtex--lighter)
  "BibTeX Candidate generator for `p-search'.

In use, there is a single argument, `files', which should be a
file name or list of file names.")

(add-to-list 'p-search-candidate-generators p-search-x-bibtex-candidate-generator)

(provide 'p-search-x-bibtex)
;;; p-search-x-bibtex.el ends here
