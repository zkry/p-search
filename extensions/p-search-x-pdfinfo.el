;;; p-search-x-pdfinfo.el --- PDF Info mapping for p-search  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; Keywords: tools

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

;; This file implements a metadata mapping for p-search using pdfinfo
;; to collect metadata.  The following fields are collected, if
;; available:
;;
;; - Title
;; - Subject
;; - Keywords
;; - Author
;; - Creator
;; - Producer
;; - CreationDate
;; - ModDate
;; - Pages
;; - Page size

;;; Code:

(require 'p-search)

(p-search-def-field 'pdf-subject 'text :weight 3)
(p-search-def-field 'pdf-creator 'text :weight 3)
(p-search-def-field 'pdf-producer 'text :weight 3)
(p-search-def-field 'pdf-pages 'text :weight 3)
(p-search-def-field 'pdf-pagesize 'category)

(defgroup p-search-x-pdfinfo nil
  "Customization for `p-search' pdfinfo mapping."
  :group 'p-search
  :prefix "p-search-x-pdfinfo-")

(defcustom p-search-x-pdfinfo-executable (executable-find "pdfinfo")
  "Location of the pdfinfo executable."
  :type '(choice (const :tag "Not found" nil)
                 (file :must-match t :tag "Executable: "))
  :group 'p-search-x-pdfinfo)

(defun p-search-x-pdfinfo--parse-info (filename)
  "Run `pdfinfo' on FILENAME, and convert to a list of fields.

`pdfinfo' will come from `p-search-x-pdfinfo-executable'."
  (when-let ((output (string-trim (shell-command-to-string (format "%s -isodates %s" p-search-x-pdfinfo-executable filename))))
             (initial-field-list (mapcar (lambda (record)
                                           (let* ((parts (split-string record ":"))
                                                  (field-name (string-trim (car parts)))
                                                  (field-value (string-trim (string-join (cdr parts) ":"))))
                                             (cons field-name field-value)))
                                         (split-string output (rx ?\n)))))
    (let ((fields))
      (cl-dolist (field initial-field-list (nreverse fields))
        (unless (string-empty-p (cdr field))
          (pcase (car field)
            ("Title"
             (push (cons 'title (cdr field)) fields))
            ("Keywords"
             (push (cons 'keywords (cdr field)) fields))
            ("Author"
             (push (cons 'author (cdr field)) fields))
            ("CreationDate"
             (push (cons 'creation-date (cdr field)) fields))
            ("ModDate"
             (push (cons 'modification-date (cdr field)) fields))
            ("Subject"
             (push (cons 'pdf-subject (cdr field)) fields))
            ("Creator"
             (push (cons 'pdf-creator (cdr field)) fields))
            ("Producer"
             (push (cons 'pdf-producer (cdr field)) fields))
            ("Pages"
             (push (cons 'pdf-pages (cdr field)) fields))
            ("Page size"
             (push (cons 'pdf-pagesize (cdr field)) fields)))))
      fields)))

(defun p-search-x-pdfinfo-annotator (_args document)
  "Annotate DOCUMENT with `pdfinfo'-derived fields.

Options taken from ARGS."
  (let ((file-name (p-search-document-property document 'file-name)))
    (when (string= "pdf" (file-name-extension file-name))
      (if-let ((id (p-search-document-property document 'id))
               (new-fields (p-search-x-pdfinfo--parse-info file-name)))
          (p-search-document-extend document
                                    (cons 'pdf id)
                                    new-fields)))))

(defconst p-search-x-pdfinfo-mapping
  (p-search-candidate-mapping-create
   :id 'p-search-x-pdfinfo-mapping
   :name "PDF Info"
   :required-property-list '(file-name)
   :input-spec '()
   :options-spec '()
   :function #'p-search-x-pdfinfo-annotator))

(add-to-list 'p-search-candidate-mappings p-search-x-pdfinfo-mapping)

(provide 'p-search-x-pdfinfo)
;;; p-search-x-pdfinfo.el ends here
