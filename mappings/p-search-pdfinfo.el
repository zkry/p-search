;;; p-search-pdfinfo.el --- PDF Info mapping for p-search  -*- lexical-binding: t; -*-

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

(p-search-def-field 'pdf-title 'text :weight 3)
(p-search-def-field 'pdf-subject 'text :weight 3)
(p-search-def-field 'pdf-producer 'text :weight 3)
(p-search-def-field 'pdf-keywords 'text :weight 3)
(p-search-def-field 'pdf-author 'text :weight 3)
(p-search-def-field 'pdf-creator 'text :weight 3)
(p-search-def-field 'pdf-producer 'text :weight 3)
(p-search-def-field 'pdf-creationdate 'text :weight 3)
(p-search-def-field 'pdf-moddate 'text :weight 3)
(p-search-def-field 'pdf-pages 'text :weight 3)
(p-search-def-field 'pdf-pagesize 'category)

(defgroup p-search-pdfinfo nil
  "Customization for p-search pdfinfo mapping."
  :group 'p-search
  :prefix "p-search-pdfinfo-")

(defcustom p-search-pdfinfo-executable (executable-find "pdfinfo")
  "Location of the pdfinfo executable."
  :type '(choice (const :tag "Not found" nil)
                 (file :must-match t :tag "Executable: "))
  :group 'p-search-pdfinfo)

(defun p-search-pdfinfo--parse-info (filename)
  "Run `pdfinfo' on FILENAME, and convert to a list of fields.

`pdfinfo' will come from `p-search-pdfinfo-executable'."
  (when-let ((output (string-trim (shell-command-to-string (format "%s -isodates %s" p-search-pdfinfo-executable filename))))
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
             (add-to-list 'fields (cons 'pdf-title (cdr field))))
            ("Subject"
             (add-to-list 'fields (cons 'pdf-subject (cdr field))))
            ("Keywords"
             (add-to-list 'fields (cons 'pdf-keywords (cdr field))))
            ("Author"
             (add-to-list 'fields (cons 'pdf-author (cdr field))))
            ("Creator"
             (add-to-list 'fields (cons 'pdf-creator (cdr field))))
            ("Producer"
             (add-to-list 'fields (cons 'pdf-producer (cdr field))))
            ("CreationDate"
             (add-to-list 'fields (cons 'pdf-creationdate (cdr field))))
            ("ModDate"
             (add-to-list 'fields (cons 'pdf-moddate (cdr field))))
            ("Pages"
             (add-to-list 'fields (cons 'pdf-pages (cdr field))))
            ("Page size"
             (add-to-list 'fields (cons 'pdf-pagesize (cdr field))))))))))

(defun p-search-pdfinfo-annotator (_args document)
  "Annotate DOCUMENT with `pdfinfo'-derived fields."
  (let ((file-name (p-search-document-property document 'file-name)))
    (when (string= "pdf" (file-name-extension file-name))
      (when-let ((id (p-search-document-property document 'id))
                 (new-fields (p-search-pdfinfo--parse-info file-name)))
        (list (p-search-document-extend document
                                        (cons 'pdf id)
                                        new-fields))))))

(defconst p-search-pdfinfo-mapping
  (p-search-candidate-mapping
   :name "PDF Info"
   :required-property-list '(file-name)
   :input-spec '()
   :options-spec '()
   :function #'p-search-pdfinfo-annotator))

(add-to-list 'p-search-candidate-mappings p-search-pdfinfo-mapping)

(provide 'p-search-pdfinfo)
;;; p-search-pdfinfo.el ends here
