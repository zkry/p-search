;;; psx-denote.el --- p-search mapping for Denote  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Zachary Romero, Samuel W. Flint

;; Author: Zachary Romero
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

;; This file implements basic denote metadata mapping for p-search.
;; As of now, it supports including the title, file type, identifier,
;; signature (configurable when enabling the mapping), and keywords.
;; Denote keywords can be represented either as a categorical list, or
;; as a comma-separated text string for easier matching; the
;; representations used are configurable when enabling the mapping.

;;; Code:

(require 'p-search)
(require 'denote)

(p-search-def-field 'denote-title 'text :weight 3)
(p-search-def-field 'denote-type 'category)
(p-search-def-field 'denote-identifier 'text :weight 10)
(p-search-def-field 'denote-signature 'text :weight 3)

(defgroup psx-denote nil
  "Customization for Denote p-search mapping."
  :group 'p-search
  :group 'denote
  :prefix "psx-denote-")

(defconst psx-denote--custom-toggle-type
  '(choice (const :tag "Off by default" nil)
           (const :tag "On by default" on)))

(defcustom psx-denote-categories-as-keywords-p nil
  "Should denote keywords be generated as categories?"
  :type psx-denote--custom-toggle-type
  :group 'psx-denote)

(defcustom psx-denote-categories-as-text-p 'on
  "Should denote keywords be generated as formatted text?"
  :type psx-denote--custom-toggle-type
  :group 'psx-denote)

(defcustom psx-denote-include-signature-p 'on
  "Should the Denote signature be a part of metadata?"
  :type psx-denote--custom-toggle-type
  :group 'psx-denote)

(defcustom psx-denote-only-denote-p nil
  "Should the p-search mapper drop non-denote files?"
  :type psx-denote--custom-toggle-type
  :group 'psx-denote)

(defun psx-denote-annotator (args document)
  "Annotate DOCUMENT with denote-related metadata."
  (let* ((file-name (p-search-document-property document 'file-name)))
    (let-alist args
      (when (denote-file-is-note-p file-name)
        (let* ((id (p-search-document-property document 'id))
               (new-id (list 'denote id))
               (identifier (denote-retrieve-filename-identifier file-name))
               (title (denote-retrieve-filename-title file-name))
               (keywords (denote-extract-keywords-from-path file-name))
               (type (denote-file-type file-name))
               (title-nice (denote-retrieve-front-matter-title-value file-name type))
               (new-fields `((denote-type . ,type)
                             (denote-identifier . ,identifier))))

          (when-let ((include-signature .include-signature)
                     (signature (denote-retrieve-filename-signature file-name)))
            (ignore include-signature)
            (push (cons 'denote-signature signature) new-fields))
          (when keywords
            (push (cons 'keywords keywords) new-fields))
          (when title
            (push (cons 'title title) new-fields))
          (when (and (stringp title-nice)
                     (not (string= title title-nice)))
            (push (cons 'denote-title (list title-nice title)) new-fields))
          (list (p-search-document-extend document new-id new-fields)))))))

(defun psx-denote--arg-display (_input-spec _output-spec arguments)
  "Display ARGUMENTS in a condensed format."
  (let-alist arguments
    (format "category: %s, signature: %s"
            (propertize
             (cond
              ((and .category-keywords .category-text) "both")
              (.category-keywords "cat")
              (.category-text "text")
              (t "off"))
             'face 'p-search-value)
            (propertize (if .include-signature "on" "off") 'face 'p-search-value))))

(defconst psx-denote-mapping
  (p-search-candidate-mapping-create
   :id 'psx-denote-mapping
   :name "Denote"
   :required-property-list '(file-name)
   :input-spec '()
   :options-spec '((include-signature . (p-search-infix-toggle
                                         :key "-s"
                                         :description "Include Signature"
                                         :default-value (lambda () psx-denote-include-signature-p))))
   :function #'psx-denote-annotator
   :condenced-arg-display-function #'psx-denote--arg-display))

(add-to-list 'p-search-candidate-mappings psx-denote-mapping)

(provide 'psx-denote)
;;; psx-denote.el ends here
