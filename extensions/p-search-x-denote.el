;;; p-search-x-denote.el --- p-search mapping for Denote  -*- lexical-binding: t; -*-

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
(require 'denote nil t)

(p-search-def-field 'denote-type 'category)
(p-search-def-field 'denote-identifier 'text :weight 10)
(p-search-def-field 'denote-signature 'text :weight 3)

(declare-function denote-retrieve-filename-signature "denote.el")
(declare-function denote-retrieve-front-matter-title-value "denote.el")
(declare-function denote-file-type "denote.el")
(declare-function denote-extract-keywords-from-path "denote.el")
(declare-function denote-retrieve-filename-title "denote.el")
(declare-function denote-retrieve-filename-identifier "denote.el")
(declare-function denote-file-is-note-p "denote.el")

(defgroup p-search-x-denote nil
  "Customization for Denote `p-search' mapping."
  :group 'p-search
  :group 'denote
  :prefix "p-search-x-denote-")

(defcustom p-search-x-denote-include-signature-p 'on
  "Should the Denote signature be a part of metadata?"
  :type '(choice (const :tag "Off by default" nil)
                 (const :tag "On by default" on))
  :group 'p-search-x-denote)

(defun p-search-x-denote-annotator (args document)
  "Annotate DOCUMENT with denote-related metadata with given ARGS."
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
            (push (cons 'title (list title-nice title)) new-fields))
          (list (p-search-document-extend document new-id new-fields)))))))

(defun p-search-x-denote--arg-display (_input-spec _output-spec arguments)
  "Display ARGUMENTS in a condensed format."
  (let-alist arguments
    (format "signature: %s"
            (propertize (if .include-signature "on" "off") 'face 'p-search-value))))

(defconst p-search-x-denote-mapping
  (p-search-candidate-mapping-create
   :id 'p-search-x-denote-mapping
   :name "Denote"
   :required-property-list '(file-name)
   :input-spec '()
   :options-spec '((include-signature . (p-search-infix-toggle
                                         :key "-s"
                                         :description "Include Signature"
                                         :default-value (lambda () p-search-x-denote-include-signature-p))))
   :function #'p-search-x-denote-annotator
   :short-arg-disp-func #'p-search-x-denote--arg-display))

(add-to-list 'p-search-candidate-mappings p-search-x-denote-mapping)

(provide 'p-search-x-denote)
;;; p-search-x-denote.el ends here
