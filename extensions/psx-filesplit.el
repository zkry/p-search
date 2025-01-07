;;; psx-filesplit.el --- p-search file splitting mapping -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Zachary Romero

;; Author: Zachary Romero
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


;;; Code:

(require 'p-search)

(defun psx-filesplit-function (args document)
  "Split DOCUMENT according to ARGS."
  (let* ((split-size (alist-get 'split-size args))
         (file-content (p-search-document-property document 'content))
         (name (p-search-document-property document 'name)))
    (let* ((id (p-search-document-property document 'id)))
      (seq-map-indexed
       (lambda (string-group idx)
         (let* ((content (string-join string-group "\n"))
                (new-name (concat name (format "(%d)" idx)))
                (line-offset (* split-size idx))
                (new-id (list 'filesplit id line-offset)))
           (p-search-document-extend
            document new-id nil `((content . ,content)
                                  (name . ,new-name)
                                  (line-offset . ,line-offset)))))
       (seq-split (string-split file-content "\n") split-size)))))

(defconst psx-filesplit-mapping
  (p-search-candidate-mapping-create
   :id 'psx-filesplit-mapping
   :name "File Split"
   :required-property-list '()
   :input-spec '((split-size . (p-search-infix-number
                                :key "n"
                                :description "Split by N lines"
                                :default-value 20)))
   :options-spec '()
   :function #'psx-filesplit-function))

(add-to-list 'p-search-candidate-mappings psx-filesplit-mapping)

(provide 'psx-filesplit)
;;; psx-filesplit.el ends here
