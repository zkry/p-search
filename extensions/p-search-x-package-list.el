;;; p-search-x-package-list.el --- P-Search Candidate Generator for Installable Packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Samuel W. Flint

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

;;

;;; Code:

(require 'p-search)
(require 'cl-lib)
(require 'package)

(defun p-search-x-package-list--lighter (_config)
  "Show lighter for p-search-x-package-list candidate generator."
  "PKG")

(defun p-search-x-package-list--name (id)
  "Get the package name from ID."
  (pcase-let* ((`(,name _ _) id))
    (symbol-name name)))
(p-search-def-property 'package 'name #'p-search-x-package-list--name)

(defun p-search-x-package-list--content (id)
  "Get the content (i.e., summary) from ID."
  (pcase-let* ((`(_ ,summary _) id))
    summary))
(p-search-def-property 'package 'content #'p-search-x-package-list--content)

(p-search-def-field 'package-version 'text)
(p-search-def-field 'package-url 'text)
(p-search-def-field 'package-type 'category)
(p-search-def-field 'package-requires 'category)

(defun p-search-x-package-list--fields (id)
  "Get a list of metadata fields for package ID.

Metadata collected include:
 - author
 - url
 - package type
 - package version
 - package keywords."
  (pcase-let* ((`(_ _ ,properties) id))
    (let (fields)
      (if (vectorp properties)
          (progn
            (when-let* ((version (package--bi-desc-version properties)))
              (push (cons 'package-version
                          (mapconcat #'number-to-string version "."))
                    fields))
            (push (cons 'package-type "builtin") fields))
        (progn
          (push (cons 'package-type "installable") fields)
          (when-let ((version (package-desc-version properties)))
            (push (cons 'package-version
                        (mapconcat #'number-to-string version "."))
                  fields))
          (when-let ((requirements (package-desc-reqs properties)))
            (push (cons 'package-requires
                        (mapcar (lambda (x) (symbol-name (car x))) requirements))
                  fields))
          (when-let ((extras (package-desc-extras properties)))
            (when-let ((url (alist-get :url extras)))
              (push (cons 'package-url url) fields))
            (let (authors)
              (when-let ((maintainers (alist-get :maintainers extras)))
                (mapc (lambda (maint)
                        (push (car maint) authors))
                      maintainers))
              (when-let ((auths (alist-get :authors extras)))
                (mapc (lambda (auth)
                        (push (car auth) authors))
                      auths))
              (when authors
                (push (cons 'author authors) fields)))
            (when-let (kws (alist-get :keywords extras))
              (push (cons 'keywords kws) fields)))))
      fields)))
(p-search-def-property 'package 'fields #'p-search-x-package-list--fields)

(defun p-search-x-package-list--candidate-generator (args)
  "Generate a list of package candidates, based on ARGS."
  (let-alist args
    (append
     (mapcar (lambda (pkg)
               (p-search-documentize
                (list 'package (list (car pkg) (aref (cdr pkg) 2) (cdr pkg)))))
             package--builtins)
     (mapcar (lambda (pkg)
               (p-search-documentize
                (list 'package
                      (list (car pkg) (package-desc-summary (nth 1 pkg)) (nth 1 pkg)))))
             package-alist)
     (mapcar (lambda (pkg)
               (p-search-documentize
                (list 'package
                      (list (car pkg) (package-desc-summary (nth 1 pkg)) (nth 1 pkg)))))
             package-archive-contents))))

(defconst p-search-x-package-list-candidate-generator
  (p-search-candidate-generator-create
   :id 'pxs-package-list-candidate-generator
   :name "PKG"
   :function #'p-search-x-package-list--candidate-generator
   :lighter-function #'p-search-x-package-list--lighter))

(add-to-list 'p-search-candidate-generators p-search-x-package-list-candidate-generator)

(provide 'p-search-x-package-list)
;;; p-search-x-package-list.el ends here
