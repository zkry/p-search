;;; psx-symbols.el --- Emacs symbols search -*- lexical-binding: t; -*-

;; Author: Zachary Romero
;; URL: https://github.com/zkry/p-search.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (heap "0.5"))
;; Keywords: tools
;;

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package extends p-search to allow search on Emacs symbols,
;; with their associated properties.

;; The package adds a new candidate generator FUNCTION-SYMBOL which
;; generates candidates from the symbols in Emacs with function
;; definitions.

;;; Code:

(require 'p-search)

(defun psx-symbols-function-of-mode (mode-sym fn-sym)
  "Return non-nil if FN-SYM is a function related to the mode of MODE-SYM."
  (or (memq mode-sym (command-modes fn-sym))
      (let* ((mode-map-sym (intern (concat (symbol-name mode-sym) "-map")))
             (keymap (symbol-value mode-map-sym)))
        (and (keymapp keymap)
             (where-is-internal fn-sym keymap nil t)))))

(defconst psx-symbols-function-symbol-candidate-generator
  (p-search-candidate-generator-create
   :id 'psx-symbols-function-symbol-candidate-generator
   :name "FUNCTION-SYMBOL"
   :input-spec '((symbol-name-regexp . (p-search-infix-regexp
                                        :key "s"
                                        :description "Symbol Name Regex")))
   :options-spec '((command-p . (p-search-infix-toggle
                                 :key "-c"
                                 :description "Is Command?"))
                   (major-mode . (p-search-infix-choices
                                  :key "-m"
                                  :instruction-string "Is either specified as beloinging to selected mode, or is accessible viea MAJOR-MODE-map keymap or "
                                  :description "Major Mode"
                                  :choices (lambda ()
                                             (let ((modes '()))
                                               (mapatoms (lambda (sym)
                                                           (when (string-suffix-p "-mode" (symbol-name sym))
                                                             (push sym modes))))
                                               modes)))))
   :function
   (lambda (args)
     (let* ((node (alist-get 'info-node args))
            (command-p (alist-get 'command-p args))
            (major-mode-val (alist-get 'major-mode args))
            (symbol-name-regexp (alist-get 'symbol-name-regexp args))
            (docs '()))
       (mapatoms (lambda (sym)
                   (let ((sym-name (symbol-name sym)))
                     (when (and (string-match-p symbol-name-regexp sym-name)
                                (and (fboundp sym) (functionp sym))
                                (or (not command-p) (commandp sym))
                                (or (not major-mode-val) (psx-symbols-function-of-mode major-mode sym)))
                       (push (p-search-documentize
                              `(base ,(cons sym-name
                                            (concat sym-name "\n" (or (documentation sym) "")))))
                             docs)))))
       docs))
   :lighter-function
   (lambda (args)
     "functions")))

(add-to-list 'p-search-candidate-generators psx-symbols-function-symbol-candidate-generator)

(provide 'psx-symbols)
