;;; psx-info.el --- Emacs Search Tool Aggregator -*- lexical-binding: t; -*-

;;; Commentary:

;; This packages extends p-search to allow search on info files.
;; Require this file and the Info candidate generator should be
;; available in a p-search session.

;;; Code:

(require 'p-search)

(defvar psx-info--info-to-file (make-hash-table :test #'equal)
  "Hash table of base to (hash table of extension to full file path)")

(defun psx-info--build-tree ()
  "Generate and return cache of info files."
  (dolist (dir Info-directory-list)
    (dolist (file (directory-files-recursively dir ".*\\.info"))
      (let* ((base (file-name-base file))
             (extension (file-name-extension file))
             (file-map (gethash base psx-info--info-to-file)))
        (unless file-map
          (let ((ht (make-hash-table :test #'equal)))
            (puthash base ht psx-info--info-to-file)
            (setq file-map ht)))
        (puthash extension file file-map))))
  psx-info--info-to-file)

(defun psx-info--info-candidates ()
  "Return list of selectable info candidates."
  (unless psx-info--info-to-file
    (psx-info--build-tree))
  (hash-table-keys psx-info--info-to-file))

(defun psx-info--documents-from-file (filepath)
  "Return info documents for FILEPATH."
  (let ((documents '()))
    (with-temp-buffer
      (insert-file-contents filepath)
      (goto-char (point-min))
      (search-forward "")
      (forward-line 1)
      (while (search-forward-regexp "Node: \\(.*\\),  \\(?:Next:\\|Prev:\\|Up:\\)"
                                    (save-excursion (forward-line 1) (point))
                                    t)
        (let ((node-name (match-string 1)))
          (forward-line 1)
          (let ((node-start (point)))
            (search-forward "" nil t)
            (let ((node-body (buffer-substring-no-properties node-start (1- (point)))))
              (push (p-search-documentize `(base ,(cons node-name node-body))) documents))
            (forward-line 1)))))
    documents))

(defun psx-info--documents-for-entry (entry)
  "Return list of p-search documents for info ENTRY."
  (unless psx-info--info-to-file
    (psx-info--build-tree))
  (let* ((file-map (gethash (symbol-name entry) psx-info--info-to-file))
         (docs '()))
    (maphash
     (lambda (extension filepath)
       (unless (and (> (hash-table-count file-map) 1)
                    (equal extension info))
         (setq docs (append docs (psx-info--documents-from-file filepath)))))
     file-map)
    docs))

(defconst psx-info-candidate-generator
  (p-search-candidate-generator-create
   :name "INFO"
   :input-spec '((info-node . (p-search-infix-choices
                                :key "i"
                                :description "Info Node"
                                :choices psx-info--info-candidates
                                :default-value elisp)))
   :options-spec '()
   :function
   (lambda (args)
     (let* ((node (alist-get 'info-node args)))
       (psx-info--documents-for-entry node)))))

(add-to-list 'p-search-candidate-generators psx-info-candidate-generator)

(provide 'psx-info)
