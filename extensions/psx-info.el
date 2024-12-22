;;; psx-info.el --- Emacs Search Tool Aggregator -*- lexical-binding: t; -*-

;;; Commentary:

;; This packages extends p-search to allow search on info files.
;; Require this file and the Info candidate generator should be
;; available in a p-search session.

;;; The psx-info document identifier is a list with the three
;;; components: (dir file node-name)

;;; Code:

(require 'p-search)

(defvar psx-info--info-to-file (make-hash-table :test #'equal)
  "Hash table of base to (hash table of extension to full file path).")

(defvar psx-info-content (make-hash-table :test #'equal)
  "Hash table of info-identifier to string contents.
This is used to speed up the reading of info files in order to
not have to open the same file repeatedly.")

(defun psx-info--build-tree ()
  "Generate and return cache of info files."
  (dolist (dir Info-directory-list)
    (condition-case err
      (dolist (file (directory-files-recursively dir ".*\\.info"))
        (let* ((is-gz))
          (when (string-suffix-p ".gz" file)
            (setq file (string-trim-right file "\\.gz"))
            (setq is-gz t))
          (let* ((base (file-name-base file))
                 (extension (file-name-extension file))
                 (file-map (gethash base psx-info--info-to-file)))
            (unless file-map
              (let ((ht (make-hash-table :test #'equal)))
                (puthash base ht psx-info--info-to-file)
                (setq file-map ht)))
            (if is-gz
                (puthash extension (concat file ".gz") file-map)
              (puthash extension file file-map)))))
      (error (message "Error occurred when retrieving info files from directory %s" dir))))
  psx-info--info-to-file)

(defun psx-info--info-candidates ()
  "Return list of selectable info candidates."
  (when (or (not psx-info--info-to-file)
            (= (hash-table-count psx-info--info-to-file) 0))
    (psx-info--build-tree))
  (hash-table-keys psx-info--info-to-file))

(defun psx-info--documents-from-file (filepath)
  "Return info documents for FILEPATH."
  (let ((documents '()))
    (with-temp-buffer
      (psx-info--insert-file-contents filepath)
      (goto-char (point-min))
      (search-forward "")
      (forward-line 1)
      (while (search-forward-regexp "File: \\(.*\\),  Node: \\([^\n,]+\\),  \\(?:Next:\\|Prev:\\|Up:\\)"
                                    (save-excursion (forward-line 1) (point))
                                    t)
        (let* ((file-name (match-string 1))
               (node-name (match-string 2))
               (doc-id (list (file-name-directory filepath) file-name node-name)))
          (forward-line 2)
          (let* ((content-start-point (point)))
            (unless (search-forward "" nil t)
              (goto-char (point-max)))
            (let* ((content-string (buffer-substring-no-properties content-start-point
                                                                   (1- (point)))))
              (when (not (equal "Top" node-name))
                (puthash doc-id content-string psx-info-content))))
          (push (p-search-documentize (list 'psx-info doc-id)) documents)
          (forward-line 1))))
    documents))

(defun psx-info--documents-for-entry (entry)
  "Return list of p-search documents for info ENTRY."
  (when (or (not psx-info--info-to-file)
            (= (hash-table-count psx-info--info-to-file) 0)
            (not (gethash (symbol-name entry) psx-info--info-to-file)))
    (psx-info--build-tree))
  (let* ((file-map (gethash (symbol-name entry) psx-info--info-to-file))
         (docs '()))
    (maphash
     (lambda (extension filepath)
       (unless (and (> (hash-table-count file-map) 1)
                    (equal extension "info"))
         (setq docs (append docs (psx-info--documents-from-file filepath)))))
     file-map)
    docs))

(defun psx-info--title (info-identifier)
  "Return the title of INFO-IDENTIFIER."
  (concat (nth 1 info-identifier) ": " (nth 2 info-identifier)))

(defun psx-info--insert-file-contents (file)
  (cond
   ((and (string-suffix-p ".gz" file) (not auto-compression-mode))
    (call-process "gzip" nil t nil "-c" "-d" file))
   ((and (not (file-attributes file))
         (file-attributes (concat file ".gz")))
    (psx-info--insert-file-contents (concat file ".gz")))
   (t
    (insert-file-contents file))))

(defun psx-info--node-contents (node-name file)
  "Return the contents of NODE-NAME in info FILE."
  (with-temp-buffer
    (psx-info--insert-file-contents file)
    (search-forward (concat ",  Node: " node-name))
    (forward-line 2)
    (let* ((contents-start (point)))
      (unless (search-forward "" nil t)
        (goto-char (point-max)))
      (buffer-substring-no-properties contents-start (1- (point))))))

(defun psx-info--content (info-identifier)
  "Return the content of INFO-IDENTIFIER."
  (if-let ((content (gethash info-identifier psx-info-content)))
      content
    (pcase-let ((`(,dir ,file ,node-name) info-identifier))
    (let ((info-file (file-name-concat dir file)))
      (with-temp-buffer
        (psx-info--insert-file-contents info-file)
        (if (search-forward "\nIndirect:" nil t)
            ;; We're dealing with a directory file, we need to read
            ;; the tag table to determine the location of the node
            (let ((references '()))
              (forward-line 1)
              (while (not (looking-at ""))
                (let* ((line (thing-at-point 'line))
                       (parts (string-split line ": " t "[ \n]*")))
                  (push (cons (car parts) (string-to-number (cadr parts))) references))
                (forward-line 1))
              (search-forward "\nTag Table:\n")
              (search-forward (concat ": " node-name))
              (search-forward "")
              (let* ((index (string-to-number (buffer-substring (point) (pos-eol))))
                     (at-ref (or (seq-find
                                  (pcase-lambda (`(,file . ,idx))
                                    (>= index idx))
                                  references)
                                 (car (seq-sort-by #'cdr #'> references))))
                     (ref-file (file-name-concat dir (car at-ref))))
                (psx-info--node-contents node-name ref-file)))
          (psx-info--node-contents node-name info-file)))))))

(defun psx-info--goto (info-identifier)
  "Open the info page of INFO-IDENTIFIER."
  (pcase-let ((`(,dir ,file ,node) info-identifier))
    (Info-find-node file node)))

(defconst psx-info-candidate-generator
  (p-search-candidate-generator-create
   :id 'psx-info-candidate-generator
   :name "INFO"
   :input-spec '((info-node . (p-search-infix-choices
                                :key "i"
                                :description "Info Node"
                                :choices psx-info--info-candidates)))
   :options-spec '()
   :function
   (lambda (args)
     (let* ((node (alist-get 'info-node args)))
       (psx-info--documents-for-entry node)))
   :lighter-function
   (lambda (args)
     (let* ((node (alist-get 'info-node args)))
       (format "info:%s" (symbol-name node))))))

(p-search-def-property 'psx-info 'title #'psx-info--title)
(p-search-def-property 'psx-info 'content #'psx-info--content)

(add-to-list 'p-search-candidate-generators psx-info-candidate-generator)

(p-search-def-function 'psx-info 'p-search-goto-document #'psx-info--goto)

(provide 'psx-info)
