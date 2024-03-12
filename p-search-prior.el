;;; p-search-prior.el --- Implementation of search priors  -*- lexical-binding:t -*-

;;; Commentary:

;; This package implements various search priors to use in a p-search session.

;;; Code:

;;; Reader Functions

(require 'p-search-query)
(require 'subr-x)
(require 'eieio)

(defvar p-search-base-prior)

(declare-function p-search-prior-arguments "p-search.el")
(declare-function p-search-prior-results "p-search.el")
(declare-function p-search-generate-search-space "p-search.el")
(declare-function p-search--notify-main-thread "p-search.el")
(declare-function p-search--notify-main-thread-after-init "p-search.el")
(declare-function p-search-prior-template-input-spec "p-search.el")
(declare-function p-search-prior-template-options-spec "p-search.el")
(declare-function p-search-prior-template-create "p-search.el")


(defun p-search-prior-default-arguments (template)
  "Return default input and options of TEMPLATE as one alist.
This function is primarily used to create the base prior with reasonable
default inputs, with the args being set to nil."
  (let* ((input-spec (p-search-prior-template-input-spec template))
         (options-spec (p-search-prior-template-options-spec template))
         (res '()))
    (pcase-dolist (`(,name . (,_type . ,options)) input-spec)
      (let* ((default (plist-get options :default))
             (default-val (if (functionp default) (funcall default) default)))
        (setq res (cons
                   (cons name default-val)
                   res))))
    (pcase-dolist (`(,name . (,_type . ,options)) options-spec)
      (let* ((default (plist-get options :default))
             (default-val (if (functionp default) (funcall default) default)))
        (setq res (cons
                   (cons name default-val)
                   res))))
    (nreverse res)))

(defconst p-search-prior-base--buffers
  (p-search-prior-template-create
   :name "BUFFERS"
   :input-spec '()
   :options-spec '((major-mode . (string
                                  :key "-m"
                                  :description "Buffer major mode")))
   :search-space-function
   (lambda (args)
     (let-alist args
       (let* ((buffers (buffer-list))
              (major-mode-sym (and .major-mode (intern .major-mode))))
         (when .major-mode
           (setq buffers
                 (seq-filter
                  (lambda (buf)
                    (with-current-buffer buf
                      (eql major-mode-sym major-mode)))
                  buffers)))
         (seq-map
          (lambda (buf)
            (list 'buffer buf))
          buffers))))
   :add-prior-function #'p-search-add-prior-dispatch--buffers))

(defconst p-search-prior-base--filesystem
  (p-search-prior-template-create
   :name "FILESYSTEM"
   :input-spec '((base-directory . (directory-name
                                    :key "d"
                                    :description "Directories"
                                    :default (lambda () default-directory)))
                 (filename-regexp . (regexp
                                     :key "f"
                                     :description "Filename Pattern"
                                     :default ".*")))
   :options-spec '((ignore . (regexp
                              :key "-i"
                              :description "Ignore Patterns"
                              :multiple t))  ;; TODO - implement multiple
                   (use-git-ignore . (toggle
                                      :key "-g"
                                      :description "Git Ignore"
                                      :default-value on)))
   :search-space-function
   (lambda (args)
     (let-alist args
       (let* ((default-directory .base-directory)
              (file-candidates (if .use-git-ignore
                                   (string-split (shell-command-to-string "git ls-files") "\n" t "[\n ]")
                                 (string-split (shell-command-to-string "find . -type f") "\n" t "[\n ]")))
              (files '()))
         (dolist (file file-candidates)
           (catch 'skip
             (when (string-prefix-p "./" file)
               (setq file (substring file 2)))
             (unless (or (equal .filename-regexp ".*")
                         (string-match-p .filename-regexp file))
               (throw 'skip nil))
             (when (and .ignore (string-match-p .ignore file))
               (throw 'skip nil))
             (setq file (file-name-concat default-directory file))
             (push file files)))
         (nreverse files))))
   :add-prior-function #'p-search-add-prior-dispatch--file-system))

(defconst p-search-prior-base--multi-filesystem
  (p-search-prior-template-create
   :name "MULTI-FILESYSTEM"
   :input-spec '((base-directories . (directory-names
                                      :key "d"
                                      :description "Directories"
                                      :default (lambda () (list default-directory))))
                 (filename-regexp . (regexp
                                     :key "f"
                                     :description "Filename Pattern"
                                     :default ".*")))
   :options-spec '((ignore . (regexp
                              :key "-i"
                              :description "Ignore Patterns"
                              :multiple t))  ;; TODO - implement multiple
                   (use-git-ignore . (toggle
                                      :key "-g"
                                      :description "Git Ignore"
                                      :default-value on)))
   :search-space-function #'p-search-prior-base--multi-filesystem-search-space
   :add-prior-function #'p-search-add-prior-dispatch--file-system))

(defun p-search-prior-base--multi-filesystem-search-space (args)
  "Return search space of the multi-filesystem base prior with prior's ARGS.."
  (let-alist args
    (let* ((files '()))
      (dolist (default-directory .base-directories)
        (dolist (file (if .use-git-ignore
                          (string-split (shell-command-to-string "git ls-files") "\n" t "[\n ]")
                        (string-split (shell-command-to-string "find . -type f") "\n" t "[\n ]")))
          (catch 'skip
            (when (string-prefix-p "./" file)
              (setq file (substring file 2)))
            (unless (or (equal .filename-regexp ".*")
                        (string-match-p .filename-regexp file))
              (throw 'skip nil))
            (when (and .ignore (string-match-p .ignore file))
              (throw 'skip nil))
            (setq file (file-name-concat default-directory file))
            (push file files))))
      (nreverse files))))

(defun p-search-prior-get-base-directories ()
  "Return list of base directories of the base prior."
  (let* ((args (p-search-prior-arguments p-search-base-prior)))
    (or (alist-get 'base-directories args)
        (list (alist-get 'base-directory args))
        (error "Filesystem directory not supported"))))

(defconst p-search--subdirectory-prior-template
  (p-search-prior-template-create
   :name "subdirectory"
   :input-spec '((include-directories . (directory-names
                                         :key "i"
                                         :description "Directories")))
   :initialize-function
   (lambda (prior)
     (let* ((args (p-search-prior-arguments prior))
            (files (p-search-generate-search-space))
            (directories (seq-map #'expand-file-name (alist-get 'include-directories args)))
            (result-ht (p-search-prior-results prior)))
       (dolist (file files)
         (catch 'out
           (let* ((file-expanded (expand-file-name file)))
             (dolist (dir directories)
               (when (string-prefix-p dir file-expanded)
                 (puthash file 'yes result-ht)
                 (throw 'out nil)))))))))
  "Sample prior.")


(defconst p-search--filename-prior-template
  (p-search-prior-template-create
   :name "file-name"
   :input-spec '((include-filename . (regexp
                                      :key "i"
                                      :description "Pattern")))
   :initialize-function
   (lambda (prior) ;; TODO - remove base-prior-args as it can be obtained from base prior and args for that mattr
     (let* ((args (p-search-prior-arguments prior))
            (files (p-search-generate-search-space))
            (fn-pattern (alist-get 'include-filename args))
            (result-ht (p-search-prior-results prior))) ;; normally should do async or lazily
       (dolist (file files)
         (puthash file (if (string-match-p fn-pattern file) 'yes 'no) result-ht)))
     (p-search--notify-main-thread))))


(defconst p-search--filetype-prior-template
  (p-search-prior-template-create
   :name "file-type"
   :input-spec
   '((extension . (string
                   :key "e"
                   :description "File Extension")))
   :initialize-function
   (lambda (prior)
     (let* ((args (p-search-prior-arguments prior))
            (files (p-search-generate-search-space))
            (ext-suffix (alist-get 'base-prior-args args))
            (result-ht (p-search-prior-results prior)))
       (dolist (file files)
         (puthash file (if (string-suffix-p ext-suffix file) 'yes 'no) result-ht)))
     (p-search--notify-main-thread))))

(defconst p-search--timestamp-high-to-days 0.758518518521)

(defun p-search--normal-pdf (x mu sigma)
  "Calculate the PDF of a normal distribution N(MU, SIGMA) for value X."
  (let ((x (float x)) ;; TODO - this needs to be fast!
        (mu (float mu))
        (sigma (float sigma)))
    (/ (exp (* -0.5 (expt (/ (- x mu) sigma) 2.0)))
       (* sigma (sqrt (* 2 float-pi))))))

(defconst p-search--modification-date-prior-template
  (p-search-prior-template-create
   :name "modification-date"
   :input-spec
   '((date . (string
              :key "d"
              :description "Date"))
     (sigma . (number
               :key "s"
               :description "Std Dev")))
   :initialize-function
   (lambda (prior)
     (let* ((args (p-search-prior-arguments prior))
            (files (p-search-generate-search-space))
            (date (alist-get 'date args))
            (sigma (alist-get 'sigma args))
            (result-ht (p-search-prior-results prior)))
       (let* ((mu (* (car (date-to-time date)) p-search--timestamp-high-to-days)))
         (dolist (file files)
           (let* ((attrs (file-attributes file))
                  (days (* (car (nth 5 attrs)) p-search--timestamp-high-to-days))
                  (p (p-search--normal-pdf days mu sigma)))
             (puthash file p result-ht)))))
     (p-search--notify-main-thread))))

(defconst p-search--file-size-prior-template
  (p-search-prior-template-create
   :name "file-size"
   :input-spec '((data-size . (memory :key "d"
                                      :description "data size"))
                 (sigma . (memory :key "s"
                                  :description "Std Dev")))
   :initialize-function
   (lambda (prior)
     (let* ((args (p-search-prior-arguments prior))
            (files (p-search-generate-search-space))
            (mu-bytes (alist-get 'data-size args))
            (sigma-bytes (alist-get 'sigma args))
            (result-ht (p-search-prior-results prior)))
       (dolist (file files)
         (let* ((size (nth 7 (file-attributes file)))
                (p (p-search--normal-pdf size mu-bytes sigma-bytes)))
           (message "p(%d, %d, %d) = %f" size mu-bytes sigma-bytes p)
           (puthash file p result-ht))))
     (p-search--notify-main-thread))))


;;; Git Priors

(defun p-seach--git-authors ()
  "Return list of git authors."
  (let* ((base-directories (p-search-prior-get-base-directories))
         (res '()))
    (dolist (default-directory base-directories)
      (setq res (append res (string-lines (shell-command-to-string "git log --all --format='%aN' | sort -u") t))))
    res))

(defun p-search--git-branches ()
  "Return list of all git branches."
  (let* ((base-args (p-search-prior-arguments p-search-base-prior))
         (default-directory (alist-get 'base-directory base-args)))
    (seq-map
     (lambda (line)
       (substring line 2))
     (string-lines (shell-command-to-string "git branch -a") t))))

(defconst p-search--git-author-prior-template
  (p-search-prior-template-create
   :name "git author"
   :input-spec
   '((git-author . (choice
                    :key "a"
                    :description "Author"
                    :choices p-seach--git-authors)))
   :initialize-function 'p-search--git-author-prior-template-init
   :default-result 'no))

(defun p-search--git-author-prior-template-init (prior)
  "Initialize process for git-author PRIOR."
  (let* ((init-buf (current-buffer))
         (args (p-search-prior-arguments prior))
         (author (alist-get 'git-author args))
         (base-directories (p-search-prior-get-base-directories))
         (git-command (format "git log --author=\"%s\" --name-only --pretty=format: | sort -u" author)))
    (dolist (default-directory base-directories)
      (let* ((buf (generate-new-buffer "*p-search-git-author*")))
        (make-process
         :name "p-seach-git-author-prior"
         :buffer buf
         :command `("sh" "-c" ,git-command)
         :sentinel (lambda (_proc event)
                     (when (or (member event '("finished\n" "deleted\n"))
                               (string-prefix-p "exited abnormally with code" event)
                               (string-prefix-p "failed with code" event))
                       (with-current-buffer init-buf
                         (p-search--notify-main-thread))))
         :filter (lambda (proc string)
                   (when (buffer-live-p (process-buffer proc))
                     (with-current-buffer (process-buffer proc)
                       (let ((moving (= (point) (process-mark proc))))
                         (save-excursion
                           (goto-char (process-mark proc))
                           (insert string)
                           (set-marker (process-mark proc) (point)))
                         (if moving (goto-char (process-mark proc)))
                         (let ((files (string-split string "\n"))
                               (result-ht (p-search-prior-results prior)))
                           (dolist (f files)
                             (puthash (file-name-concat default-directory f) 'yes result-ht))))))))))))

(defconst p-search--git-branch-prior-template
  (p-search-prior-template-create
   :name "git-branch"
   :input-spec
   '((branch . (choice
                :key "b"
                :description "Git Branch"
                :choices p-seach--git-branches))
     (n . (number
           :key "n"
           :description "Number commits back"
           :default-value 5))) ;; TODO - may break sometimes
   :initialize-function 'p-search--git-branch-prior-template-init
   :default-result 'no))

(defun p-search--git-branch-prior-template-init (prior)
  "Initialize function for git-branch PRIOR."
  (let* ((args (p-search-prior-arguments prior))
         (base-prior-args (p-search-prior-arguments p-search-base-prior))
         (branch (alist-get 'branch args))
         (n (alist-get 'n args))
         (default-directory (alist-get 'base-directory base-prior-args))
         (git-command (format "git diff --name-only %s~%d %s"
                              branch n branch)))
    (dolist (default-directory base-directories)
      (let* ((buf (generate-new-buffer "*p-search-git-branch*")))
        (make-process
         :name "p-seach-git-author-prior"
         :buffer buf
         :command `("sh" "-c" ,git-command)
         :sentinel (lambda (_proc event)
                     (when (or (member event '("finished\n" "deleted\n"))
                               (string-prefix-p "exited abnormally with code" event)
                               (string-prefix-p "failed with code" event))
                       (p-search--notify-main-thread)))
         :filter (lambda (proc string)
                   (when (buffer-live-p (process-buffer proc))
                     (with-current-buffer (process-buffer proc)
                       (let ((moving (= (point) (process-mark proc))))
                         (save-excursion
                           (goto-char (process-mark proc))
                           (insert string)
                           (set-marker (process-mark proc) (point)))
                         (if moving (goto-char (process-mark proc)))
                         (let ((files (string-split string "\n"))
                               (result-ht (p-search-prior-results prior)))
                           (dolist (f files)
                             (puthash (file-name-concat default-directory f) 'yes result-ht))))))))))))

(defconst p-search--git-co-changes-prior-template
  (p-search-prior-template-create
   :name "git co-changes"
   :input-spec
   '() ;; TODO - may break sometimes
   :options-spec
   '((use-edited-files . (toggle
                          :key "-e"
                          :description "Use `git-status' files as input."
                          :default-value on))
     (file . (file
              :key "-f"
              :description "Specific file to calculate co-changes.")))
   :initialize-function 'p-search--git-co-changes-prior-template-init
   :default-result 0))

(defun p-search--git-co-changes-prior-template-init (prior)
  "Initialize function for git-co-changes PRIOR."
  (let* ((args (p-search-prior-arguments prior))
         (base-prior-args (p-search-prior-arguments p-search-base-prior))
         (use-edited-files (alist-get 'use-edited-files args))
         (file (alist-get 'file args))
         (default-directory (alist-get 'base-directory base-prior-args))
         (buf (generate-new-buffer "*p-search-git-cochanges-search*"))
         ;; (git-command (format "git diff --name-only %s~%d %s"
         ;;                      branch n branch))
         (base-files '()))
    (when use-edited-files
      (let* ((status-files (seq-map
                            (lambda (line) (substring line 3))
                            (seq-filter
                             (lambda (line) (not (string-prefix-p "??" line)))
                             (string-lines
                              (shell-command-to-string "git status -s") t)))))
        (setq base-files (append base-files status-files))))
    (when file
      (setq base-files (append base-files (list file))))
    (dolist (file base-files)
      (let* ((git-command (format "git log --pretty=format:\"%%H\" -- %s"
                                  file)))
        (make-process
         :name "p-seach-git-cochanges-prior"
         :buffer buf
         :command `("sh" "-c" ,git-command)
         :sentinel (lambda (proc event)
                     (when (equal event "finished\n")
                       (with-current-buffer (process-buffer proc)
                         (let* ((result-ht (p-search-prior-results prior))
                                (file-counts (make-hash-table :test #'equal))
                                (N 0)
                                (commit-hashes (string-lines (buffer-string) t)))
                           (dolist (hash commit-hashes)
                             (let* ((changed-files
                                     (thread-first "git show --pretty=format:\"\" --name-only %s"
                                                   (format hash)
                                                   (shell-command-to-string)
                                                   (string-lines t))))
                               (cl-incf N (length changed-files))
                               (dolist (file changed-files)
                                 (puthash file (1+ (gethash file file-counts 0)) file-counts))))
                           (maphash
                            (lambda (file count)
                              (let* ((p (/ (float count) N)))
                                (puthash (file-name-concat default-directory file) p result-ht)))
                            file-counts)))
                       (p-search--notify-main-thread))))))))

(defconst p-search--git-mod-freq-prior-template
  (p-search-prior-template-create
   :name "git commit frequency"
   :input-spec
   '((n-commits . (number
                   :key "n"
                   :description "Consider last N commits."
                   :default-value 20)))
   :options-spec
   '((branch . (choice
                :key "-b"
                :description "Git Branch"
                :choices p-search--git-branches)))
   :initialize-function #'p-search--git-mod-freq-prior-template-init
   :default-result 0))


(defun p-search--git-mod-freq-prior-template-init (prior)
  "Initialization function of git-mod-freq-prior-template PRIOR."
  (let* ((base-dirs (p-search-prior-get-base-directories))
         (args (p-search-prior-arguments prior))
         (search-space (p-search-generate-search-space))
         (n-commits (alist-get 'n-commits args))
         (branch (alist-get 'branch args))
         (last-commits-cmd (if branch
                               (format "git log %s -%d --pretty=format:\"%%H\"" branch n-commits)
                             (format "git log -%d --pretty=format:\"%%H\"" n-commits)))
         (commits (string-lines (shell-command-to-string last-commits-cmd) t))
         (file-counts (make-hash-table :test #'equal))
         (N (length search-space)) ;; apply laplace smoothing
         (result-ht (p-search-prior-results prior)))
    (dolist (default-directory base-dirs)
      (dolist (commit commits)
        (let* ((files (string-lines (shell-command-to-string (format "git show --pretty=format:\"\" --name-only %s" commit)) t)))
          (cl-incf N (length files))
          (dolist (file files)
            ;; default of 1 for laplace smoothing
            (puthash file (1+ (gethash file file-counts 1)) file-counts)))))
    (maphash
     (lambda (file count)
       (let* ((p (/ (float count) N)))
         (puthash (file-name-concat default-directory file) p result-ht)))
     file-counts)
    (setf (p-search-prior-default-result prior) (/ 1.0 N))))

;;; Emacs

(defconst p-search--emacs-open-buffer-template
  (p-search-prior-template-create
   :name "emacs open buffer"
   :input-spec
   '()
   :options-spec
   '()
   :initialize-function #'p-search--emacs-open-buffer-prior-template-init
   :default-result 'no))

(defun p-search--emacs-open-buffer-prior-template-init (prior)
  "Initialize function for Emacs open-buffer PRIOR."
  (let* ((buffer-files (seq-map #'buffer-file-name (buffer-list)))
         (result-ht (p-search-prior-results prior)))
    (dolist (file buffer-files)
      (puthash file 'yes result-ht))))

;;; Buffers

(defconst p-search--buffer-name-template
  (p-search-prior-template-create
   :name "buffer name"
   :input-spec '((query . (string
                           :key "q"
                           :description "search query")))
   :options-spec
   '((regexp . (toggle
                :key "-r"
                :description "Regex"
                :default-value nil)))
   :initialize-function
   (lambda (prior)
     (let* ((args (p-search-prior-arguments prior))
            (query (alist-get 'query args))
            (regexp-p (alist-get 'regexp args))
            (all-docs (p-search-generate-search-space))
            (result-ht (p-search-prior-results prior)))
       (dolist (doc all-docs)
         (pcase-let* ((`(buffer ,buf) doc)
                      (name (buffer-name buffer))
                      (matchp (if regexp-p
                                  (string-match-p query name)
                                (string-search query name))))
           (if matchp
               (puthash doc 'yes result-ht)
             (puthash doc 'no result-ht))))))
   :default-result 'no))

(defun p-search--major-modes ()
  "Return a list of symbols that are likely major mode commands."
  (let ((modes '()))
    (mapatoms (lambda (sym)
                (when (and (fboundp sym)
                           (string-suffix-p "-mode" (symbol-name sym)))
                  (push sym modes))))
    modes))

(defconst p-search--buffer-major-mode
  (p-search-prior-template-create
   :name "buffer major mode"
   :input-spec '((mode-symbol . (choice
                                 :key "m"
                                 :description "major mode"
                                 :choices p-search--major-modes)))
   :options-spec
   '()
   :initialize-function
   (lambda (prior)
     (let* ((args (p-search-prior-arguments prior))
            (mode-symbol (intern (alist-get 'mode-symbol args)))
            (all-docs (p-search-generate-search-space))
            (result-ht (p-search-prior-results prior)))
       (dolist (doc all-docs)
         (pcase-let* ((`(buffer ,buf) doc))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (if (eq major-mode mode-symbol)
                   (puthash doc 'yes result-ht)
                 (puthash doc 'no result-ht))))))))
   :default-result 'no))

(defconst p-search--buffer-text-query-prior-template
  (p-search-prior-template-create
   :name "text query"
   :input-spec '((query . (string
                           :key "q"
                           :description "search query")))
   :options-spec
   '((algorithm . (choice
                   :key "-a"
                   :description "search ranking algorithm"
                   :default-value bm25
                   :choices
                   (bm25
                    boolean
                    tf-idf))))
   :initialize-function #'p-search--buffer-text-query-template-init
   :default-result 'no
   :result-hint-function #'p-search--text-search-hint))

(defun p-search--buffer-text-query-template-init (prior)
  "Run text query for PRIOR."
  (let* ((init-buffer (current-buffer))
         (args (p-search-prior-arguments prior))
         (query (alist-get 'query args))
         (algorithm (alist-get 'algorithm args)))
    (unless (eq algorithm 'bm25)
      (error "Algorithm not implemented"))
    (let* ((all-docs (p-search-generate-search-space))
           (total-size 0))
      (dolist (doc all-docs)
        (let ((size (p-search-document-size doc)))
          (unless size
            (error "Invalid document %s" doc))
          (cl-incf total-size size)))
      (p-search-query query (length all-docs) total-size
                      (lambda (p-ht)
                        (setf (p-search-prior-results prior) p-ht)
                        (with-current-buffer init-buffer
                          (p-search--notify-main-thread)))))))

;;; Text search

(defconst p-search--text-query-prior-template
  (p-search-prior-template-create
   :name "text query"
   :input-spec '((query . (string
                           :key "q"
                           :description "search query")))
   :options-spec
   '((algorithm . (choice
                   :key "-a"
                   :description "search ranking algorithm"
                   :default-value bm25
                   :choices
                   (bm25
                    boolean
                    tf-idf)))
     (tool . (choice
              :key "-p"
              :description "search program"
              :choices
              (rg
               ag
               grep))))
   :initialize-function #'p-search--text-query-template-init
   :default-result 'no
   :result-hint-function #'p-search--text-search-hint))

(defun p-search--text-query-template-init (prior)
  "Initialize search functions for text query PRIOR."
  (let* ((init-buffer (current-buffer))
         (args (p-search-prior-arguments prior))
         (query (alist-get 'query args))
         (algorithm (alist-get 'algorithm args))
         (tool (alist-get 'tool args))
         (base-directories (p-search-prior-get-base-directories)))
    (when (not (member tool '(ag rg grep)))
      (error "Tool not implemented"))
    (unless (eq algorithm 'bm25)
      (error "Algorithm not implemented"))
    (let* ((all-docs (p-search-generate-search-space))
           (total-size 0))
      (dolist (doc all-docs)
        (let ((size (p-search-document-size doc)))
          (unless size
            (error "Invalid file produced %s" file))
          (cl-incf total-size size)))
      (let ((p-search-query-tool tool)
            (p-seach-query-directories (p-search-prior-get-base-directories)))
        (p-search-query query (length all-docs) total-size
                        (lambda (p-ht)
                          (setf (p-search-prior-results prior) p-ht)
                          (with-current-buffer init-buffer
                            (p-search--notify-main-thread))))))))

(defun p-search--text-search-hint (prior buffer)
  "Mark places where the query args of PRIOR matches text in BUFFER."
  (let* ((args (p-search-prior-arguments prior))
         (query (p-search-query-parse (alist-get 'query args))))
    (with-current-buffer buffer
      (p-search-query-mark query))))

(provide 'p-search-prior)
;;; p-search-prior.el ends here
