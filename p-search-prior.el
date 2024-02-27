;;; p-search-prior.el --- Implementation of search priors  -*- lexical-binding:t -*-

;;; Commentary:

;; This package implements various search priors to use in a p-search session.

;;; Code:

;;; Reader Functions

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
         (nreverse files))))))

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
                 (throw 'out nil))))))
       (p-search--notify-main-thread-after-init))))
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


;;; Seach Priors


(defconst p-search--textsearch-prior-template
  (p-search-prior-template-create
   :name "text search"
   :input-spec
   '((search-term . (regexp :key "i" :description "Pattern")))
   :options-spec
   '((tool . (choice
              :key "-p"
              :description "search program"
              :choices
              (rg ;; TODO - how to specify defaults
               ag
               grep)))
     (strategy . (choice
                  :choices
                  (exact+case-insensitive+word-break
                   exact)
                  :key "-s"
                  :description "search scheme")))
   :initialize-function 'p-search--textsearch-prior-template-init
   :default-result 'no))

(defun p-search--textsearch-prior-template-init (prior)
  "Initialize process fro the text-search PRIOR."
  (let* ((args (p-search-prior-arguments prior))
         (base-prior-args (p-search-prior-arguments p-search-base-prior))
         (input (alist-get 'search-term args))
         (default-directory (alist-get 'base-directory base-prior-args)) ;; TODO: allow for multiple
         (ag-file-regex (alist-get 'filename-regexp base-prior-args))
         (cmd `("ag" ,input "-l" "--nocolor"))
         (buf (generate-new-buffer "*p-search-text-search*")))
    (when ag-file-regex
      (setq cmd (append cmd `("-G" ,ag-file-regex))))
    (make-process
     :name "p-search-text-search-prior"
     :buffer buf
     :command cmd
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
                         (puthash (file-name-concat default-directory f) 'yes result-ht))))))))))

;;; Git Priors

(defun p-seach--git-authors ()
  "Return list of git authors."
  (let* ((base-args (p-search-prior-arguments p-search-base-prior))
         (default-directory (alist-get 'base-directory base-args)))
    (string-lines (shell-command-to-string "git log --all --format='%aN' | sort -u") t)))

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
  ""
  (let* ((args (p-search-prior-arguments prior))
         (base-prior-args (p-search-prior-arguments p-search-base-prior))
         (author (alist-get 'git-author args))
         (default-directory (alist-get 'base-directory base-prior-args))
         (buf (generate-new-buffer "*p-search-git-author-search*"))
         (git-command (format "git log --author=\"%s\" --name-only --pretty=format: | sort -u" author)))
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
                         (puthash (file-name-concat default-directory f) 'yes result-ht))))))))))

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

(defun p-search--git-branch-prior-template-init (prior base-prior-args args)
  (let* ((branch (alist-get 'branch args))
         (n (alist-get 'n args))
         (default-directory (alist-get 'base-directory base-prior-args))
         (buf (generate-new-buffer "*p-search-git-branch-search*"))
         (git-command (format "git diff --name-only %s~%d %s"
                              branch n branch)))
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
                         (puthash (file-name-concat default-directory f) 'yes result-ht))))))))))

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
  ""
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
  "Initialization function of git-mod-freq-prior-template."
  (let* ((args (p-search-prior-arguments prior))
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
    (dolist (commit commits)
      (let* ((files (string-lines (shell-command-to-string (format "git show --pretty=format:\"\" --name-only %s" commit)) t)))
        (cl-incf N (length files))
        (dolist (file files)
          ;; default of 1 for laplace smoothing
          (puthash file (1+ (gethash file file-counts 1)) file-counts))))
    (maphash
     (lambda (file count)
       (let* ((p (/ (float count) N)))
         (puthash (file-name-concat default-directory file) p result-ht)))
     file-counts)
    (setf (p-search-prior-default-result prior) (/ 1.0 N))
    (p-search--notify-main-thread-after-init)))

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
  ""
  (let* ((buffer-files (seq-map #'buffer-file-name (buffer-list)))
         (result-ht (p-search-prior-results prior)))
    (dolist (file buffer-files)
      (puthash file 'yes result-ht))
    (p-search--notify-main-thread-after-init)))


;;; Text search

(defconst p-search--text-query-prior-template
  (p-search-prior-template-create
   :name "text query"
   :input-spec '((query . (string
                           :key "q"
                           :description "search query")))
   :options-spec
   '((subword . (toggle
                 :key "-s"
                 :description "break special casing to new queries"
                 :default-value on))
     (algorithm . (choice
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
   :default-result 0 ;; TODO: should actually be 'no
   :result-hint-function #'p-search--text-search-hint))

(defun p-search--text-query-parse-terms (query-str)
  (let* ((terms (string-split query-str " "))
         (broken-terms '()))
    (dolist (term terms)
      (let* ((new-terms '())
             (current "")
             (i 0))
        (while (< i (length term))
          (let ((at-char (aref term i)))
            (cond
             ((= at-char ?_)
              (when (> (length current) 0)
                (push (downcase current) new-terms)
                (setq current "")))
             ((= at-char ?-)
              (when (> (length current) 0)
                (push (downcase current) new-terms)
                (setq current "")))
             ((or (and (> i 0)
                       (char-uppercase-p at-char)
                       (not (char-uppercase-p (aref term (1- i)))))
                  (and (> i 0)
                       (> (length term) (1+ i))
                       (char-uppercase-p at-char)
                       (char-uppercase-p (aref term (1- i)))
                       (not (char-uppercase-p (aref term (1+ i))))))
              (when (> (length current) 0)
                (push (downcase current) new-terms)
                (setq current ""))
              (setq current (concat current (char-to-string at-char))))
             (t (setq current (concat current (char-to-string at-char))))))
          (cl-incf i))
        (when (and (> (length current) 0)
                   (not (equal current term)))
          (push (downcase current) new-terms))
        (setq broken-terms (append broken-terms new-terms))
        (setq new-terms '())))
    `((queries . ,terms)
      (broken-terms . ,broken-terms))))

(defun p-search--text-query-terms-expand (terms tool)
  (pcase tool
    ('ag
     (seq-mapcat
      (lambda (term)
        (list (list (concat "\\b(?=\\w)" term "\\b(?<=\w)") :case-insensitive t)
              (concat "[a-z]"
                      (capitalize (substring term 0 1))
                      (substring term 1))))
      terms))
    ('rg
     (seq-mapcat
      (lambda (term)
        (list (list (concat "\\b" term "\\b") :case-insensitive t)
              (concat "[a-z]"
                      (capitalize (substring term 0 1))
                      (substring term 1))))
      terms))
    ('emacs
     (seq-mapcat
      (lambda (term)
        (list (list (concat "\\<" term "\\>") :case-insensitive t)
              (concat "[a-z]"
                      (capitalize (substring term 0 1))
                      (substring term 1))))
      terms))))

(defun p-search--text-query-all-search-items (args &optional tool)
  "Generate list of query strings based on prior's ARGS.
If TOOL is provided, use it to override the tool in ARGS."
  (let* ((query (alist-get 'query args))
         (subword (alist-get 'subword args))
         (tool (or tool (alist-get 'tool args)))
         (terms (p-search--text-query-parse-terms query))
         (query-terms (alist-get 'queries terms)))
    (when subword
      (setq query-terms (append query-terms (alist-get 'broken-terms terms))))
    (p-search--text-query-terms-expand query-terms tool)))

(defun p-search--text-query-template-init (prior)
  "Initialize search functions for text query PRIOR."
  (let* ((args (p-search-prior-arguments prior))
         (algorithm (alist-get 'algorithm args))
         (tool (alist-get 'tool args)))
    (when (eq tool 'grep)
      (error "Tool not implemented"))
    (unless (eq algorithm 'bm25)
      (error "Algorithm not implemented"))
    (let* ((query-terms (p-search--text-query-all-search-items args)))
      (let* ((results (make-vector (length query-terms) nil)))
        (dotimes (i (length query-terms))
          (p-search--text-query-bm25 prior i results (nth i query-terms) tool))))))

(defun p-search--text-query-search-command (tool query case-insensitive)
  (pcase tool
    ('ag
     `("ag" "-c" "--nocolor" ,@(and case-insensitive '("-i")) ,query))
    ('rg
     `("rg" "--count-matches" "--color" "never" ,@(and case-insensitive '("-i")) ,query))))

(defun p-search--text-query-bm25 (prior i results query tool)
  "Perform BM25 query with QUERY-STRING and add result to RESULTS at index I.
If all results are populated, finalize the PRIOR results."
  (let* ((all-files (p-search-generate-search-space))
         (query-string (if (listp query) (car query) query))
         (case-i (and (listp query) (plist-get (cdr query) :case-insensitive)))
         (buf (generate-new-buffer (format "*bm25 %s*" query-string)))
         (cmd (p-search--text-query-search-command tool query-string case-i))
         (file-counts (make-hash-table :test #'equal))
         (file-scores (make-hash-table :test #'equal))
         (total-counts 0)
         (docs-containing 0)
         (k1 1.2)
         (b 0.75))
    (make-process
     :name "p-search-text-search"
     :buffer buf
     :command cmd
     :sentinel (lambda (proc event)
                 (when (or (member event '("finished\n" "deleted\n"))
                           (string-prefix-p "exited abnormally with code" event)
                           (string-prefix-p "failed with code" event))
                   (let ((start-time (current-time)))
                     (with-current-buffer (process-buffer proc)
                       (let* ((files (string-split (buffer-string) "\n")))
                         (dolist (f files)
                           (when (string-match "^\\(.*\\):\\([0-9]*\\)$" f)
                             (let* ((fname (match-string 1 f))
                                    (count (string-to-number (match-string 2 f))))
                               (puthash (file-name-concat default-directory fname) count file-counts)
                               (cl-incf docs-containing)
                               (cl-incf total-counts count))))))
                     (let* ((N (length all-files))
                            (all-files-size 0))
                       (dolist (file all-files)
                         (cl-incf all-files-size (nth 7 (file-attributes file))))
                       (let* ((idf (log (+ (/ (+ N (- docs-containing) 0.5)
                                              (+ docs-containing 0.5))
                                           1)))
                              (avg-size (/ (float all-files-size) N)))
                         (maphash
                          (lambda (file count)

                            (let* ((size (nth 7 (file-attributes file)))
                                   (score (* idf (/ (* count (+ k1 1))
                                                    (+ count (* k1 (+ 1 (- b) (* b (/ (float size) avg-size)))))))))
                              (puthash file score file-scores)))
                          file-counts)))
                     (message "BM25 complete: %s" (time-since start-time))
                     (setf (aref results i) file-scores)
                     (when (seq-every-p #'identity results)
                       (p-search--text-query-finalize-results prior results))))))))

(defun p-search--text-query-finalize-results (prior results)
  "Compute final calculations for PRIOR of RESULTS.
RESULTS is a vector of hash table of file scores."
  (let ((max-score 0)
        (min-score most-positive-fixnum)
        (probs (make-hash-table :test 'equal)))
    (dotimes (i (1- (length results)))
      (let* ((at (aref results i))
             (next (aref results (1+ i))))
        (maphash
         (lambda (file score)
           (let ((next-score (+ score (gethash file at 0))))
             (puthash file next-score next)))
         at)))
    (maphash
     (lambda (_ score)
       (cond
        ((> score max-score)
         (setq max-score score))
        ((< score min-score)
         (setq min-score score))))
     (aref results (1- (length results))))
    (let* ((extra (/ min-score 2.0))) ;; smoothing
      (maphash
       (lambda (file score)
         (puthash file (/ (+ (float score) extra)
                          (+ max-score extra))
                  probs))
       (aref results (1- (length results))))
      (setf (p-search-prior-results prior) probs)
      (setf (p-search-prior-default-result prior) (/ extra (+ max-score extra)))
      (p-search--notify-main-thread))))

(defun p-search--text-search-hint (prior buffer)
  "Mark places where the query args of PRIOR matches text in BUFFER."
  (let* ((search-regexps (p-search--text-query-all-search-items (p-search-prior-arguments prior) 'emacs))
         (ress '()))
    (with-current-buffer buffer
      (dolist (search-item search-regexps)
        (let* ((regexp (if (listp search-item) (car search-item) search-item))
               (case-fold-search (and (listp search-item) (plist-get (cdr search-item) :case-insensitive))))
          (while (search-forward-regexp regexp nil t)
            (push (cons (match-beginning 0) (match-end 0)) ress)))))
    ress))

(provide 'p-search-prior)
;;; p-search-prior.el ends here
