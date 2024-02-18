;;; p-search-prior.el --- Implementation of search priors  -*- lexical-binding:t -*-

;;; Commentary:

;; This package implements various search priors to use in a p-search session.

;;; Code:

;;; Reader Functions

;; ToDo List:
;; File System:
;; - [x] f n Filename
;; - [x] f d Directory
;; - [x] f t File Type
;; - [x] f m Modification Date
;; - [x] f s Size
;; - [ ] f c Distance
;;
;; Git:
;; - [x] g a Author
;; - [x] g b Branch
;; - [ ] g c File Co-Changes
;; - [ ] g m Modification Frequency
;; - [ ] g t Commit Time
;;
;; Vector:
;; - [ ] v d Vector Distance
;;
;; Emacs
;; - [ ] e b Open Buffer
;;
;; Source
;; - [ ] s t Text Match
;; - [ ] s c Co-located text match
;; - [ ] s f Text Frequency
;;
;; Source (Regexp)
;; - [ ] r t Regexp Text Match
;; - [ ] r c co-located regexp text match
;; - [ ] r f regexp frequency

(require 'subr-x)

(defun p-search-prior-default-arguments (template)
  "Return default input and options of TEMPLATE as one alist.
This function is primarily used to create the base prior with reasonable
default inputs, with the args being set to nil."
  (let* ((input-spec (oref template input-spec))
         (options-spec (oref template options-spec))
         (res '()))
    (pcase-dolist (`(,name . (,type . ,options)) input-spec)
      (let* ((default (plist-get options :default))
             (default-val (if (functionp default) (funcall default) default)))
        (setq res (cons
                   (cons name default-val)
                   res))))
    (pcase-dolist (`(,name . (,type . ,options)) options-spec)
      (let* ((default (plist-get options :default))
             (default-val (if (functionp default) (funcall default) default)))
        (setq res (cons
                   (cons name default-val)
                   res))))
    (nreverse res)))

;;;; Reference Priors:

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
                                   (string-split (shell-command-to-string "git ls-files") "\n")
                                 (string-split (shell-command-to-string "find . -type f") "\n")))
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
   (lambda (prior base-prior-args args)
     (let* ((files (p-search-generate-search-space))
            (directories (seq-map #'expand-file-name (alist-get 'include-directories args)))
            (result-ht (p-search-prior-results prior)))
       (dolist (file files)
         (catch 'out
           (let* ((file-expanded (expand-file-name file)))
             (dolist (dir directories)
               (when (string-prefix-p dir file-expanded)
                 (puthash file 'yes result-ht)
                 (throw 'out nil))))))
       (p-search--notify-main-thread))))
  "Sample prior.")


(defconst p-search--filename-prior-template
  (p-search-prior-template-create
   :name "file-name"
   :input-spec '((include-filename . (regexp
                                      :key "i"
                                      :description "Pattern")))
   :initialize-function
   (lambda (prior base-prior-args args) ;; TODO - remove base-prior-args as it can be obtained from base prior and args for that mattr
     (let* ((files (p-search-generate-search-space))
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
   (lambda (prior base-prior-args args)
     (let* ((files (p-search-generate-search-space))
            (ext-suffix (alist-get 'base-prior-args args))
            (result-ht (p-search-prior-results prior)))
       (dolist (file files)
         (puthash file (if (string-suffix-p ext-suffix file) 'yes 'no) result-ht)))
     (p-search--notify-main-thread))))

(defconst p-search--timestamp-high-to-days 0.758518518521)

(defun p-search--normal-pdf (x mu sigma)
  (let ((x (float x)) ;; TODO - this needs to be fast!
        (mu (float mu))
        (sigma (float sigma)))
    (/ (exp (* -0.5 (expt (/ (- x mu) sigma) 2.0)))
       (* sigma (sqrt (* 2 pi))))))

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
   (lambda (prior base-prior-args args)
     (let* ((files (p-search-generate-search-space))
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
   (lambda (prior base-prior-args args)
     (let* ((files (p-search-generate-search-space))
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

(defun p-search--textsearch-prior-template-init (prior base-prior-args args)
  (let* ((input (alist-get 'search-term args))
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
     :sentinel (lambda (proc event)
                 (when (or (member event '("finished\n" "deleted\n"))
                           (string-prefix-p "exited abnormally with code" event)
                           (string-prefix-p "failed with code"))
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
  (let* ((base-args (oref p-search-base-prior arguments))
         (default-directory (alist-get 'base-directory base-args)))
    (string-lines (shell-command-to-string "git log --all --format='%aN' | sort -u") t)))

(defun p-seach--git-branches ()
  (let* ((base-args (oref p-search-base-prior arguments))
         (default-directory (alist-get 'base-directory base-args)))
    (seq-map
     (lambda (line)
       (string-trim-left line "[ *]*"))
     (string-lines (shell-command-to-string "git branch  -a") t))))

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

(defun p-search--git-author-prior-template-init (prior base-prior-args args)
  (let* ((author (alist-get 'git-author args))
         (default-directory (alist-get 'base-directory base-prior-args))
         (buf (generate-new-buffer "*p-search-git-author-search*"))
         (git-command (format "git log --author=\"%s\" --name-only --pretty=format: | sort -u" author)))
    (make-process
     :name "p-seach-git-author-prior"
     :buffer buf
     :command `("sh" "-c" ,git-command)
     :sentinel (lambda (proc event)
                 (when (or (member event '("finished\n" "deleted\n"))
                           (string-prefix-p "exited abnormally with code" event)
                           (string-prefix-p "failed with code"))
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
     :sentinel (lambda (proc event)
                 (when (or (member event '("finished\n" "deleted\n"))
                           (string-prefix-p "exited abnormally with code" event)
                           (string-prefix-p "failed with code"))
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
   :default-result 'no))

(defun p-search--git-co-changes-prior-template-init (prior base-prior-args args)
  ""
  (let* ((use-edited-files (alist-get 'use-edited-files args))
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
        (message "δ git command: %s" git-command)
        (make-process
         :name "p-seach-git-cochanges-prior"
         :buffer buf
         :command `("sh" "-c" ,git-command)
         :sentinel (lambda (proc event)
                     (when (equal event "finished\n")
                       (with-current-buffer (process-buffer proc)
                         (let* ((result-ht (p-search-prior-results prior))
                                (file-counts (make-hash-table))
                                (N 0)
                                (commit-hashes (string-lines (buffer-string) t)))
                           (message "δ got %d commit hashes" (length commit-hashes))
                           (dolist (hash commit-hashes)
                             (let* ((changed-files
                                     (thread-first "git show --pretty=format:\"\" --name-only %s"
                                                   (format hash)
                                                   (shell-command-to-string)
                                                   (string-lines t))))
                               (message "δ   got %d lines" (length changed-files))
                               (cl-incf N (length changed-files))
                               (dolist (file changed-files)
                                 (puthash file (1+ (gethash file file-counts 0)) file-counts))))
                           (message "δ total: %d" (hash-table-count file-counts ))
                           (maphash
                            (lambda (file count)
                              (let* ((p (/ (float count) N)))
                                (puthash (file-name-concat default-directory file) p result-ht)))
                            file-counts)))
                       (p-search--notify-main-thread))))))))

;;; p-search-prior.el ends here
