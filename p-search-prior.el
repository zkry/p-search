;;; p-search-prior.el --- Implementation of search priors  -*- lexical-binding:t -*-

;;; Commentary:

;; This package implements various search priors to use in a p-search session.

;;; Code:

;;; Reader Functions

;; ToDo List:
;; File System:
;; - [ ] f n File Name
;; - [ ] f d Directory
;; - [ ] f t File Type
;; - [ ] f m Modification Date
;; - [ ] f s Size
;; - [ ] f c Distance
;;
;; Git:
;; - [ ] g a Author
;; - [ ] g b Branch
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


;;;; Reference Priors:

(defconst p-search-prior-base--filesystem-
  (p-seach-prior-template-create
   :name "FILESYSTEM"
   :base-prior-key t
   :input-spec '((include-directories . (directory-names
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
                   (use-git-ignore . (switch
                                      :key "-g"
                                      :description "Git Ignore"
                                      :default t)))
   ))


(defconst p-search--subdirectory-prior-template
  (p-search-prior-template-create
   :name "subdirectory"
   :base-prior-key 'include-directories
   :input-spec '((include-directories . (directory-names
                                         :key "i"
                                         :description "Directories"))))
  "Sample prior.")


(defconst p-search--filename-prior-template
  (p-search-prior-template-create
   :name "file-name"
   :base-prior-key 'include-filename
   :input-spec '((include-filename . (regexp
                                      :key "i"
                                      :description "Pattern")))
   ;; :initialize-function
   ;; (lambda (base-priors result-ht input-regexp-match) ;; superfelous
   ;;   (let* ((files (p-search-extract-files base-priors))) ;; normally should do async or lazily
   ;;     (dolist (file files)
   ;;       (puthash file (if (string-match-p input-regexp-match file) 'yes 'no)
   ;;                result-ht)))))))
   ))

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
   :initialize-function
   (lambda (prior base-priors args)
     (let* ((input (alist-get 'search-term args))
            (default-directory (car (alist-get 'include-directories base-priors))) ;; TODO: allow for multiple
            (file-args (alist-get 'include-filename base-priors))
            (ag-file-regex (and file-args (concat "(" (string-join file-args "|") ")")))
            (cmd `("ag" ,input "-l" "--nocolor"))
            (buf (generate-new-buffer "*pcase-text-search*")))
       (when ag-file-regex
         (setq cmd (append cmd `("-G" "\\.go$"))))
       (make-process
        :name "p-search-text-search-prior"
        :buffer buf
        :command cmd
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
   :default-result 'no))

;;; p-search-prior.el ends here
