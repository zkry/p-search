;;; p-search.el --- Emacs Search Tool Aggregator -*- lexical-binding: t; -*-

;; Author: Zachary Romero
;; URL: https://github.com/zkry/p-search.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (f "0.20.0"))
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

;; P-SEARCH is an tool for combining and executing searches in Emacs.
;; The tool takes its inspiration from Bayesian search theory where it
;; is assumed that the thing being searched for has a prior
;; distribution of where it can be found, and that the act of looking
;; should update our posterior probability distribution.

;; Terminology: In p-search there are parts of the search, the prior
;; and likelihood.  The prior is specified via certain predicates that
;; reflect your beliefs where the file is located.  For example, you
;; could be 90% sure that a file is in a certain directory, and 10%
;; elsewhere.  Or you can be very sure that what you are looking for
;; will contain some form of a search term.  Or you may have belife
;; that the object you are looking for may have a more active Git log
;; than other files.  Or you think you remember seeing the file you
;; were looking for in one of your open buffers.  And so on.  The
;; important thing is that priors have 1) an objective criteria 2) a
;; subjective belief tied to the criteria.
;;
;; The second part of the equation is the likelihood.  When looking
;; for something, the very act of looking for something and not
;; finding doesn't mean that the its not there!  Think about when
;; looking for your keys.  You may check the same place several times
;; until you actually find them.  The act of observation reduces your
;; probability that the thing being looked for is there, but it
;; doesn't reduce it to zero.  When looking for something via
;; p-search, you mark the item with one of several gradations of
;; certainty that the element being looked for exists.  After
;; performing the observation, the probabilities where things exists
;; gets updated.

;;; Code:
(require 'heap)
(require 'cl-lib)

;;; Options

(defgroup p-search nil
  "Emacs Search Tool Aggregator."
  :group 'extensions)



;;; Priors


(defconst p-search-importance-levls
  '(low medium high critical))

(defvar p-search-prior-templates nil
  "List of avalable prior templates.")

(cl-defstruct (p-search-prior-template
               (:copier nil)
               (:constructor p-search-prior-template-create))
  "Structure representing a class of priors."
  name ; Name of prior, to be identified by the user
  input-function
  initialize-function ; Function to retrieve the necessary input from prior;
  add-function        ; Function if exists, prompts user to extend input
  base-prior-key      ; If non-nil, provide to all other priors for their use.
  default-result
  options-spec)

(cl-defstruct (p-search-prior
               (:copier nil)
               (:constructor p-search-prior-create))
  template    ; prior-template
  inputs
  importance
  results     ; hash table containing the result
  proc-thread
  options)

(defun p-search-expand-files (base-priors)
  "Return the list of files that should be considerd according to BASE-PRIORS."
  (let* ((included-directories (alist-get 'include-directories base-priors))
         (included-filenames (alist-get 'include-filename base-priors))
         (f-regexp (if included-filenames
                       (rx-to-string
                        `(or ,@(seq-map
                                (lambda (elt)
                                  (list 'regexp elt))
                                included-filenames)))
                       (regexp-opt included-filenames)
                     ".*")))
    ;; This is a very bare-bones implementation which can most likely be improved.
    ;; Overall though it's still fast: 76,000 results in 1.1 sec.
    ;; TOTO: Cache the results of BASE-PRIORS
    (seq-mapcat
     (lambda (dir)
       (directory-files-recursively dir f-regexp))
     included-directories)
    ;; what other types of things could we include? buffers? File
    ;; snippets?  how about stings are filenames and things like
    ;; '(buffer "*arst*") can be interpereted otherwise.
    ))

(defconst p-search--subdirectory-prior-template
  (p-search-prior-template-create
   :name "subdirectory"
   :input-function
   (lambda ()
     (list (f-canonical (read-directory-name "Directory: "))))
   :add-function
   (lambda (prev)
     (cons (f-canonical (read-directory-name "Directory: ")) prev))
   :base-prior-key 'include-directories)
  "Sample prior.")

(defconst p-search--filename-prior-template
  (p-search-prior-template-create
   :name "file-name"
   :input-function
   (lambda ()
     (list(read-regexp "Matching regexp: ")))
   :add-function
   (lambda (prev)
     (cons (read-regexp "Matching regexp: ") prev ))
   :base-prior-key 'include-filename
   ;; :initialize-function
   ;; (lambda (base-priors result-ht input-regexp-match) ;; superfelous
   ;;   (let* ((files (p-search-extract-files base-priors))) ;; normally should do async or lazily
   ;;     (dolist (file files)
   ;;       (puthash file (if (string-match-p input-regexp-match file) 'yes 'no)
   ;;                result-ht)))))))
   ))

(defconst p-search--textseach-prior-template
  (p-search-prior-template-create
   :name "text search"
   :input-function
   (lambda ()
     (read-string "String input: "))
   :options-spec
   '((tool . (choice (const :tag "ripgrep (rg)" rg) ;; TODO - how to specify defaults
                     (const :tag "The Silver Searcher (ag)" ag)
                     (const :tag "grep" grep)))
     (strategy . (choice (const :tag "exact, case-insensitive, word-break" (exact case-insensitive word-break)) ;; TODO - how to specify defaults
                         (const :tag "exact" (exact)))))
   :initialize-function
   (lambda (base-priors prior input)
     (let* ((default-directory (car (alist-get 'include-directories base-priors))) ;; TODO: allow for multiple
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

(defvar-local p-search-priors nil
  "List of active priors for current session.")

(defun p-search--extract-default-options (template)
  (let* ((options-spec (p-search-prior-template-options-spec template))
         (options '()))
    (pcase-dolist (`(,prop-name . ,spec) options-spec)
      (pcase spec
        (`(choice . ,choices)
         (push (cons prop-name
                     (last (car choices)))
               options))))
    (nreverse options)))

(defun p-search--instantiate-prior (template)
  "Create and return a prior according to TEMPLATE."
  (let* ((input-func (p-search-prior-template-input-function template))
         (input (and input-func (funcall input-func)))
         (init-func (p-search-prior-template-initialize-function template))
         (prior (p-search-prior-create
                 :template template
                 :inputs input
                 :results (make-hash-table :test 'equal)
                 :importance 'medium
                 :options (p-search--extract-default-options template)))
         (base-priors (p-search--base-priors-values))
         (init-res (funcall init-func base-priors prior input)))
    (setf (p-search-prior-proc-thread prior) init-res)
    prior))

(defun p-search--base-priors-values ()
  "Return list of base priors' values from current sessions priors."
  (let ((ret '()))
    (dolist (p p-search-priors)
      (let* ((template (p-search-prior-template p))
             (base-prior-key (p-search-prior-template-base-prior-key template)))
        (when base-prior-key
          (let* ((prev (alist-get base-prior-key ret))
                 (prior-input (p-search-prior-inputs p))
                 (next (append prior-input prev)))
            (assq-delete-all base-prior-key ret)
            (setq ret (cons (cons base-prior-key next) ret))))))
    ret))

(defun p-search--dependent-priors ()
  "Return list of priors that are not base-priors."
  (let ((ret '()))
    (dolist (p p-search-priors)
      (let* ((template (p-search-prior-template p))
             (base-prior-key (p-search-prior-template-base-prior-key template)))
        (unless base-prior-key
          (push p ret))))
    (nreverse ret)))

(defun p-search-modifier (result importance)
  "Combine RESULT and IMPORTANCE into a probability."
  (pcase (list result importance)
    (`(neutral ,_) 0.5)
    (`(,_ none) 0.5)
    ('(yes low) 0.6)
    ('(yes medium) 0.75)
    ('(yes high) 0.9)
    ('(yes critical) 1.0)
    ('(no low) 0.4)
    ('(no medium) 0.25)
    ('(no high) 0.1)
    ('(no critical) 0.0)
    (`(,p low) ; TODO: speed up betaI calculation
     (calc-eval (format "betaI(%f, 0.5, 0.5)" p)))
    (`(,p medium) p)
    (`(,p high)
     (calc-eval (format "betaI(%f, 3.0, 3.0)" p)))
    (`(,p critical)
     (calc-eval (format "betaI(%f, 100, 100)" p)))))

(defun p-search--calculate-probs ()
  "For all current priors, calculate element probabilities."
  (let* ((base-priors (p-search--base-priors-values))
         (files (p-search-expand-files base-priors))
         (dependent-priors (p-search--dependent-priors))
         (marginal-p 0.0)
         (res (make-heap (lambda (a b) (> (cadr a) (cadr b))) (length files))))
    (dolist (file files)
      (let* ((probability 1.0))
        (dolist (p dependent-priors)
          (let* ((prior-results (p-search-prior-results p))
                 (prior-template (p-search-prior-template p))
                 (default-result (p-search-prior-template-default-result prior-template))
                 (prior-importance (p-search-prior-importance p))
                 (file-result (gethash file prior-results (or default-result 'neutral))) ;; TODO indecies?
                 (modifier (p-search-modifier file-result prior-importance)))
            (setq probability (* probability modifier))))
        (heap-add res (list file probability))
        (cl-incf marginal-p probability)))
    res))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p-search mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar p-search-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (keymap-set map "a" #'p-search-add-prior)
    (keymap-set map "g" #'p-search-refresh-buffer)
    map))

(defun p-search-highlight-point-section ()
  (let* ((ovs (overlays-in (point-min) (point-max))))
    (dolist (ov ovs)
      (overlay-put ov 'face nil)))
  (let* ((ovs (overlays-at (point)))
         (max-ov nil)
         (max-section -1))
    (dolist (ov ovs)
      (let* ((section (overlay-get ov 'p-search-section-level)))
        (when (> section max-section)
          (setq max-ov ov)
          (setq max-section section))))
    (when max-ov
      (overlay-put max-ov 'face 'highlight))))

(defun p-search-pre-command-hook ()
  (p-search-highlight-point-section))

(defun p-search-post-command-hook ()
  (p-search-highlight-point-section))

(define-derived-mode p-search-mode special-mode "p-search"
  ""
  :group 'p-search
  (hack-dir-local-variables-non-file-buffer)
  (add-hook 'pre-command-hook #'p-search-pre-command-hook nil t)
  (add-hook 'post-command-hook #'p-search-post-command-hook t t)
  ;; (setq-local revert-buffer-function #'magit-refresh-buffer)
  ;; (setq-local bookmark-make-record-function #'magit--make-bookmark)
  ;; (setq-local imenu-create-index-function #'magit--imenu-create-index)
  ;; (setq-local isearch-filter-predicate #'magit-section--open-temporarily)
  )

;;;;;;;;;;;;;;;;;;;
;;; prior init ;;;;

(defun p-search-setup-priors ()
  "Set up the buffers pre-existing priors."
  (let* ((project-root (expand-file-name (project-root (project-current)))) ;; TODO - make this an opt
         (initial-priors (list (p-search-prior-create
                                :template p-search--subdirectory-prior-template
                                :importance 'critical
                                :inputs `(,project-root)))))
    (setq p-search-priors initial-priors)))

(defun p-search-user-setup-prior (template)
  "Return an instantiated prior from TEMPLATE and users input."
  )

;;;;;;;;;;;;;;;;;
;;; display ;;;;;

(defvar p-search--section-level 0)

(defun p-search-add-section-overlay (start end)
  "Add overlay to indicate collapsible section from START to END."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'p-search-section-level p-search--section-level)))

(defmacro p-search-add-section (section-name &rest body)
  (declare (indent 1))
  (let ((start (make-symbol "start"))
        (end (make-symbol "end")))
    `(let ((,start (point))
           (p-search--section-level (1+ p-search--section-level)))
       (insert ,section-name)
       (when (not (= (char-after (1- (point))) ?\n))
         (insert "\n"))
       ,@body
       (let ((,end (point)))
         (p-search-add-section-overlay ,start ,end)))))

(defun p-search-format-inputs (inputs)
  "Apply formatings to INPUTS to display to end user."
  (cond
   ((listp inputs)
    (seq-map #'p-search-format-inputs inputs))
   ((stringp inputs)
    (let ((home-dir (expand-file-name "~")))
      (replace-regexp-in-string (regexp-quote home-dir) "~" inputs)))
   (t
    inputs)))

(defun p-search-insert-prior (prior)
  (let* ((start (point))
         (template (p-search-prior-template prior))
         (template-name (p-search-prior-template-name template))
         (inputs (p-search-prior-inputs prior))
         (importance (p-search-prior-importance prior))
         (importance-char (alist-get
                           importance
                           '((critical . "!")
                             (high . "H")
                             (medium . "M")
                             (low . "L")
                             (none . "-"))))
         (options (p-search-prior-options prior))) ;; TODO - calculate status
    (p-search-add-section ""
      (insert importance-char " " (propertize template-name 'face 'magit-header-line-key))
      (insert (make-string (- 16 (current-column)) ?\s)
              (propertize (prin1-to-string (p-search-format-inputs inputs)) 'face 'magit-dimmed) "\n")
      (pcase-dolist (`(,name . ,val) options)
        (insert "  " (symbol-name name) (prin1-to-string val) "\n")))))

(defun p-search-refresh-buffer ()
  "Refresh the current p-search buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (at-line (line-number-at-pos)))
    (erase-buffer)
    (p-search-add-section (propertize "Priors" 'face 'magit-header-line)
      (dolist (prior p-search-priors)
        (p-search-insert-prior prior)))
    (goto-char (point-min))
    (forward-line (1- at-line))))

;;;;;;;;;;;;;;;;;;;;
;;; mode helpers ;;;

(defun p-search-setup-buffer ()
  ""
  (let* ((buffer (generate-new-buffer "p-search"))) ;; TODO - more sophisticated naming
    (with-current-buffer buffer
      (p-search-mode))
    (display-buffer buffer nil)
    (with-current-buffer buffer
      (p-search-setup-priors)
      (p-search-refresh-buffer))
    buffer)) ;; TODO - more sophisticated display mechanisms

;;;;;;;;;;;;;;;;
;;; commands ;;;

(defvar p-search-available-prior-templates
  (list p-search--textseach-prior-template
        p-search--filename-prior-template
        p-search--subdirectory-prior-template))

(defun p-search-add-section ()
  "Command to add prior to current p-search session."
  (interactive)
  (let* ((names (seq-map #'p-search-prior-template-name p-search-available-prior-templates))
         (selection (completing-read "Prior: " names nil t))
         (template (seq-find (lambda (template)
                               (string= selection (p-search-prior-template-name template)))
                             p-search-available-prior-templates)))

    template))


(defun p-search ()
  ""
  (interactive)
  (p-search-setup-buffer))




;; Initial priors
;; (setq p-search-priors (list (p-search-prior-create
;;                              :template p-search--subdirectory-prior-template
;;                              :inputs '("/Users/zromero/Downloads"))
;;                             (p-search-prior-create
;;                              :template p-search--filename-prior-template
;;                              :inputs '("\\.go$"))))

;; (defconst my-text-search (p-search--instantiate-prior
;;                           p-search--textseach-prior-template))

;; (defconst my-text-search2 (p-search--instantiate-prior
;;                            p-search--textseach-prior-template))

;; (p-search-prior-inputs my-text-search2)

;; (hash-table-keys (p-search-prior-results my-text-search2))

;; (setq p-search-priors (append p-search-priors (list my-text-search2)))

;; (heap-root (p-search--calculate-probs))
;; (let ((probs (p-search--calculate-probs)))
;;   (defconst mytestdata (list (heap-delete-root probs)
;;                              (heap-delete-root probs)
;;                              (heap-delete-root probs)
;;                              (heap-delete-root probs))))


(provide 'p-search)
;;; p-search.el ends here
