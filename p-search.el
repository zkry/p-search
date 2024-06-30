;;; p-search.el --- Emacs Search Tool Aggregator -*- lexical-binding: t; -*-

;; Author: Zachary Romero
;; URL: https://github.com/zkry/p-search.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
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

;; TODO - consider name change: b-search, b-find, baysrch, prb-seek,

;;; Code:
(require 'heap)
(require 'cl-lib)

(cl-defstruct (p-search-prior-template
               (:copier nil)
               (:constructor p-search-prior-template-create))
  "Structure representing a class of priors."
  (name nil :documentation "Name of prior, to be identified by the user")
  (initialize-function nil :documentation "Function to populate prior results.  Called with three arguments: prior, base-priors, and args.")
  (base-prior-key nil :documentation "Argument name that, when given a critical importance, is used to determine the base file listing, as well as given to other priors to optimize their performance.") ;; deprecated
  (default-result nil :documentation "Result that should be returned if no file is specified.")
  (input-spec nil :documentation "Specification of inputs required for the function to function.")
  (options-spec nil :documentation "Specification of parameters which alter the operation of the prior.")
  (search-space-function nil :documentation "Function that when called returns a list of items to be the seach space.  This function existing determines if a prior is a \"base prior\".")
  (result-hint-function nil :documentation "Optional function that takes the result in a buffer and returns ranges of significance.")
  (add-prior-function nil :documentation "Function for base priors that dispatches the add-prior transient."))

(cl-defstruct (p-search-prior
               (:copier nil)
               (:constructor p-search-prior-create))
  "An instantiated prior created from a template which is actively
providing information to a search.in "
  (template nil :type p-search-prior-template)
  (importance nil :documentation "How much the prior should influence results.") ;; TODO - identiy where values come from
  (results nil :documentation "hash table containing the result.  Maps from file name to result indicator.")
  (proc-thread nil :documentation "This slot stores the process or thread that does main computation.")
  (arguments nil :documentation "Arguments provided to the prior.  These are the union of inputs and options.")
  (default-result nil :documentation "Override of the tempate's default result."))

(require 'p-search-transient)
(require 'p-search-prior)

;;; Options

(defgroup p-search nil
  "Emacs Search Tool Aggregator."
  :group 'extensions)


;;; Faces

(defgroup p-search-faces nil
  "Faces used by p-saerch."
  :group 'p-search
  :group 'faces)

(defface p-search-section-highlight
  `((((class color) (background light))
     :extend t
     :background "grey95")
    (((class color) (background  dark))
     :extend t
     :background "grey20"))
  "Face for highlighting the current section."
  :group 'p-search-faces)

(defface p-search-section-heading
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DarkGoldenrod4"
     :weight bold)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "LightGoldenrod2"
     :weight bold))
  "Face for section headings."
  :group 'p-search-faces)

(defface p-search-header-line-key
  '((t :inherit font-lock-builtin-face))
  "Face for keys in the `header-line'."
  :group 'p-search-faces)

(defface p-search-value
  '((t :inherit transient-value))
  "Face for keys in the `header-line'."
  :group 'p-search-faces)

(defface p-search-hi-yellow
  '((((min-colors 88) (background dark))
     (:weight bold :box (:line-width 1 :color "yellow1" :style nil)))
    (((background dark)) (:weight bold :box (:line-width 1 :color "yellow" :style nil)))
    (((min-colors 88)) (:weight bold :box (:line-width 1 :color "yellow1" :style nil)))
    (t (:weight bold :box (:line-width 1 :color "yellow" :style nil))))
  "Face for highlighting in p-search mode with bold text and a box."
  :group 'p-search-faces)



;;; Documents

;; A document is an atomic unit of search.  In p-search, documents are
;; identified with an S-expression which contains the data needed to
;; retrieve the document's text.

(defvar p-search-document-types
  '((buffer . p-search-document--buffer-text)
    (file . p-search-document--file-text)
    (section . p-search-document--section-text)))

(defun p-search-document--buffer-text (buffer-name)
  (if (buffer-live-p buffer-name)
    (with-current-buffer buffer-name
      (buffer-substring-no-properties (point-min) (point-max)))
    "<killed buffer>"))

(defun p-search-document--file-text (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun p-search-document--section-text (child start end)
  (substring (p-search-document-text child) start end))

(defun p-search-document-text (document)
  (let* ((sym (car document))
         (dispatch-func (alist-get sym p-search-document-types)))
    (unless dispatch-func
      (error "Invalid document type %s" sym))
    (apply dispatch-func (cdr document))))

(defun p-search-document-size (document)
  (pcase document
    (`(section ,_ ,start ,end) ;; TODO should this be here?
     (- end start))
    (`(buffer ,buffer)
     (buffer-size buffer))
    (`(file ,file-name)
     (nth 7 (file-attributes file-name)))
    (_
     (length (p-search-document-text document)))))

(defun p-search-document-title (document)
  (pcase document
    (`(section ,child ,start ,end) ;; TODO should this be here?
     (let ((child-title (p-search-document-title child)))
       (format "%s[%d,%d]" child-title start end)))
    (`(buffer ,buffer)
     (buffer-name buffer))
    (`(file ,file-name)
     file-name)
    (_ (format "%s" document))))



;;; Priors

(defvar p-search-base-prior)

(defconst p-search-available-base-prior-templates
  (list p-search-prior-base--buffers
        p-search-prior-base--filesystem
        p-search-prior-base--multi-filesystem)
  "Alist of available priors to use as a base.")

(defvar p-search-default-base-prior-template
  p-search-prior-base--filesystem
  "Prior template to use for initial base.")


(defconst p-search-importance-levls
  '(low medium high critical))

(defvar p-search-prior-templates nil
  "List of avalable prior templates.")

(let ((prior (p-search-prior-create)))
  (setf (p-search-prior-results prior) 100))

(defun p-search-base-prior-p (prior)
  "Return non-nil if PRIOR is a base prior."
  (p-search-prior-template-search-space-function (p-search-prior-template prior)))

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

(defconst p-search-search-space-cache (make-hash-table :test #'equal)) ;; TODO - Allow invalidation (like with command G)

(defun p-search-generate-search-space () ;; new version of p-search-expand-files
  "Generate all search candidates from `p-search-base-prior'."
  (when p-search-base-prior
    (let* ((args (oref p-search-base-prior arguments))
           (template (oref p-search-base-prior template))
           (search-space-func (oref template search-space-function))
           (search-space-key (list (type-of p-search-base-prior) args)))
      (if-let* ((res (gethash search-space-key p-search-search-space-cache)))
          res
        (unless search-space-func
          (error "Base prior has no search-space-function"))
        (let* ((res (funcall search-space-func args)))
          (puthash search-space-key res p-search-search-space-cache)
          res)))))

(defvar-local p-search-priors nil
  "List of active priors for current session.")
(defvar-local p-search-marginal nil
  "Marginal distribution of search space.
This is the denominator we need to divide all probabilities by to get a proper
probability distribution.")
(defvar-local p-search-base-prior nil
  "The prior which is used to determine search space.
This prior has a special function in that its
`search-space-function' is called which returns all of the
elements to search over.")

(defun p-search--extract-default-options (template)
  "Return the default option values of TEMPLATE if they exist."
  (let* ((options-spec (p-search-prior-template-options-spec template))
         (options '()))
    (pcase-dolist (`(,prop-name . ,spec) options-spec)
      (pcase spec
        (`(choice . ,choices)
         (push (cons prop-name
                     (car (last (car choices))))
               options))))
    (nreverse options)))

(defun p-search--instantiate-prior (template &optional args)
  "Create and return a prior according to TEMPLATE with ARGS."
  (let* ((init-func (p-search-prior-template-initialize-function template))
         (importance (alist-get 'importance args))
         (prior (p-search-prior-create
                 :template template
                 :arguments args
                 :results (make-hash-table :test 'equal)
                 :importance (or importance 'medium)))
         (base-priors (oref p-search-base-prior arguments))  ;; TODO - rename
         (init-res (funcall init-func prior)))
    (setf (p-search-prior-proc-thread prior) init-res)
    prior))

(defun p-search--validate-prior (prior args)
  "Throw an error if PRIOR is defined improperly with ARGS."
  (let* ((template (p-search-prior-template prior))
         (input-spec (p-search-prior-template-input-spec template)))
    (pcase-dolist (`(,id . _) input-spec)
      (unless (alist-get id args)
        (user-error "Input value `%s' not defined" id )))))

(defun p-search--rerun-prior (prior)
  "Stop currently running PRIOR and re-runs it."
  (let* ((old-thread|proc (p-search-prior-proc-thread prior))
         (template (p-search-prior-template prior))
         (init-func (p-search-prior-template-initialize-function template))
         (args (p-search-prior-arguments prior))
         (base-priors (oref p-search-base-prior arguments)))
    (setf (p-search-prior-results prior) (make-hash-table :test #'equal))
    (let ((init-res (and init-func (funcall init-func prior))))
      (when (threadp old-thread|proc)
        (thread-signal old-thread|proc 'stop nil))
      (when (and (processp old-thread|proc) (eql (process-status old-thread|proc) 'run))
        (kill-process old-thread|proc))
      (setf (p-search-prior-proc-thread prior) init-res)
      prior)))

(defun p-search--base-priors-values ()
  "Return list of base priors' values from current sessions priors."
  (let ((ret '()))
    (dolist (p p-search-priors)
      (let* ((template (p-search-prior-template p))
             (base-prior-key (p-search-prior-template-base-prior-key template)))
        (when base-prior-key
          (let* ((prev (alist-get base-prior-key ret))
                 (args (p-search-prior-arguments p))
                 (key (p-search-prior-template-base-prior-key template))
                 (prior-input (alist-get key args))
                 (next (append prior-input prev)))
            (assq-delete-all base-prior-key ret)
            (setq ret (cons (cons base-prior-key next) ret))))))
    ret))

(defconst p-search-importance-levels
  '(none low medium high critical))

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
     ;;(read (calc-eval (format "betaI(%f, 0.5, 0.5)" p)))
     p) ;; TODO - don't use calc-eval
    (`(,p medium) p)
    (`(,p high)
     ;; (read (calc-eval (format "betaI(%f, 3.0, 3.0)" p)))
     p)
    (`(,p critical)
     ;;(read (calc-eval (format "betaI(%f, 100, 100)" p)))
     p)))

(defvar-local p-search-posterior-probs nil
  "Heap of calculated posterior probabilities.
Elements are of the type (FILE PROB).")

(defun p-search--create-heap (&optional size)
  (make-heap (lambda (a b) (if (= (cadr a) (cadr b))
                               (string> (format "%s" (car a)) (format "%s" (car b)))
                             (> (cadr a) (cadr b))))
             size))

(defun p-search--calculate-probs ()
  "For all current priors, calculate element probabilities."
  (message "--- p-search--calculate-probs")
  (let* ((old-heap p-search-posterior-probs)
         (files (p-search-generate-search-space))
         (dependent-priors p-search-priors)
         (marginal-p 0.0)
         (res (p-search--create-heap (length files)))
         (start-time (current-time)))
    (setq p-search-posterior-probs res)
    (dolist (file files)
      (let* ((probability 1.0))
        (dolist (p dependent-priors)
          (let* ((prior-results (p-search-prior-results p))
                 (prior-template (p-search-prior-template p))
                 (default-result (or (gethash :default prior-results)
                                     (p-search-prior-default-result p)
                                     (p-search-prior-template-default-result prior-template)))
                 (prior-importance (alist-get 'importance  (p-search-prior-arguments p) 'medium)) ;; TODO - Default?
                 (complement (alist-get 'complement  (p-search-prior-arguments p)))
                 (file-result (gethash file prior-results (or default-result 'neutral))) ;; TODO indecies?
                 (modifier (p-search-modifier file-result prior-importance)))
            (when complement
              (setq modifier (- 1 modifier)))
            (setq probability (* probability modifier))))
        (setq probability (* probability (gethash file p-search-observations 1.0)))
        (heap-add res (list file probability))
        (cl-incf marginal-p probability))
      (pcase-let* ((`(,_ ,sec ,ms) (time-since start-time)))
        (when (> (+ (* 1000 sec) ms) 300000)
          (with-temp-buffer
            (thread-yield))
          (setq start-time (current-time)))))
    (setq p-search-marginal marginal-p)
    (message "--- p-search--calculate-probs done")
    res))

(defun p-search-list-all-docs ()
  "Return list of all docs contained in `p-search-posterior-probs' in order of probability."
  (let ((elts '())
        (copy (p-search--create-heap (heap-size p-search-posterior-probs))))
    (while (not (heap-empty p-search-posterior-probs))
      (pcase-let* ((`(,doc ,p) (heap-delete-root p-search-posterior-probs)))
        (push (list doc (/ p p-search-marginal)) elts)
        (heap-add copy (list doc p))))
    (setq p-search-posterior-probs copy)
    (setq elts (nreverse elts))
    elts))


;;; Posteriors

(defvar-local p-search-observations nil
  "Hash table of observiations.")

(defun p-search-obs-not-relevant ()
  "Mark the result under the point as not-relevant."
  (interactive)
  (let* ((document (p-search--document-at-point)))  ;; TODO - use terminology "document" instead of "file"
    (unless document
      (user-error "No document found at point"))
    (puthash document (* (gethash document p-search-observations 1) 0.3) p-search-observations)
    (p-search--notify-main-thread)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p-search mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local p-search-main-worker-thread nil
  "Thread to do the calculations.  Currently not used.")

(defvar p-search-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (keymap-set map "a" #'p-search-add-prior)
    (keymap-set map "e" #'p-search-edit)
    (keymap-set map "g" #'p-search-refresh-buffer)
    (keymap-set map "i" #'p-search-importance)
    (keymap-set map "k" #'p-search-kill-prior)
    (keymap-set map "n" #'p-search-obs-not-relevant)
    (keymap-set map "r" #'p-search-reinstantiate-prior)
    (keymap-set map "<tab>" #'p-search-toggle-section)
    (keymap-set map "<return>" #'p-search-find-file)
    (keymap-set map "C-o" #'p-search-display-file)
    (keymap-set map "1" #'p-search-show-level-1)
    (keymap-set map "2" #'p-search-show-level-2)
    (keymap-set map "3" #'p-search-show-level-3)
    map)
  "Mode-map for p-search-mode.")

(defun p-search-pre-command-hook ()
  "Pre-command-hook for p-search mode."
  ;; (p-search-highlight-point-section)
  )

(defun p-search-post-command-hook ()
  "Post-command-hook for p-search mode."
  (p-search-highlight-point-section))

(defun p-search-kill-hook ()
  "Cleanup resources started by the p-search session."
  (when (timerp p-search-display-timer)
    (cancel-timer p-search-display-timer))
  (when (threadp p-search-main-worker-thread)
    (thread-signal p-search-main-worker-thread 'exit nil))
  (let* ((log-buf (get-buffer (format "logs: %s" (buffer-name (current-buffer))))))
    (when log-buf
      (kill-buffer log-buf))))

(define-derived-mode p-search-mode special-mode "p-search"
  "Major mode for p-search."
  :group 'p-search
  (hack-dir-local-variables-non-file-buffer)
  (add-hook 'pre-command-hook #'p-search-pre-command-hook nil t)
  (add-hook 'post-command-hook #'p-search-post-command-hook t t)
  (add-hook 'kill-buffer-hook #'p-search-kill-hook nil t)
  (setq p-search-observations (make-hash-table :test 'equal))
  (let ((buf (current-buffer)))
    (setq p-search-display-timer
          (run-with-timer 1 1
                          (lambda ()
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (p-search-display-function)))))))
  ;; (setq-local revert-buffer-function #'magit-refresh-buffer)
  ;; (setq-local bookmark-make-record-function #'magit--make-bookmark)
  ;; (setq-local imenu-create-index-function #'magit--imenu-create-index)
  ;; (setq-local isearch-filter-predicate #'magit-section--open-temporarily)
  )

;;;;;;;;;;;;;;;;;
;;; logging  ;;;;

(defun p-search-log (format-string &rest args)
  "Log formatted FORMAT-STRING with ARGS to log buffer."
  (let* ((message (apply #'format (cons format-string args)))
         (ts (format-time-string "%H:%M:%S %3N"))
         (buf-name (format "logs: %s" (buffer-name (current-buffer)))))
    (with-current-buffer (get-buffer-create buf-name)
      (save-excursion
        (goto-char (point-max))
        (insert "\n" ts "\t" message)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prior initialization ;;;;

(defun p-search-setup-priors (&optional prior-template)
  "Set up the buffers pre-existing priors."
  (setq p-search-priors nil)
  (let ((template (or prior-template p-search-default-base-prior-template)))
    (setq p-search-base-prior (p-search-prior-create
                               :template template
                               :arguments (p-search-prior-default-arguments template)))))

;;;;;;;;;;;;;;;;;
;;; display ;;;;;

(defvar-local p-search--section-level 0
  "Variable used to determine leveling of nested sections.")

(defun p-search-highlight-point-section ()
  "Put a highlight property on section overlay at point."
  (let* ((ovs (overlays-in (point-min) (point-max))))
    (dolist (ov ovs)
      (overlay-put ov 'face nil)))
  (let* ((ovs (overlays-at (point)))
         (max-ov nil)
         (max-section -1))
    (dolist (ov ovs)
      (let* ((section (overlay-get ov 'p-search-section-level)))
        (when (and section (> section max-section))
          (setq max-ov ov)
          (setq max-section section))))
    (when max-ov
      (overlay-put max-ov 'face 'p-search-section-highlight))))

(defun p-search-add-section-overlay (start end &optional props key)
  "Add overlay to indicate collapsible section from START to END.
PROPS are additional properties to put on the overlay.  KEY is the
value of the overlay property p-search-key."
  (let ((ov (make-overlay start end)))
    (when key
      (overlay-put ov 'p-search-key key))
    (overlay-put ov 'p-search-section-level p-search--section-level)
    (overlay-put ov 'before-string
                   (propertize " " 'display '(left-fringe magit-fringe-bitmapv)))
    (while props
      (let ((k (car props))
            (v (cadr props)))
        (overlay-put ov k v)
        (setq props (cddr props))))))

(defmacro p-search-add-section (section-name &rest body)
  "Insert a collapsable section at the point with heading SECTION-NAME.
BODY should then insert the contents of the collapsible section, making
sure to end with a newline.  The section then spans from the start of
the heading to the point where BODY leaves off."
  (declare (indent 1))
  (let ((start (make-symbol "start"))
        (end (make-symbol "end"))
        (props (make-symbol "props"))
        (key (make-symbol "key")))
    `(let ((,start (point))
           (p-search--section-level (1+ p-search--section-level))
           (,props (and (not (stringp ,section-name))
                        (alist-get 'props ,section-name nil)))
           (,key (and (not (stringp ,section-name))
                      (alist-get 'key ,section-name nil))))
       (if (stringp ,section-name)
           (insert ,section-name)
         (insert (alist-get 'heading ,section-name)))
       (when (not (= (char-after (1- (point))) ?\n))
         (insert "\n"))
       ,@body
       (let ((,end (point)))
         (p-search-add-section-overlay ,start ,end ,props
                                       (or ,key (and (stringp ,section-name)
                                                     ,section-name)))))))

(defun p-search-format-inputs (inputs)
  "Apply formatings to INPUTS to display to end user."
  (cond
   ((listp inputs)
    (seq-map #'p-search-format-inputs inputs))
   ((stringp inputs)
    (let ((home-dir (expand-file-name "~")))
      (replace-regexp-in-string (regexp-quote home-dir) "~" inputs)))
   (t inputs)))

(defun p-search-insert-prior (prior)
  "Insert text for PRIOR at point."
  (let* ((start (point))
         (template (p-search-prior-template prior))
         (template-name (p-search-prior-template-name template))
         (importance (alist-get 'importance (p-search-prior-arguments prior)))
         (importance-char (alist-get
                           importance
                           '((critical . "!")
                             (high . "H")
                             (medium . "M")
                             (low . "L")
                             (none . "-"))))
         (options (p-search-prior-arguments prior))) ;; TODO - calculate status
    (p-search-add-section `((heading . "")
                            (props . (p-search-prior ,prior))
                            (key . ,prior))
      (insert (or importance-char " ") " " (propertize template-name 'face 'p-search-header-line-key) "\n")
      (pcase-dolist (`(,name . ,val) options)
        (unless (eq name 'template)
          (insert "  " (symbol-name name) ": " (prin1-to-string val) "\n"))))))

(defun p-search-refresh-buffer ()
  "Refresh the current p-search buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (at-line (line-number-at-pos))
        (occlusion-states '()))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'p-search-key)
       (push (cons (overlay-get ov 'p-search-key)
                   (overlay-get ov 'p-search-section-hidden))
             occlusion-states))
      (delete-overlay ov))
    (erase-buffer)
    (p-search-add-section (propertize "Priors" 'face 'p-search-section-heading)
      (p-search-insert-prior p-search-base-prior)
      (dolist (prior p-search-priors)
        (p-search-insert-prior prior)))
    (goto-char (point-min))
    (let ((p-search-last-results nil))
      (p-search-display-function))
    (forward-line (1- at-line))
    (save-excursion
      (let* ((ovs (overlays-in (point-min) (point-max))))
        (dolist (ov ovs)
          (let* ((key (overlay-get ov 'p-search-key))
                 (is-hidden (alist-get key occlusion-states nil nil #'equal)))
            (when is-hidden
              (goto-char (overlay-start ov))
              (p-search-toggle-section)))))))
  (when (called-interactively-p 'any)
    (p-search--notify-main-thread)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main thread | display func ;;;

(defvar-local p-search-top-n 5
  "Number of results to display in p-search buffer.
The higher this value is, the more computationally expensive it
will be to display the results.")

(defvar-local p-search-display-timer nil)
(defvar-local p-search-last-results nil
  "Variable to store the last nop-n results.  If value is the same don't re-draw.")

(defun p-search--display-columns ()
  "Return a list of two numbers: the start of column 2 and the end of column 2."
  (let* ((body-width (window-body-width))
         (page-width (min 100 body-width)))
    (list
     page-width
     (- page-width 12))))

(defun p-search-result-window (document)
  "Return the string of the contents of DOCUMENT to preview to user."
  ;; TODO - apply major mode for faces
  ;; TODO - make sure this is customizable
  (let ((all-priors p-search-priors))
    (with-temp-buffer
      (let* ((line-nums '())
             (max-line 0)
             (has-matches t))
        (insert (p-search-document-text document))
        (funcall major-mode)
        (goto-char (point-min))
        (let* ((hints '()))
          (dolist (prior all-priors)
            (let* ((template (p-search-prior-template prior))
                   (hint-func (p-search-prior-template-result-hint-function template)))
              (when hint-func
                (let* ((hint-results (funcall hint-func prior (current-buffer))))
                  (setq hints (append hints hint-results))))))
          (pcase-dolist (`(,start . ,end) hints)
            (put-text-property start end 'face 'p-search-hi-yellow)
            (let ((lnum (line-number-at-pos start)))
              (push lnum line-nums)
              (when (> lnum max-line)
                (setq max-line lnum)))))
        (setq line-nums (cl-remove-duplicates line-nums))
        (when (not line-nums)
          (setq line-nums '(10 9 8 7 6 5 4 3 2 1))
          (setq max-line 10)
          (setq has-matches nil))
        (setq line-nums (seq-sort #'> line-nums))
        (let* ((lines '())
               (digits (1+ (floor (log max-line 10)))))
          (dolist (num line-nums)
            (goto-char (point-min))
            (forward-line (1- num))
            (insert (format (concat "%" (number-to-string digits) "d: ") num))
            (push (buffer-substring (pos-bol) (pos-eol)) lines))
          (when (> (length lines) 10)
            (let ((after-ct (- (length lines) 10)))
              (setq lines (seq-take lines 10))
              (when has-matches
                (setq lines
                      (append lines (list (concat (make-string (+ digits 2) ?\s) (format "%d more..." after-ct))))))))
          (string-join lines "\n"))))))

(defun p-search-display-function () ;; TODO - rename
  "Add the search results to p-search buffer.
This function is expected to be called every so often in a p-search buffer."
  (when p-search-posterior-probs
    (pcase-let* ((`(_ ,start-s ,start-us _) (current-time))
           (elts '())) ;; (1) Get top-n results
      (dotimes (i p-search-top-n)
        (let* ((newelt (heap-delete-root p-search-posterior-probs)))
          (push newelt elts)))
      (setq elts (nreverse elts))
      (setq elts (seq-filter #'identity elts))
      ;; re-add the fetched elements to the heap so they don't get lost.
      (dolist (elt elts)
        (heap-add p-search-posterior-probs elt))
      (unless (equal p-search-last-results elts)
        (setq p-search-last-results elts)
        (let* ((body-width (window-body-width))
               (page-width (min 100 body-width))
               (page-dims (p-search--display-columns))
               (start-line (line-number-at-pos))
               (inhibit-read-only t)
               (ovs (overlays-in (point-min) (point-max))) ;; (2) Display results
               (res-ov (seq-find (lambda (ov) (overlay-get ov 'p-search-results)) ovs)))
          (if res-ov
              (let ((start (overlay-start res-ov))
                    (end (overlay-end res-ov)))
                (delete-region start end)
                (delete-overlay res-ov)
                (goto-char start))
            (goto-char (point-max))
            (insert "\n"))
          (p-search-add-section
              `((heading . ,(propertize
                             (format "Search Results (%d)" (heap-size p-search-posterior-probs))
                             'face 'p-search-section-heading))
                (props . (p-search-results t))
                (key . p-search-results-header))
            (pcase-dolist (`(,document ,p) elts)
              (let* ((doc-name (p-search-document-title document))
                     (heading-line-1
                      (concat (truncate-string-to-width
                               (propertize (p-search-format-inputs doc-name) 'face 'p-search-header-line-key) (- (cadr page-dims) 5))))
                     (heading-line
                      (concat heading-line-1
                              (make-string (- (cadr page-dims) (length heading-line-1)) ?\s)
                              (format "%.10f" (/ p p-search-marginal)))))
                (p-search-add-section `((heading . ,heading-line)
                                        (props . (p-search-result ,doc-name))
                                        (key . ,doc-name))
                  (insert (p-search-result-window document))
                  (insert "\n")))))
          (goto-char (point-min))
          (forward-line (1- start-line))
          (p-search-highlight-point-section)))
      (pcase-let* ((`(_ ,s ,us _) (current-time))
                   (duration (- (+ (* s 1000000) us)
                                (+ (* start-s 1000000) start-us))))
        (p-search-log "display-function complete (%d μs)" duration)))))

(defvar-local p-search-main-thread-mutex (make-mutex "p-search main thread"))
(defvar-local p-search-main-thread-cond (make-condition-variable p-search-main-thread-mutex "p-search main thread cond"))

(defun p-search--notify-main-thread ()
  "Notify that a calculation needs to be made."
  (p-search--calculate-probs)
  (p-search-display-function))

(defun p-search-main-thread ()
  "Initial function of the calculation thread."
  (with-mutex p-search-main-thread-mutex
    (let ((i 0)
          (home-buffer (current-buffer)))
      (while t
        (message "Running main thread: %d" i) (cl-incf i)
        (p-search--calculate-probs)
        (with-temp-buffer
          (condition-wait p-search-main-thread-cond))))))

(defun p-search-start-main-thread ()
  "Initialize computation thread."
  ;; Note: Getting threads to work as expected is actually really
  ;; confusing.  It turns out that running `p-search--calulate-probs'
  ;; is actually pretty fast so I'll forego the threads for now.
  ;; Worst case the user has to wait for a 0.5 second computaiton to
  ;; occur.
  (p-search--notify-main-thread))


;;; Sections   ;;;;

(defun p-search-deepest-section-overlays-at (position)
  "Return the overlay at POSITION with the highest section level."
  (let* ((deepest nil)
         (deepest-level -1)
         (ovs (overlays-at (point))))
    (dolist (ov ovs)
      (let ((lvl (overlay-get ov 'p-search-section-level)))
        (when (and lvl (> lvl deepest-level))
          (setq deepest-level lvl)
          (setq deepest ov))))
    deepest))

(defun p-search-short-info-string-from-overlay (overlay)
  "Return a condenced string indicating the essential settings of OVERLAY prior."
  (let* ((prior (overlay-get overlay 'p-search-prior)))
    (when prior
      (let* ((template (p-search-prior-template prior))
             (inputs (p-search-prior-template-input-spec template))
             (args (p-search-prior-arguments prior)))
        (if (> (length inputs) 1)
            (concat " "
                  (propertize
                   (string-join
                    (seq-map
                     (lambda (input)
                       (format "%s: %s" (symbol-name (car input))
                               (alist-get (car input) args "") "x"))
                     inputs)
                    " ")
                   'face
                   'p-search-value)
                  " ")
          (concat " "
                  (propertize
                   (format "%s" (alist-get (caar inputs) args))
                   'face
                   'p-search-value)
                  " "))))))

(defun p-search-occlude-section (overlay)
  "Occlude a toggable section of OVERLAY."
  (unless (overlay-get overlay 'p-search-section-level)
    (error "Overlay not a section"))
  (overlay-put overlay 'p-search-section-hidden t)
  (overlay-put overlay 'before-string
               (propertize " " 'display '(left-fringe magit-fringe-bitmap>)))
  (let* ((ov-start (overlay-start overlay))
         (eol-ov-start (save-excursion (goto-char ov-start) (pos-eol)))
         (occ-ov-start (1+ eol-ov-start))
         (ov-end (overlay-end overlay))
         (occ-ov (make-overlay occ-ov-start ov-end)))
    (overlay-put occ-ov 'invisible t)
    (overlay-put overlay 'p-search-occluding-overlay occ-ov)
    (when-let* ((condenced-string (p-search-short-info-string-from-overlay overlay))
                (info-ov (make-overlay ov-start eol-ov-start)))
      (overlay-put info-ov 'after-string (p-search-short-info-string-from-overlay overlay))
      (overlay-put overlay 'p-search-info-overlay info-ov))
    (goto-char ov-start)))

(defun p-search-reveal-section (overlay)
  "Reveal the contents of OVERLAY."
  (unless (overlay-get overlay 'p-search-section-level)
    (error "Overlay not a section"))
  (overlay-put overlay 'p-search-section-hidden nil)
  (overlay-put overlay 'before-string
               (propertize " " 'display '(left-fringe magit-fringe-bitmapv)))
  (let* ((occ-ov (overlay-get overlay 'p-search-occluding-overlay))
         (info-ov (overlay-get overlay 'p-search-info-overlay)))
    (unless occ-ov
      (error "Unable to find occluding/info overlay"))
    (delete-overlay occ-ov)
    (when info-ov
      (delete-overlay info-ov))))

(defun p-search-fold-sections-to-level (level)
  "Toggle the fold of the various sections to LEVEL.
E.g., level 1 is everything folded except the top level."
  (let* ((all-overlays (overlays-in (point-min) (point-max))))
    (dolist (ov all-overlays)
      (when (overlay-get ov 'p-search-section-hidden)
        (p-search-reveal-section ov)))
    (dolist (i '(2 1 0))
      (dolist (ov all-overlays)
        (when-let* ((ov-level (overlay-get ov 'p-search-section-level)))
          (if (> (1+ i) level)
              (when (= ov-level i)
                (p-search-occlude-section ov))))))))


;;;;;;;;;;;;;;;;;;;;
;;; mode helpers ;;;

(defun p-search-setup-buffer ()
  "Initialize the p-search session's buffer."
  (let* ((buffer (generate-new-buffer "p-search"))) ;; TODO - more sophisticated naming
    (with-current-buffer buffer
      (p-search-mode))
    (display-buffer buffer nil)
    (with-current-buffer buffer
      (p-search-setup-priors)
      (p-search-refresh-buffer)
      (p-search-start-main-thread))
    buffer)) ;; TODO - more sophisticated display mechanisms

(defun p-search--prior-at-point ()
  "Return the prior at the current point."
  (car (get-char-property-and-overlay (point) 'p-search-prior)))

(defun p-search--document-at-point ()
  "Return the prior at the current point."
  (car (get-char-property-and-overlay (point) 'p-search-result)))

;;;;;;;;;;;;;;;;
;;; commands ;;;

(defun p-search-input-importance ()
  "Prompt the user to enter importance."
  (intern (completing-read "Importance: " p-search-importance-levels nil t)))

(defun p-search-add-prior ()
  "Command to add prior to current p-search session."
  (interactive)
  (let* ((add-func (p-search-prior-template-add-prior-function
                    (p-search-prior-template p-search-base-prior))))
    (funcall add-func)))

(defun p-search-swap-base-prior ()
  "Change the base prior to a new type."
  (interactive)
  (let* ((choices (seq-map
                   (lambda (prior-template)
                     (cons (p-search-prior-template-name prior-template)
                           prior-template))
                   p-search-available-base-prior-templates))
         (selected-base-prior (completing-read "Base: " choices nil t))
         (template (alist-get selected-base-prior choices nil nil #'equal)))
    (p-search-setup-priors template)))

(defun p-search-kill-prior ()
  "Delete the prior under the point."
  (interactive)
  (let* ((prior (p-search--prior-at-point)))
    (if (p-search-base-prior-p prior)
        (p-search-swap-base-prior)
      (setq p-search-priors (remove prior p-search-priors)))
    (p-search--notify-main-thread)
    (p-search-refresh-buffer)))

(defun p-search-reinstantiate-prior ()
  "Re-run the prior under the point.
This is useful if the underlying data that the prior uses changes."
  (interactive)
  (let* ((prior (p-search--prior-at-point)))
    (message "Reinstantiating prior: %s" (p-search-prior-template-name (p-search-prior-template prior)))
    (p-search--rerun-prior prior)
    (p-search--notify-main-thread)
    (p-search-refresh-buffer)))

(defun p-search-importance ()
  "Command to edit the importance of an element."
  (interactive)
  (let* ((prior (p-search--prior-at-point)))
    (unless prior
      (user-error "No prior at point"))
    (let* ((new-importance (p-search-input-importance)))
      (setf (p-search-prior-importance prior) new-importance))
    (p-search-refresh-buffer)
    (p-search--notify-main-thread)
    (setq p-search-main-worker-dirty-p t)))

(defun p-search-edit ()
  "Command to edit the properties of a prior."
  (interactive)
  (let* ((prior (p-search--prior-at-point)))
    (unless prior
      (user-error "No prior at point"))
    (let* ((p-search-prior-editing t)
           (p-search-current-prior-template (p-search-prior-template prior))
           (p-search-default-inputs (cons
                                     (cons 'prior prior)
                                     (p-search-prior-arguments prior))))
      (call-interactively #'p-search-create-prior-dispatch))))

(defun p-search-shutdown ()
  "Quit the underlying p-search session."
  (interactive)
  (thread-signal p-search-main-worker-thread 'error nil)
  (while (thread-live-p p-search-main-worker-thread)
    (thread-yield))
  (kill-buffer))

(defun p-search-toggle-section ()
  "Toggle the visibility of the section under the point."
  (interactive)
  (let* ((ov (p-search-deepest-section-overlays-at (point)))
         (hidden-p (overlay-get ov 'p-search-section-hidden)))
    (if hidden-p
        (p-search-reveal-section ov)
      (p-search-occlude-section ov))))

(defun p-search-show-level-1 ()
  "Show only to top level sections of the buffer."
  (interactive)
  (p-search-fold-sections-to-level 1))

(defun p-search-show-level-2 ()
  "Show two levels of nesting, folding everything beneath them."
  (interactive)
  (p-search-fold-sections-to-level 2))

(defun p-search-show-level-3 ()
  "Show three levels of nesting."
  (interactive)
  (p-search-fold-sections-to-level 3))

(defun p-search-find-file ()
  "Navigate to the file under the point in other buffer."
  (interactive)
  (let* ((file-name (p-search--document-at-point))
         (line-number (save-excursion
                        (goto-char (pos-bol))
                        (and (looking-at " *\\([0-9]+\\): ")  ;; TODO?: extract this regexp
                             (string-to-number (match-string 1))))))
    (unless file-name
      (user-error "No file found at point"))
    (find-file-other-window file-name)
    (when line-number
      (goto-char (point-min))
      (forward-line (1- line-number)))))

(defun p-search-display-file ()
  "Display file under point in other buffer."
  (interactive)
  (let* ((file-name (p-search--document-at-point)))
    (unless file-name
      (user-error "No file found at point"))
    (display-buffer (find-file-noselect file-name))))

(defun p-search-minibuffer-results ()
  "Display current results in minibuffer to select."
  (interactive)
  (let* ((all-documents (p-search-list-all-docs))
         (selections (seq-map (pcase-lambda (`(,doc ,p))
                                (cons (p-search-document-title doc)
                                      (list doc p)))
                              all-documents))
         (max-selection (seq-max (seq-map (lambda (sel) (length (car sel))) selections)))
         (completion-extra-properties
          (list :annotation-function
                (lambda (selection)
                  (format "%s%f"
                          (make-string (- (+ 2 max-selection)
                                          (length selection))
                                       ?\s)
                          (cadr (alist-get selection selections nil nil #'equal))))))
         (selection (completing-read "Find search result: "
                                     (lambda (string pred action)
                                       (if (eq action 'metadata)
                                           `(metadata (display-sort-function . ,#'identity))
                                         (complete-with-action action selections string pred)))
                                     nil
                                     t)))))

(defun p-search ()
  "Start a p-search session."
  (interactive)
  (p-search-setup-buffer))

(provide 'p-search)
;;; p-search.el ends here
