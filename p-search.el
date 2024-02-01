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

;; TODO - consider name change: b-search, b-find, baysrch, prb-seek,

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
  (name nil :documentation "Name of prior, to be identified by the user")
  (initialize-function nil :documentation "Function to populate prior results.  Called with three arguments: prior, base-priors, and args.")
  (base-prior-key nil :documentation "Argument name that, when given a critical importance, is used to determine the base file listing, as well as given to other priors to optimize their performance.") ;; deprecated
  (default-result nil :documentation "Result that should be returned if no file is specified.")
  (input-spec nil :documentation "Specification of inputs required for the function to function.")
  (options-spec nil :documentation "Specification of parameters which alter the operation of the prior.")
  (search-space-function nil :documentation "Function that when called returns a list of items to be the seach space."))

(cl-defstruct (p-search-prior
               (:copier nil)
               (:constructor p-search-prior-create))
  "An instantiated prior created from a template which is actively
providing information to a search.in "
  (template nil :type p-search-prior-template)
  (importance nil :documentation "How much the prior should influence results.") ;; TODO - identiy where values come from
  (results nil :documentation "hash table containing the result.  Maps from file name to result indicator.")
  (proc-thread nil :documentation "This slot stores the process or thread that does main computation.")
  (arguments nil :documentation "Arguments provided to the prior.  These are the union of inputs and options."))

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
  (let* ((args (oref p-search-base-prior arguments))
         (template (oref p-search-base-prior template))
         (search-space-func (oref template search-space-function))
         (search-space-key (list (type-of p-search-base-prior) args)))
    (if-let* ((res (gethash search-space-key p-search-search-space-cache)))
        res
      (unless search-space-func
        (error "base prior has no search-space-function."))
      (let* ((res (funcall search-space-func args)))
        (puthash search-space-key res p-search-search-space-cache)
        res))))

(defvar-local p-search-priors nil
  "List of active priors for current session.")
(defvar-local p-search-base-prior nil
  "The prior which is used to determine search space.
This prior has a special function in that its
`search-space-function' is called which returns all of the
elements to search over.")

(defun p-search--extract-default-options (template)
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
  "Create and return a prior according to TEMPLATE."
  (let* ((init-func (p-search-prior-template-initialize-function template))
         (importance (alist-get 'importance args))
         (prior (p-search-prior-create
                 :template template
                 :arguments args
                 :results (make-hash-table :test 'equal)
                 :importance (or importance 'medium)))
         (base-priors (oref p-search-base-prior arguments))
         (init-res (funcall init-func prior base-priors args)))
    (setf (p-search-prior-proc-thread prior) init-res)
    prior))

(defun p-search--rerun-prior (prior)
  "Stop currently running PRIOR and re-runs it."
  (let* ((old-thread|proc (p-search-prior-proc-thread prior))
         (template (p-search-prior-template prior))
         (init-func (p-search-prior-template-initialize-function template))
         (args (p-search-prior-arguments prior))
         (base-priors (oref p-search-base-prior arguments))
         (init-res (and init-func (funcall init-func prior base-priors args))))
    (when (threadp old-thread|proc)
      (thread-signal old-thread|proc 'stop nil))
    (when (and (processp old-thread|proc) (eql (process-status old-thread|proc) 'run))
      (kill-process old-thread|proc))
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
                 (args (p-search-prior-arguments p))
                 (key (p-search-prior-template-base-prior-key template))
                 (prior-input (alist-get key args))
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
     (calc-eval (format "betaI(%f, 0.5, 0.5)" p)))
    (`(,p medium) p)
    (`(,p high)
     (calc-eval (format "betaI(%f, 3.0, 3.0)" p)))
    (`(,p critical)
     (calc-eval (format "betaI(%f, 100, 100)" p)))))

(defvar-local p-search-posterior-probs nil
  "Heap of calculated posterior probabilities.
Elements are of the type (FILE PROB).")

(defun p-search--calculate-probs ()
  "For all current priors, calculate element probabilities."
  (message "--- p-search--calculate-probs")
  (let* ((old-heap p-search-posterior-probs)
         (files (p-search-generate-search-space))
         (dependent-priors (p-search--dependent-priors))
         (marginal-p 0.0)
         (res (make-heap (lambda (a b) (if (= (cadr a) (cadr b))
                                           (string> (car a) (car b))
                                         (> (cadr a) (cadr b))))
                         (length files)))
         (start-time (current-time)))
    (setq p-search-posterior-probs res)
    (dolist (file files)
      (let* ((probability 1.0))
        (dolist (p dependent-priors)
          (let* ((prior-results (p-search-prior-results p))
                 (prior-template (p-search-prior-template p))
                 (default-result (p-search-prior-template-default-result prior-template))
                 (prior-importance (alist-get 'importance  (p-search-prior-arguments p) 'medium)) ;; TODO - Default?
                 (file-result (gethash file prior-results (or default-result 'neutral))) ;; TODO indecies?
                 (modifier (p-search-modifier file-result prior-importance)))
            (setq probability (* probability modifier))))
        (heap-add res (list file probability))
        (cl-incf marginal-p probability))
      (pcase-let* ((`(,_ ,sec ,ms) (time-since start-time)))
        (when (> (+ (* 1000 sec) ms) 300000)
          (with-temp-buffer
            (thread-yield))
          (setq start-time (current-time)))))
    (message "--- p-search--calculate-probs done")
    res))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; p-search mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local p-search-main-worker-thread nil)

(defvar p-search-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (keymap-set map "a" #'p-search-add-prior)
    (keymap-set map "e" #'p-search-edit)
    (keymap-set map "g" #'p-search-refresh-buffer)
    (keymap-set map "i" #'p-search-importance)
    (keymap-set map "k" #'p-search-kill-prior)
    (keymap-set map "<tab>" #'p-search-toggle-section)
    map))

(defun p-search-pre-command-hook ()
  ;; (p-search-highlight-point-section)
  )

(defun p-search-post-command-hook ()
  (p-search-highlight-point-section)
  )

(defun p-search-kill-hook ()
  (when (timerp p-search-display-timer)
    (cancel-timer p-search-display-timer))
  (when (threadp p-search-main-worker-thread)
    (thread-signal p-search-main-worker-thread 'exit nil))
  (let* ((log-buf (get-buffer (format "logs: %s" (buffer-name (current-buffer))))))
    (when log-buf
      (kill-buffer log-buf))))

(with-temp-buffer
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert "hey")
      (kill-buffer buf))
    (current-buffer)))

(define-derived-mode p-search-mode special-mode "p-search"
  ""
  :group 'p-search
  (hack-dir-local-variables-non-file-buffer)
  (add-hook 'pre-command-hook #'p-search-pre-command-hook nil t)
  (add-hook 'post-command-hook #'p-search-post-command-hook t t)
  (add-hook 'kill-buffer-hook #'p-search-kill-hook nil t)
  (let ((buf (current-buffer)))
    (setq p-search-display-timer
          (run-with-timer 1 1
                          (lambda ()
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (p-search-display-function)))))))
  (p-search-start-main-thread)

  ;; (setq-local revert-buffer-function #'magit-refresh-buffer)
  ;; (setq-local bookmark-make-record-function #'magit--make-bookmark)
  ;; (setq-local imenu-create-index-function #'magit--imenu-create-index)
  ;; (setq-local isearch-filter-predicate #'magit-section--open-temporarily)
  )

;;;;;;;;;;;;;;;;;
;;; logging  ;;;;

(defun p-search-log (format-string &rest args)
  ""
  (let* ((message (apply #'format (cons format-string args)))
         (ts (format-time-string "%H:%M:%S %3N"))
         (buf-name (format "logs: %s" (buffer-name (current-buffer)))))
    (with-current-buffer (get-buffer-create buf-name)
      (save-excursion
        (goto-char (point-max))
        (insert "\n" ts "\t" message)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prior initialization ;;;;

(defun p-search-setup-priors ()
  "Set up the buffers pre-existing priors."
  (let* ((proj (project-current))
         (project-root (and proj (expand-file-name (project-root (project-current))))) ;; TODO - make this an opt
         (initial-priors (list (p-search-prior-create
                                :template p-search--subdirectory-prior-template
                                :importance 'critical
                                :arguments
                                `((include-directories . ,(and project-root (list project-root))))))))
    (setq p-search-priors nil)
    (setq p-search-base-prior (p-search-prior-create
                               :template p-search-prior-base--filesystem
                               :arguments (p-search-prior-default-arguments p-search-prior-base--filesystem)))))

;;;;;;;;;;;;;;;;;
;;; display ;;;;;

(defvar-local p-search--section-level 0)

(defun p-search-highlight-point-section ()
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
      (overlay-put max-ov 'face 'highlight))))

(defun p-search-add-section-overlay (start end &optional props key)
  "Add overlay to indicate collapsible section from START to END."
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
   (t
    inputs)))

(defun p-search-insert-prior (prior)
  (let* ((start (point))
         (template (p-search-prior-template prior))
         (template-name (p-search-prior-template-name template))
         (importance (p-search-prior-importance prior))
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
      (insert (or importance-char " ") " " (propertize template-name 'face 'magit-header-line-key) "\n")
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
    (p-search-add-section (propertize "Priors" 'face 'magit-header-line)
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

(defun p-search-display-function () ;; TODO - rename
  "Add the search results to p-search buffer.
This function is expected to be called every so often in a p-search buffer."
  (message "DISPLAY")
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
        (let* ((start-line (line-number-at-pos))
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
          (p-search-add-section `((heading . ,(format "Search Results (%d)" (heap-size p-search-posterior-probs)))
                                  (props . (p-search-results t)))
            (pcase-dolist (`(,name ,p) elts)
              (p-search-add-section `((heading . "")
                                      (props . (p-search-result ,name)))
                (insert (truncate-string-to-width (propertize (p-search-format-inputs name) 'face 'magit-header-line-key) 65))
                (insert (make-string (- 70 (current-column)) ?\s)) ;; TODO - better column
                (insert (format "%.6f" p))
                (insert "\n"))))
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
  (with-mutex p-search-main-thread-mutex
    (condition-notify p-search-main-thread-cond)))

(defun p-search-main-thread ()
  (with-mutex p-search-main-thread-mutex
    (let ((i 0)
          (home-buffer (current-buffer)))
      (while t
        (message "Running main thread: %d" i) (cl-incf i)
        (p-search--calculate-probs)
        (with-temp-buffer
          (condition-wait p-search-main-thread-cond))))))

(defun p-search-start-main-thread ()
  (when (and p-search-main-worker-thread
             (thread-live-p p-search-main-worker-thread))
    (thread-signal p-search-main-worker-thread 'error nil))
  (setq p-search-main-worker-thread
        (make-thread #'p-search-main-thread "p-search-main")))

;;;;;;;;;;;;;;;;;;;
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
  "Returns a condenced string indicating the essential settings of OVERLAY prior."
  (let* ((prior (overlay-get overlay 'p-search-prior)))
    (when prior
      (let* ((template (p-search-prior-template prior))
             (inputs (p-search-prior-template-input-spec template))
             (args (p-search-prior-arguments prior)))
        (concat " "
         (propertize
          (string-join
           (seq-map
            (lambda (input)
              (format "%s" (alist-get (car input) args "")))
            inputs)
           " ")
          'face
          'transient-value)
         " ")))))

(defun p-search-occlude-section (overlay)
  "Occlude a toggable section."
  (unless (overlay-get overlay 'p-search-section-level)
    (error "overlay not a section"))
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
  (unless (overlay-get overlay 'p-search-section-level)
    (error "overlay not a section"))
  (overlay-put overlay 'p-search-section-hidden nil)
  (overlay-put overlay 'before-string
               (propertize " " 'display '(left-fringe magit-fringe-bitmapv)))
  (let* ((occ-ov (overlay-get overlay 'p-search-occluding-overlay))
         (info-ov (overlay-get overlay 'p-search-info-overlay)))
    (unless occ-ov
      (error "unable to find occluding/info overlay."))
    (delete-overlay occ-ov)
    (when info-ov
      (delete-overlay info-ov))))



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
      (p-search-refresh-buffer)
      (p-search-start-main-thread))
    buffer)) ;; TODO - more sophisticated display mechanisms

(defun p-search--prior-at-point ()
  (car (get-char-property-and-overlay (point) 'p-search-prior)))

;;;;;;;;;;;;;;;;
;;; commands ;;;

(defun p-search-input-importance ()
  "Prompt the user to enter importance."
  (intern (completing-read "Importance: " p-search-importance-levels nil t)))

(defun p-search-add-prior ()
  "Command to add prior to current p-search session."
  (interactive)
  (p-search-add-prior-dispatch))

(defun p-search-kill-prior ()
  (interactive)
  (let* ((prior (p-search--prior-at-point)))
    (setq p-search-priors (remove prior p-search-priors))
    (p-search-refresh-buffer)
    (p-search--notify-main-thread)))

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
  (interactive)
  (thread-signal p-search-main-worker-thread 'error nil)
  (while (thread-live-p p-search-main-worker-thread)
    (thread-yield))
  (kill-buffer))

(defun p-search-toggle-section ()
  (interactive)
  (let* ((ov (p-search-deepest-section-overlays-at (point)))
         (hidden-p (overlay-get ov 'p-search-section-hidden)))
    (if hidden-p
        (p-search-reveal-section ov)
        (p-search-occlude-section ov))))

(defun p-search ()
  ""
  (interactive)
  (p-search-setup-buffer))

;;; p-search.el ends here
