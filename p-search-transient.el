;;; p-search-transient.el --- Support for transients  -*- lexical-binding:t -*-

;;; Commentary:

;; This library implements p-search-specific prefix and suffix classes,
;; and their methods.

;;; Code:

;;; Reader Functions

(defun p-search-read-date-dist (prompt init hist)
  (let* ((date (org-read-date))
         (sigma (read-number "standard deviation (days): " nil hist)))
    (cons date sigma)))

(defun p-search-read-directories (prompt init hist)
  (completing-read-multiple "Directories: " #'completion-file-name-table #'directory-name-p nil nil hist))

(defun p-search-read-bytes (prompt &optional init hist)
  (catch 'done
    (while t
     (let* ((val (read-string prompt init hist)))
       (save-match-data
         (if (not (string-match " *\\([0-9]*\\) *\\([kKmMgGtT]?i?[bB]?\\)" val))
             (progn
               (beep)
               (message "invalid byte size input")
               (sit-for 1))
           (let* ((num (match-string 1 val))
                  (unit (downcase (match-string 2 val)))
                  (to-byte (alist-get unit '(("" . 1)
                                             ("b" . 1)
                                             ("kb" . 1000)
                                             ("kib" . 1024)
                                             ("mb" . 1000000)
                                             ("mib" . 1048576)
                                             ("tb" .  1000000000000)
                                             ("tib" . 1099511627776))
                                      nil nil #'equal)))
             (if to-byte
                 (throw 'done (* (string-to-number num) to-byte))
               (beep)
               (message "invalid byte size input")
               (sit-for 1))
             )))))))

;;; Transient Classes

(defclass p-search--option (transient-variable)
  ((option-symbol :initarg :option-symbol :initform nil)
   (default-value :initarg :default-value :initform nil)))

(cl-defmethod transient-infix-value ((obj p-search--option))
  (when-let ((value (oref obj value)))
      (cons
       (oref obj option-symbol)
       (oref obj value))))

(cl-defmethod transient-init-value ((obj p-search--option))
  ""
  (let* ((option-symbol (oref obj option-symbol))
         (default-value (and (slot-boundp obj 'default-value) (oref obj default-value)))
         (init-value (or (alist-get option-symbol p-search-default-inputs) default-value)))
    (when init-value
      (oset obj value init-value))))


(defclass p-search--template (p-search--option)
  ((template :initarg :template)))

(cl-defmethod transient-init-value :around ((obj p-search--template))
  ""
  (ignore))

(cl-defmethod transient-format-value ((obj p-search--template)) ""
  (p-search-prior-template-name (oref obj template)))

(cl-defmethod transient-infix-value :around ((obj p-search--template)) ""
  (cons 'template
        (oref obj template)))

(transient-define-infix p-search--template-infix ()
  :class p-search--template)


(cl-defmethod transient-infix-set ((obj p-search--option) value)
  ""
  (oset obj value value))

(cl-defmethod transient-format-value ((obj p-search--option))
  (if-let* ((value (and (slot-boundp obj 'value) (oref obj value))))
      (cond
       ((stringp value)
        (propertize (format "\"%s\"" value) 'face 'transient-value))
       (t
        (propertize (format "%s" value) 'face 'transient-value)))
    (propertize "nil" 'face 'transient-inactive-value)))


(defclass p-search--date+sigma (p-search--option)
  ((reader :initform #'p-search-read-date-dist)
   (prompt :initform "Date and σ days :")))

(cl-defmethod transient-format-value ((obj p-search--date+sigma))
  (if-let* ((value (and (slot-boundp obj 'value) (oref obj value))))
      (propertize
       (format "N(%s,%ddays)" (car value) (cdr value))
       'face 'transient-value)
    (propertize "N(?,?)" 'face 'transient-inactive-value)))

(transient-define-infix p-search--date+sigma-infix ()
  :class p-search--date+sigma)


(defclass p-search--directory (p-search--option)
  ((multi-value :initarg :multi-value :initform nil)))

(cl-defmethod transient-init-value ((obj p-search--directory))
  (cl-call-next-method)
  (let* ((multi-value-p (oref obj multi-value)))
    (if multi-value-p
        (progn
          (oset obj reader #'p-search-read-directories)
          (oset obj prompt "Directories: "))
      (oset obj reader (lambda (prompt init hist)
                         (read-directory-name prompt init nil nil init)))
      (oset obj prompt "Directory: "))))

(cl-defmethod transient-format-value ((obj p-search--directory))
  (let* ((multi-value-p (and (slot-boundp obj 'multi-value) (oref obj multi-value)))
         (value (and (slot-boundp obj 'value) (oref obj value))))
    (cond
     ((and value multi-value-p)
      (concat "("
                (string-join
                 (seq-map
                  (lambda (dir)
                    (propertize (format "\"%s\"" dir) 'face 'transient-value))
                  value)
                 ", ")
                ")"))
     ((and value (not multi-value-p))
      (propertize value 'face 'transient-value))
     ((and (not value) multi-value-p)
      (propertize "()" 'face 'transient-inactive-value))
     (t
      (propertize "nil" 'face 'transient-inactive-value)))))

(transient-define-infix p-search--directory-infix ()
  :class p-search--directory)


(defclass p-search--file (p-search--option)
  ((reader :initform #'read-file-name)
   (prompt :initform "file: ")))

(transient-define-infix p-search--file-infix ()
  :class p-search--file)



(defclass p-search--regexp (p-search--option)
  ((reader :initform #'read-regexp)
   (prompt :initform "regexp: ")))

(transient-define-infix p-search--regexp-infix ()
  :class p-search--regexp)


(defclass p-search--string (p-search--option)
  ((reader :initform #'read-string)
   (multi-value :initarg :multi-value :initform nil )))

(cl-defmethod transient-init-value ((obj p-search--string))
  ""
  (cl-call-next-method)
  (if-let* ((multi-value-p (oref obj multi-value)))
      (oset obj prompt (format "%s (comma separated): " (symbol-name (oref obj option-symbol))))
    (oset obj prompt (format "%s: " (symbol-name (oref obj option-symbol))))))

(cl-defmethod transient-format-value ((obj p-search--string))
  (if-let* ((val (and (slot-boundp obj 'value) (oref obj value))))
      (if-let* ((multi-value-p (oref obj multi-value))
                (values (cdr (transient-infix-value obj))))
          (concat "("
                  (string-join
                   (seq-map
                    (lambda (val)
                      (propertize (format "\"%s\"" val)
                                  'face 'transient-value))
                    values)
                   ", ")
                  ")")
        (propertize
         (format "\"%s\"" val) 'face 'transient-value))
    (propertize "nil" 'face 'transient-inactive-value)))

(cl-defmethod transient-infix-value ((obj p-search--string))
  (if-let* ((val (oref obj value)))
      (cons
       (oref obj option-symbol)
       (if-let* ((multi-value-p (oref obj multi-value)))
           (string-split val "[ ,]" t " *")
         val))
    nil))

(transient-define-infix p-search--string-infix ()
  :class p-search--string)


(defclass p-search--number (p-search--option)
  ((reader :initform #'read-number)))

(transient-define-infix p-search--number-infix ()
  :class p-search--number)


(defclass p-search--memory (p-search--option)
  ((reader :initform #'p-search-read-bytes)
   (prompt :initform "Memory size (e.g. 10MB): ")))

(cl-defmethod transient-format-value ((obj p-search--memory))
  (if-let* ((bytes (and (slot-boundp obj 'value) (oref obj value))))
      (cond
       ((< bytes 10e3) (format "%dB" bytes))
       ((< bytes 10e6) (format "%.2fKB" (/ bytes 1000.0)))
       ((< bytes 10e9) (format "%.2fMB" (/ bytes 1000000)))
       (t (format "%.2fGB" (/ bytes 1000000000))))
    (propertize "nil" 'face 'transient-inactive-value)))

(transient-define-infix p-search--memory-infix ()
  :class p-search--memory)


(defclass p-search--choices (p-search--option)
  ((choices :initarg :choices)
   (init-choice :initarg :init-choice)))

(cl-defmethod transient-init-value ((obj p-search--choices))
  ""
  (cl-call-next-method)
  (oset obj prompt (format "%s: " (symbol-name (oref obj option-symbol))))
  (when-let (init-value (and (slot-boundp obj 'init-choice) (oref obj init-choice)))
    (oset obj value init-value))) ;; TODO - do I need init-choice

(cl-defmethod transient-infix-read ((obj p-search--choices))
  (let* ((choices (oref obj choices))
         (prompt (oref obj prompt)))
    (intern (completing-read (oref obj prompt) (if (functionp choices) (funcall choices)
                                                 choices)
                             nil t))))

(transient-define-infix p-search--choices-infix ()
  :class p-search--choices)


(defclass p-search--toggle (p-search--option)
  ((init-state :initarg :init-state)))

(cl-defmethod transient-init-value ((obj p-search--toggle))
  ""
  (cl-call-next-method)
  (oset obj prompt (format "%s: " (symbol-name (oref obj option-symbol))))
  (when-let (init-value (and (slot-boundp obj 'init-state) (oref obj init-state)))
    (oset obj value init-state)))

(cl-defmethod transient-infix-read ((obj p-search--toggle))
  (let* ((val (oref obj value)))
    (if val
        nil
      'on)))

(transient-define-infix p-search--toggle-infix ()
  :class p-search--toggle)

;;;

(defvar p-search-current-prior-template nil)
(defvar p-search-default-inputs nil)
(defvar p-search-prior-editing nil
  "If nil, prior is being created.  If argument alist, provides initial args for transient.
When an alist, the prior key contains the prior to be updated.")

(defun p-search-transient-read-spec (spec)
  "Call the read function for SPEC, returning it's value."
  (let* ((prompt-string (symbol-name (car spec))))
   (pcase spec
     (`(,name . (directory-names . ,opts))
      (p-search-read-directories (format "%s (comma separated): " prompt-string) nil nil))
     (`(,name . (directory-name . ,opts))
      (read-directory-name (format "%s (comma separated): " prompt-string)))
     (`(,name . (string . ,opts))
      (read-string (format "%s: " prompt-string)))
     (`(,name . (regexp . ,opts))
      (read-regexp (format "%s: " prompt-string)))
     (`(,name . (memory . ,opts))
      (p-search-read-bytes (format "%s: " prompt-string)))
     (`(,name . (number . ,opts))
      (read-number (format "%s: " prompt-string)))
     (`(,name . (toggle . ,opts))
      (error "Not implemented: switch"))
     (`(,name . (choice . ,opts))
      (let* ((choices (plist-get opts :choices)))
        (completing-read (format "%s: " prompt-string) (if (functionp choices) (funcall choices) choices)
                         nil t)))
     (_ (error "unsupported spec %s" spec)))))

(defun p-search-read-current-specs ()
  (let* ((res '())
         (template (p-search-prior-template-input-spec p-search-current-prior-template)))
    (pcase-dolist (`(,name . ,spec) template)
      (push (cons name
                  (p-search-transient-read-spec (cons name spec)))
            res))
    (nreverse res)))

(defun p-search-transient-parse-spec (spec &optional always-read)
  (pcase spec
    (`(,name . (directory-names . ,opts))
     (let* ((key (plist-get opts :key))
            (description (plist-get opts :description)))
       (transient-parse-suffix
        transient--prefix
        `(,key ,description
               p-search--directory-infix
               :multi-value t
               :option-symbol ,name
               :always-read ,always-read))))
    (`(,name . (directory-name . ,opts))
     (let* ((key (plist-get opts :key))
            (description (plist-get opts :description)))
       (transient-parse-suffix
        transient--prefix
        `(,key ,description
               p-search--directory-infix
               :option-symbol ,name
               :always-read ,always-read))))
    (`(,name . (file . ,opts))
     (let* ((key (plist-get opts :key))
            (description (plist-get opts :description)))
       (transient-parse-suffix
        transient--prefix
        `(,key ,description
               p-search--file-infix
               :option-symbol ,name
               :always-read ,always-read))))
    (`(,name . (regexp . ,opts))
     (let* ((key (plist-get opts :key))
            (description (plist-get opts :description)))
       (transient-parse-suffix
        transient--prefix
        `(,key ,description
               p-search--regexp-infix
               :option-symbol ,name
               :always-read ,always-read))))
    (`(,name . (choice . ,opts))
     (let* ((key (plist-get opts :key))
            (description (plist-get opts :description))
            (choices (plist-get opts :choices)))
       (transient-parse-suffix
        transient--prefix
        `(,key ,description
               p-search--choices-infix
               :init-choice ,(if (functionp choices) nil
                               (car choices)) ;; First choice is default
               :choices ,choices
               :option-symbol ,name
               :always-read ,always-read))))
    (`(,name . (toggle . ,opts))
     (let* ((key (plist-get opts :key))
            (description (plist-get opts :description)))
       (transient-parse-suffix
        transient--prefix
        `(,key ,description
               p-search--toggle-infix
               :option-symbol ,name
               :always-read ,always-read))))
    (`(,name . (memory . ,opts))
     (let* ((key (plist-get opts :key))
            (description (plist-get opts :description)))
       (transient-parse-suffix
        transient--prefix
        `(,key ,description
               p-search--memory-infix
               :option-symbol ,name
               :always-read ,always-read))))
    (`(,name . (string . ,opts))
     (let* ((key (plist-get opts :key))
            (description (plist-get opts :description)))
       (transient-parse-suffix
        transient--prefix
        `(,key ,description
               p-search--string-infix
               :option-symbol ,name
               :always-read ,always-read))))
    (`(,name . (number . ,opts))
     (let* ((key (plist-get opts :key))
            (description (plist-get opts :description)))
       (transient-parse-suffix
        transient--prefix
        `(,key ,description
               p-search--number-infix
               :option-symbol ,name
               :always-read ,always-read))))
    (_ (error "unsupported spec %s" spec))))

(defun p-search-transient-input-suffixes ()
  (let* ((ress (list (transient-parse-suffix
                      transient--prefix
                      `(" " ""
                        p-search--template-infix
                        :template ,p-search-current-prior-template
                        :inapt-if always))))
         (template (p-search-prior-template-input-spec p-search-current-prior-template)))
    (dolist (spec template)
      (push (p-search-transient-parse-spec spec t) ress))
    (nreverse ress)))

(defun p-search-transient-option-suffixes ()
  (let* ((ress '())
         (template (p-search-prior-template-options-spec p-search-current-prior-template)))
    (dolist (spec template)
      (push (p-search-transient-parse-spec spec) ress))
    (nreverse ress)))

(defun p-search--is-prior-edit-mode ()
  p-search-prior-editing)
(defun p-search--is-prior-create-mode ()
  (not p-search-prior-editing))
(defun p-search--is-base-prior-template ()
  (oref p-search-current-prior-template search-space-function))

(transient-define-prefix p-search-create-prior-dispatch ()
  "Dispatch create-prior command."
  ["Input"
   :setup-children
   (lambda (_)
     (append
      (list)
      (p-search-transient-input-suffixes)))]
  ["Options"
   :setup-children
   (lambda (_)
     (append
      (p-search-transient-option-suffixes)
      (if (p-search--is-base-prior-template)
          '()
        (list (transient-parse-suffix
               transient--prefix
               `("-c" "complement"
                 p-search--toggle-infix
                 :init-state nil))
              (transient-parse-suffix
               transient--prefix
               `("-i" "importance"
                 p-search--choices-infix
                 :choices ,p-search-importance-levls
                 :init-choice medium
                 :option-symbol importance))))))]
  ["Actions"
   ("c" "create"
    (lambda ()
      (interactive)
      (p-search-transient-prior-create))
    :if p-search--is-prior-create-mode)
   ("e" "edit"
    (lambda ()
      (interactive)
      (p-search-transient-prior-edit))
    :if p-search--is-prior-edit-mode)])

(defun p-search-transient-prior-create ()
  (interactive)
  (let* ((args (transient-args 'p-search-create-prior-dispatch))
         (template (alist-get 'template args))
         (prior (p-search--instantiate-prior template args)))
    (setq p-search-priors (append p-search-priors (list prior)))
    (p-search-refresh-buffer)))

(defun p-search-transient-prior-edit (&optional prior)
  (interactive)
  (let* ((prior (or prior (car (get-char-property-and-overlay (point) 'p-search-prior))))
         (args (transient-args 'p-search-create-prior-dispatch)))
    (setf (p-search-prior-arguments prior) args)
    (p-search--rerun-prior prior)
    (p-search--notify-main-thread)
    (p-search-refresh-buffer)))

(defun zr/todo ()
  (interactive)
  (message ">>> %s" (transient-args 'p-search-create-prior-dispatch)))

(defun p-search-dispatch-prior-creation (prior-template)
  (let* ((p-search-current-prior-template prior-template)
         (p-search-default-inputs (p-search-read-current-specs)))
    (call-interactively #'p-search-create-prior-dispatch)))

(transient-define-prefix p-search-add-prior-dispatch ()
  "Dispatch an add-prior command."
  [["File System"
    ("f d" "directory"
     (lambda ()
       (interactive)
       (p-search-dispatch-prior-creation p-search--subdirectory-prior-template)))
    ("f n" "file name"
     (lambda ()
       (interactive)
       (p-search-dispatch-prior-creation p-search--filename-prior-template))) ;; regexp -> input
    ("f t" "file type"
     (lambda ()
       (interactive)
       (p-search-dispatch-prior-creation p-search--filetype-prior-template))) ;; string -> input
    ("f m" "modification date"
     (lambda ()
       (interactive)
       (p-search-dispatch-prior-creation p-search--modification-date-prior-template))) ;; date+sigma -> input
    ("f s" "size"
     (lambda ()
       (interactive)
       (p-search-dispatch-prior-creation p-search--file-size-prior-template))) ;; byte string -> input
    ("f c" "distance" zr/todo) ;; directory-name -> input
    ]
   ["Git"
    ("g a" "author"
     (lambda ()
       (interactive)
       (p-search-dispatch-prior-creation  p-search--git-author-prior-template))) ;; git -> read authors -> input
    ("g b" "branch"
     (lambda ()
       (interactive)
       (p-search-dispatch-prior-creation p-search--git-branch-prior-template))) ;; git -> read branches -> input
    ("g c" "file co-changes" zr/todo) ;;; read file -> input
    ("g m" "modification frequency" zr/todo) ;; ---
    ("g t" "commit time" zr/todo)]] ;; date+sigma -> input
  [["Vector"
    ("v d" "vector distance" zr/todo)] ;; read string -> input
   ["Emacs"
    ("e b" "open buffer" zr/todo)] ;;  -> ---
   ["Source"
    ("s t" "text match" zr/todo) ;; string -> inptut
    ("s c" "co-located text match" zr/todo) ;; multiple string -> input
    ("s f" "text frequency" zr/todo)]  ;; string -> input
   ["Source (Regexp) "
    ("r t" "regexp text match"
     (lambda ()
       (interactive)
       (p-search-dispatch-prior-creation p-search--textsearch-prior-template))) ;; string -> inptut
    ("r c" "co-located regexp text match" zr/todo) ;; multiple string -> input
    ("r f" "regexp frequency" zr/todo)]])

(provide 'p-search)

(provide 'p-search-transient)
;;; p-search-transient.el ends here
