;;; p-search-transient.el --- Support for transients -*- lexical-binding:t -*-

;; Author: Zachary Romero
;; URL: https://github.com/zkry/p-search
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools
;;

;;; Commentary:

;; This packages defines the various transient integrations that
;; p-search uses.

;;; Code:

(require 'org)
(require 'transient)

;;; Variables:

(defvar p-search-transient-default-inputs nil) ;; TODO: This can probably be removed
(defvar p-search-enable-instructions)

;;; Transient Definitions:

(defun p-search-transient-read-date (prompt init _hist)
  "Read date via `org-read-date' using PROMPT and INIT for initial value."
  (org-read-date nil nil nil prompt nil init))

(defun p-search-read-directories (_prompt _init hist)
  "Read multiple directories.
HIST is the input history of the underlying `completing-read-multiple' command."
  (completing-read-multiple
   "Directories: "
   #'completion-file-name-table
   #'directory-name-p nil nil hist))

(defun p-search-read-file-name (prompt init _hist)
  "Read file name, showing PROMPT with INIT as initial value."
  (read-file-name prompt nil nil t init))

(defun p-search-read-existing-file-names (prompt init hist)
  "Read files, showing PROMPT with HIST, having initial input INIT.

Note, if INIT is nil, `default-directory' will be used."
  (completing-read-multiple prompt
                            #'completion-file-name-table
                            #'file-exists-p
                            nil
                            (abbreviate-file-name (or init default-directory))
                            hist))

(defun p-search-read-bytes (prompt &optional init hist)
  "Read a byte value (e.g. \"1MB\"), displaying PROMPT to user.
INIT and HIST the initial value and input history respectively."
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
               (sit-for 1)))))))))


;;; Base Option Class

(defclass p-search--option (transient-variable)
  ((option-symbol :initarg :option-symbol :initform nil)
   (default-value :initarg :default-value :initform nil)
   (instruction-string :initarg :instruction-string :initform nil)
   (description :initarg :description :initform nil)))

(cl-defmethod transient-infix-value ((obj p-search--option))
  "Return value of OBJ, being a cons pair of its symbol and value."
  (when-let ((value (oref obj value)))
      (cons
       (oref obj option-symbol)
       (oref obj value))))

(cl-defmethod transient-init-value ((obj p-search--option))
  "Return initial value of OBJ.
The initial value will either be: the value of the options symbol
in `p-search-transient-default-inputs' or the value in the transient
objects `default-value' slot."
  (let* ((option-symbol (oref obj option-symbol))
         (default-value (and (slot-boundp obj 'default-value) (oref obj default-value)))
         (init-value (or (alist-get option-symbol p-search-transient-default-inputs) default-value))
         (instruction-string (and (slot-boundp obj 'instruction-string)
                                  (oref obj instruction-string)))
         (description (and (slot-boundp obj 'description)
                           (oref obj description))))
    (oset obj prompt
          (if (and instruction-string p-search-enable-instructions)
              (format "%s\n%s: " instruction-string (or description (symbol-name (oref obj option-symbol))))
            (format "%s: " (or description (symbol-name (oref obj option-symbol))))))
    (when init-value
      (oset obj value init-value))))

(cl-defmethod transient-infix-set ((obj p-search--option) value)
  "Simple infix set method to set VALUE slot of OBJ."
  (oset obj value value))

(cl-defmethod transient-format-value ((obj p-search--option))
  "Format value of OBJ."
  (if-let* ((value (and (slot-boundp obj 'value) (oref obj value))))
      (cond
       ((stringp value)
        (propertize (format "\"%s\"" value) 'face 'transient-value))
       (t
        (propertize (format "%s" value) 'face 'transient-value)))
    (propertize "nil" 'face 'transient-inactive-value)))


;;; TODO - make directory only accept single parameter


;;; Directory

(defclass p-search--directory (p-search--option)
  ((reader :initform (lambda (prompt init _hist)
                       (read-directory-name prompt init nil nil init)))
   (multi-value :initarg :multi-value :initform nil)))

(cl-defmethod transient-init-value ((obj p-search--directory))
  "Return initial values of OBJ."
  (cl-call-next-method)
  (let* ((multi-value-p (oref obj multi-value)))
    (if multi-value-p
        (progn
          (oset obj reader #'p-search-read-directories)
          (oset obj prompt "Directories: "))
      (oset obj reader (lambda (prompt init _hist)
                         (read-directory-name prompt init nil nil init)))
      (oset obj prompt "Directory: "))))

(cl-defmethod transient-format-value ((obj p-search--directory))
  "Format value of OBJ, looking like a list of items."
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

(transient-define-infix p-search-infix-directory ()
  :class p-search--directory)


(defclass p-search--file (p-search--option)
  ((reader :initform #'p-search-read-file-name)))

(transient-define-infix p-search-infix-file ()
  :class p-search--file)

(defclass p-search--files (p-search--option)
  ((reader :initform #'p-search-read-existing-file-names)))

(transient-define-infix p-search-infix-files ()
  :class p-search--files)

(defclass p-search--regexp (p-search--option)
  ((reader :initform #'read-regexp)
   (prompt :initform "regexp: ")))

(transient-define-infix p-search-infix-regexp ()
  :class p-search--regexp)



;;; String

(defclass p-search--string (p-search--option)
  ((reader :initform #'read-string)
   (multi-value :initarg :multi-value :initform nil)))

(cl-defmethod transient-init-value ((_obj p-search--string))
  "The init-value transient method, doing nothing."
  (cl-call-next-method))

(cl-defmethod transient-format-value ((obj p-search--string))
  "Format transient OBJ value for string."
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
  "Set initial transient value of OBJ for string."
  (if-let* ((val (oref obj value)))
      (cons
       (oref obj option-symbol)
       (if-let* ((multi-value-p (oref obj multi-value)))
           (string-split val "[ ,]" t " *")
         val))
    nil))

(transient-define-infix p-search-infix-string ()
  :class p-search--string)


;;; Number

(defclass p-search--number (p-search--option)
  ((reader :initform #'read-number)
   (prompt :initform "number: ")))

(transient-define-infix p-search-infix-number ()
  :class p-search--number)


(defclass p-search--memory (p-search--option)
  ((reader :initform #'p-search-read-bytes)
   (prompt :initform "Memory size (e.g. 10MB): ")))

(cl-defmethod transient-format-value ((obj p-search--memory))
  "Format the value of memory value OBJ."
  (if-let* ((bytes (and (slot-boundp obj 'value) (oref obj value))))
      (cond
       ((< bytes 10e3) (format "%dB" bytes))
       ((< bytes 10e6) (format "%.2fKB" (/ bytes 1000.0)))
       ((< bytes 10e9) (format "%.2fMB" (/ bytes 1000000)))
       (t (format "%.2fGB" (/ bytes 1000000000))))
    (propertize "nil" 'face 'transient-inactive-value)))

(transient-define-infix p-search-infix-memory ()
  :class p-search--memory)


;;; Time

(defclass p-search--date (p-search--option)
  ((reader :initform #'p-search-transient-read-date)))

(transient-define-infix p-search-infix-date ()
  :class p-search--date)


;;; Choices

(defclass p-search--choices (p-search--option)
  ((choices :initarg :choices)))

(cl-defmethod transient-infix-read ((obj p-search--choices))
  "Transient infix-read method for choices OBJ."
  (let* ((choices (oref obj choices)))
    (intern (completing-read
             (oref obj prompt)
             (if (functionp choices) (funcall choices)
               choices)
             nil t))))

(transient-define-infix p-search-infix-choices ()
  :class p-search--choices)


;;; Constant - pre-defined value that can't be changed.

(defclass p-search--const (p-search--option)
  ((init-value :initarg :value)))

(cl-defmethod transient-infix-read ((obj p-search--const))
  "Prevent read for transient OBJ, returning original value."
  (beep)
  (message "value read only")
  (oref obj value))

(transient-define-infix p-search-infix-const ()
  :class p-search--const)


;;; Custom - provide a custom reader

(defclass p-search--custom (p-search--option)
  ((reader :initarg :reader)))

(cl-defmethod transient-infix-read ((obj p-search--custom))
  "Transient infix-read for OBJ custom reader type."
  (funcall (oref obj reader)))

(transient-define-infix p-search-infix-custom ()
  :class p-search--custom)



;;; Toggle

(defclass p-search--toggle (p-search--option)
  ((init-state :initarg :init-state)))

(cl-defmethod transient-init-value ((obj p-search--toggle))
  "Transient init-value function for toggle type OBJ."
  (cl-call-next-method)
  (when-let (init-value (and (slot-boundp obj 'init-state) (oref obj init-state)))
    (oset obj value init-value)))

(cl-defmethod transient-infix-read ((obj p-search--toggle))
  "Transient infix-read method for OBJ of type toggle."
  (let* ((val (oref obj value)))
    (if val
        nil
      'on)))

(transient-define-infix p-search-infix-toggle ()
  :class p-search--toggle)

(provide 'p-search-transient)

;;; p-search-transient.el ends here
