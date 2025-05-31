;;; p-search-transient.el --- Support for transients -*- lexical-binding:t -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

(defun p-search-transient-read-directories (_prompt _init hist)
  "Read multiple directories.
HIST is the input history of the underlying `completing-read-multiple' command."
  (completing-read-multiple
   "Directories: "
   #'completion-file-name-table
   #'directory-name-p nil nil hist))

(defun p-search-transient-read-file-name (prompt init _hist)
  "Read file name, showing PROMPT with INIT as initial value."
  (read-file-name prompt nil nil t init))

(defun p-search-transient-read-existing-file-names (prompt init hist)
  "Read files, showing PROMPT with HIST, having initial input INIT.

Note, if INIT is nil, `default-directory' will be used."
  (completing-read-multiple prompt
                            #'completion-file-name-table
                            #'file-exists-p
                            nil
                            (abbreviate-file-name (or init default-directory))
                            hist))

(defun p-search-transient-read-bytes (prompt &optional init hist)
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

(defclass p-search-transient--option (transient-variable)
  ((option-symbol :initarg :option-symbol :initform nil)
   (default-value :initarg :default-value :initform nil)
   (instruction-string :initarg :instruction-string :initform nil)
   (description :initarg :description :initform nil)))

(cl-defmethod transient-infix-value ((obj p-search-transient--option))
  "Return value of OBJ, being a cons pair of its symbol and value."
  (when-let ((value (oref obj value)))
      (cons
       (oref obj option-symbol)
       (oref obj value))))

(cl-defmethod transient-init-value ((obj p-search-transient--option))
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

(cl-defmethod transient-infix-set ((obj p-search-transient--option) value)
  "Simple infix set method to set VALUE slot of OBJ."
  (oset obj value value))

(cl-defmethod transient-format-value ((obj p-search-transient--option))
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

(defclass p-search-transient--directory (p-search-transient--option)
  ((reader :initform (lambda (prompt init _hist)
                       (read-directory-name prompt init nil nil init)))
   (multi-value :initarg :multi-value :initform nil)))

(cl-defmethod transient-init-value ((obj p-search-transient--directory))
  "Return initial values of OBJ."
  (cl-call-next-method)
  (let* ((multi-value-p (oref obj multi-value)))
    (if multi-value-p
        (progn
          (oset obj reader #'p-search-transient-read-directories)
          (oset obj prompt "Directories: "))
      (oset obj reader (lambda (prompt init _hist)
                         (read-directory-name prompt init nil nil init)))
      (oset obj prompt "Directory: "))))

(cl-defmethod transient-format-value ((obj p-search-transient--directory))
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
  :class p-search-transient--directory)


(defclass p-search-transient--file (p-search-transient--option)
  ((reader :initform #'p-search-transient-read-file-name)))

(transient-define-infix p-search-infix-file ()
  :class p-search-transient--file)

(defclass p-search-transient--files (p-search-transient--option)
  ((reader :initform #'p-search-transient-read-existing-file-names)))

(transient-define-infix p-search-infix-files ()
  :class p-search-transient--files)

(defclass p-search-transient--regexp (p-search-transient--option)
  ((reader :initform #'read-regexp)
   (prompt :initform "regexp: ")))

(transient-define-infix p-search-infix-regexp ()
  :class p-search-transient--regexp)



;;; String

(defclass p-search-transient--string (p-search-transient--option)
  ((reader :initform #'read-string)
   (multi-value :initarg :multi-value :initform nil)))

(cl-defmethod transient-init-value ((_obj p-search-transient--string))
  "The init-value transient method, doing nothing."
  (cl-call-next-method))

(cl-defmethod transient-format-value ((obj p-search-transient--string))
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

(cl-defmethod transient-infix-value ((obj p-search-transient--string))
  "Set initial transient value of OBJ for string."
  (if-let* ((val (oref obj value)))
      (cons
       (oref obj option-symbol)
       (if-let* ((multi-value-p (oref obj multi-value)))
           (string-split val "[ ,]" t " *")
         val))
    nil))

(transient-define-infix p-search-infix-string ()
  :class p-search-transient--string)


;;; Number

(defclass p-search-transient--number (p-search-transient--option)
  ((reader :initform #'read-number)
   (prompt :initform "number: ")))

(transient-define-infix p-search-infix-number ()
  :class p-search-transient--number)


(defclass p-search-transient--memory (p-search-transient--option)
  ((reader :initform #'p-search-transient-read-bytes)
   (prompt :initform "Memory size (e.g. 10MB): ")))

(cl-defmethod transient-format-value ((obj p-search-transient--memory))
  "Format the value of memory value OBJ."
  (if-let* ((bytes (and (slot-boundp obj 'value) (oref obj value))))
      (cond
       ((< bytes 10e3) (format "%dB" bytes))
       ((< bytes 10e6) (format "%.2fKB" (/ bytes 1000.0)))
       ((< bytes 10e9) (format "%.2fMB" (/ bytes 1000000)))
       (t (format "%.2fGB" (/ bytes 1000000000))))
    (propertize "nil" 'face 'transient-inactive-value)))

(transient-define-infix p-search-infix-memory ()
  :class p-search-transient--memory)


;;; Time

(defclass p-search-transient--date (p-search-transient--option)
  ((reader :initform #'p-search-transient-read-date)))

(transient-define-infix p-search-infix-date ()
  :class p-search-transient--date)


;;; Choices

(defclass p-search-transient--choices (p-search-transient--option)
  ((choices :initarg :choices)))

(cl-defmethod transient-infix-read ((obj p-search-transient--choices))
  "Transient infix-read method for choices OBJ."
  (let* ((choices (oref obj choices)))
    (intern (completing-read
             (oref obj prompt)
             (if (functionp choices) (funcall choices)
               choices)
             nil t))))

(transient-define-infix p-search-infix-choices ()
  :class p-search-transient--choices)


;;; Constant - pre-defined value that can't be changed.

(defclass p-search-transient--const (p-search-transient--option)
  ((init-value :initarg :value)))

(cl-defmethod transient-infix-read ((obj p-search-transient--const))
  "Prevent read for transient OBJ, returning original value."
  (beep)
  (message "value read only")
  (oref obj value))

(transient-define-infix p-search-infix-const ()
  :class p-search-transient--const)


;;; Custom - provide a custom reader

(defclass p-search-transient--custom (p-search-transient--option)
  ((reader :initarg :reader)))

(cl-defmethod transient-infix-read ((obj p-search-transient--custom))
  "Transient infix-read for OBJ custom reader type."
  (funcall (oref obj reader)))

(transient-define-infix p-search-infix-custom ()
  :class p-search-transient--custom)



;;; Toggle

(defclass p-search-transient--toggle (p-search-transient--option)
  ((init-state :initarg :init-state)))

(cl-defmethod transient-init-value ((obj p-search-transient--toggle))
  "Transient init-value function for toggle type OBJ."
  (cl-call-next-method)
  (when-let (init-value (and (slot-boundp obj 'init-state) (oref obj init-state)))
    (oset obj value init-value)))

(cl-defmethod transient-infix-read ((obj p-search-transient--toggle))
  "Transient infix-read method for OBJ of type toggle."
  (let* ((val (oref obj value)))
    (if val
        nil
      'on)))

(transient-define-infix p-search-infix-toggle ()
  :class p-search-transient--toggle)

(provide 'p-search-transient)

;;; p-search-transient.el ends here
