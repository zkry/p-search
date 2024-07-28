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

;;; Code:

(require 'heap nil t) ;; TODO: Remove noerror
(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'range)
(require 'org)
(require 'transient)

(require p-search-transient)
(require p-search-query)



;;; Custom
(defgroup p-search nil
  "Emacs Search Tool Aggregator."
  :prefix "p-search-"
  :group 'applications)

(defcustom p-search-default-search-tool
  (cond ((executable-find "rg") :rg)
        ((executable-find "ag") :ag)
        (t :grep))
  "Default tool to use when running search on filesystem."
  :group 'p-search
  :type '(choice (const :tag "grep" :grep)
                 (const :tag "ag (the_silver_searcher)" :ag)
                 (const :tag "rg (ripgrep)" :rg)))

(defcustom p-search-default-document-preview-size 10
  "Default number of lines show in the results preview section."
  :group 'p-search
  :type 'integer)


;;; Consts

(defconst p-search-score-yes 0.7)
(defconst p-search-score-neutral 0.3)
(defconst p-search-score-no 0.3)
(defconst p-search-importance-levels '(none low medium high critical))
(defconst p-search-query-wildcards '((:rg . "[^\w]")
                                     (:ag . "[^\s]")
                                     (:grep . "[^[:space:]]"))
  "Alist of search tool to wildcard regexp.")



;;; Vars

(defconst p-search-documentizer-functions (make-hash-table :test #'equal)
  "Hashmap of document type to document property alist ((prop-name . function)).
The documentizer is used to make common document types uniform and extendable.
Documents are given ID of the form (list type-sym element), where element can
be any Lisp object.")

(defconst p-search-candidate-generators '()
  "List of candidate-generator objects known to the p-search system.")

(defconst p-search-default-candidate-generators '()
  "List of candidate generators to be applied on startup of p-search session.")

(defconst p-search-prior-templates '()
  "List of prior templates known to p-search system.")



;;; Session Vars

;; The vars in this section are used on a per-search-session basis.

(defvar-local p-search-candidates-cache nil
  "Cache of generated candidates.")

(defvar-local p-search-candidates-by-generator nil
  "Map of candidate generator to candidate document id.")

(defvar-local p-search-active-candidate-generators nil
  "Alist of candidate-generator objects to user-provided args alist.")

(defvar-local p-search-priors nil
  "List of active prior components for search.")

(defvar-local p-search-posterior-probs nil
  "Heap of calculated posterior probabilities.
Elements are of the type (DOC-ID PROB).")

(defvar-local p-search--section-level 0
  "Variable used to determine leveling of nested sections.")

(defvar-local p-search-top-n 5
  "Number of results to display in p-search buffer.")

(defvar-local p-search-observations nil
  "Hash table of observiations.")

(defvar-local p-search-marginal nil
  "Hash table of observiations.")

(defvar-local p-search-parent-session-buffer nil
  "Stores the buffer of the p-search session used to create a child buffer.")

(defvar-local p-search-document-preview-size p-search-default-document-preview-size
  "The number of lines to show for each document preview in the current session.")

(defvar-local p-search-git-roots nil
  "List of known git roots used as cache.")


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

(defface p-search-prior
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DarkOliveGreen4"
     :weight bold)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DarkSeaGreen2"
     :weight bold))
  "Face for prior and candidate generators."
  :group 'p-search-faces)

(defface p-search-hi-yellow
  '((((min-colors 88) (background dark))
     (:weight bold :box (:line-width 1 :color "yellow1" :style nil)))
    (((background dark)) (:weight bold :box (:line-width 1 :color "yellow" :style nil)))
    (((min-colors 88)) (:weight bold :box (:line-width 1 :color "yellow1" :style nil)))
    (t (:weight bold :box (:line-width 1 :color "yellow" :style nil))))
  "Face for highlighting in p-search mode with bold text and a box."
  :group 'p-search-faces)



;;; Types

(cl-defstruct (p-search-candidate-generator
               (:copier nil)
               (:constructor p-search-candidate-generator-create))
  "Structure representing a method of creating search candidates."
  (name nil
   :documentation "Name of the generator, to be shown on search page.")
  (input-spec nil
   :documentation "Specification of inputs required for the function to function.")
  (options-spec nil
   :documentation "Specification of optional inputs required for the function to function.")
  (function nil
   :documentation "Function to generate list of candidates.
Takes one argument, the combined input/option arguments as an alist.")
  (term-frequency-function nil
   :documentation "Function to determine term frequencies of docs
generated by generator.  Function should have the following signiture:
(args query-term callback &key case-insensitive)")
  (term-presence-function nil
   :documentation "Function to determine whether the term is present
in the documents generated by generator.  Function should have the following signiture:
(args query-term callback &key case-insensitive)"))

(cl-defstruct (p-search-prior-template
               (:copier nil)
               (:constructor p-search-prior-template-create))
  "Structure representing a class of priors.
Base prior templates will have a non-nil value of `search-space-function'."
  (group nil :documentation "Group symbol of prior (e.g. git, filesystem)")
  (required-properties nil
   :documentation "List of required properties for the prior to be applicable.")
  (name nil
   :documentation "Name of prior, to be identified by the user")
  (input-spec nil
   :documentation "Specification of inputs required for the function to function.")
  (options-spec nil
                :documentation "Specification of parameters which alter the operation of the prior.")
  (initialize-function nil
                       :documentation "Function to populate prior results.
Called with three arguments: prior, base-priors, and args.")

  (result-hint-function nil
   :documentation "Optional function that takes the result in a buffer and
returns ranges of significance."))

(cl-defstruct (p-search-prior
               (:copier nil)
               (:constructor p-search-prior-create))
  "An instantiated prior created from a template informs a search."
  (template nil :type p-search-prior-template)
  (results nil
   :documentation "hash table containing the result.
Maps from file name to result indicator.")
  (proc-or-thread nil
   :documentation "This slot stores the process or thread that does main computation.")
  (arguments nil
   :documentation "Arguments provided to the prior.  These are the union of inputs and options.")
  (default-result nil ;; TODO - is this needed?
   :documentation "Override of the tempate's default result."))



;;; Helper Functions

(defun p-search--file-text (file-name)
  "Return the contents of OS-file FILE-NAME."
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun p-search--file-size (file-name) ;; TODO - add support for multibyte file sizes
  "Return the size of FILE-NAME in bytes."
  (nth 7 (file-attributes file-name)))

(defun p-search--file-git-root (file-name)
  "Return the git root of FILE-NAME."
  (catch 'done
    (dolist (root p-search-git-roots)
      (when (string-prefix-p root file-name)
        (throw 'done root)))
    (let* ((default-directory (file-name-directory file-name))
           (new-root (string-trim-right (shell-command-to-string "git rev-parse --show-toplevel"))))
      (push new-root p-search-git-roots )
      new-root)))

(defun p-search--git-available-p ()
  "Return non-nil if git is available from default directory."
  (= (call-process "git" nil nil nil "status") 0))




;;; Search Tool Interface

;; External tools are used to support fast searching of files without
;; building an index.  This section defineds an interface to work with
;; these tools.

(defun p-search--escape-term (string meta-chars)
  "Insert escape \\ characters in STRING for all chars in META-CHARS."
  (let* ((ret-str (make-string (* (length string) 2) 0))
         (i 0))
    (dotimes (j (length string))
      (if (member (aref string j) meta-chars)
          (progn
            (aset ret-str i ?\\)
            (aset ret-str (1+ i) (aref string j))
            (cl-incf i 2))
        (aset ret-str i (aref string j))
        (cl-incf i)))
    (substring ret-str 0 i)))

(defun p-search--rg-escape (string)
  "Insert escape \\ characters in STRING based on Rust's regex parser (used in rg)."
  (p-search--escape-term string '(?\\ ?. ?+ ?\( ?\) ?| ?\[ ?\] ?\{ ?\} ?^ ?$ ?# ?& ?- ?~))) ;; ?* ?\?

(defun p-search--ag-escape (string)
  "Insert escape \\ characters in STRING based on PCRE regex pattern (used in ag)."
  (p-search--escape-term string '(?\\ ?^ ?$ ?. ?\[ ?| ?\( ?\) ?+ ?{))) ;; ?* ?\?

(defun p-search--grep-escape (string)
  "Insert escape \\ characters in STRING based on PCRE regex pattern (used in ag)."
  (p-search--escape-term string '(?\\ ?.)))

(defun p-search--replace-wildcards (string tool)
  "Replace wildcard character in STRING with the wildcard regex fragment.
TOOL is used to look up the correct wildchard character."
  (let ((wildcard (or (alist-get tool p-search-query-wildcards)
                      "[^[:blank:]]")))
    (let ((pos 0)
          (new-string ""))
      ;; Replace * with wildcard*
      (while-let ((match (string-search "*" string pos)))
        (when (or (zerop match) (not (eql (aref string (1- match)) ?\\)))
          (setq new-string (concat new-string
                                   (substring string pos match)
                                   wildcard "*")))
        (setq pos (1+ match)))
      ;; replace ? with wildcard
      (let ((string (concat new-string (substring string pos)))
            (pos 0)
            (new-string ""))
        (while-let ((match (string-search "?" string pos)))
          (when (or (zerop match) (not (eql (aref string (1- match)) ?\\)))
            (setq new-string (concat new-string
                                     (substring string pos match)
                                     wildcard)))
          (setq pos (1+ match)))
        (concat new-string (substring string pos))))))

(defun p-search-query-grep--term-regexp (string)
  "Create a term regular expression from STRING.
A term regex is noted for marking boundary characters."
  (let* ((escaped-string (p-search--replace-wildcards (p-search--grep-escape string) :grep)))
    (list (propertize (concat "\\(\\<\\|_\\)" escaped-string "\\(\\>\\|_\\)") 'p-search-case-insensitive t)
          (concat "\\B"
                  (capitalize (substring escaped-string 0 1))
                  (substring escaped-string 1)))))

(defun p-search-query-rg--term-regexp (string)
  "Create a term regular expression from STRING.
A term regex is noted for marking boundary characters."
  (let* ((escaped-string (p-search--replace-wildcards (p-search--rg-escape string) :rg)))
    (list (propertize (concat "(\\b|_)" escaped-string "(\\b|_)") 'p-search-case-insensitive t)
          (concat "[a-z]" ;; TODO - use not-in-word-boundary instead
                  (capitalize (substring escaped-string 0 1))
                  (substring escaped-string 1))))) ;; TODO - will these double-count camel case?

(defun p-search-query-ag--term-regexp (string)
  "Create a term regular expression from STRING for ag tool.
A term regex is noted for marking boundary characters."
  (let* ((escaped-string (p-search--replace-wildcards (p-search--ag-escape string) :ag)))
    (list (propertize (concat "(\\b|_)" escaped-string "(\\b|_)") 'p-search-case-insensitive t)
          (concat "[a-z]" ;; TODO - use not-in-word-boundary instead
                  (capitalize (substring escaped-string 0 1))
                  (substring escaped-string 1)))))

(defun p-search-query-emacs--term-regexp (string)
  "Create a term regular expression from STRING.
A term regex is noted for marking boundary characters."
  (list (propertize (p-search--replace-wildcards (concat "\\<" string "\\>") :emacs) 'p-search-case-insensitive t)
        (concat (capitalize (substring string 0 1))
                (substring string 1))))

(defun p-search-query-grep--command (term)
  "Return command line arguments for rg search of TERM."
  (let ((case-insensitive-p (get-text-property 0 'p-search-case-insensitive term)))
    `("grep" "-r" "-c" ,@(and case-insensitive-p '("--ignore-case")) ,term ".")))

(defun p-search-query-rg--command (term)
  "Return command line arguments for rg search of TERM."
  (let ((case-insensitive-p (get-text-property 0 'p-search-case-insensitive term)))
    `("rg" "--count-matches" "--color" "never" ,@(and case-insensitive-p '("-i")) ,term)))

(defun p-search-query-ag--command (term)
  "Return command line arguments for ag search of TERM."
  (let ((case-insensitive-p (get-text-property 0 'p-search-case-insensitive term)))
    `("ag" "-c" "--nocolor" ,@(and case-insensitive-p '("-i")) ,term)))

(defun p-search-query-commands (string tool)
  "Return list of runnable commands from STRING based on TOOL."
  (pcase tool
    (:grep (seq-map #'p-search-query-grep--command (p-search-query-grep--term-regexp string))) ;; TODO
    (:rg (seq-map #'p-search-query-rg--command (p-search-query-rg--term-regexp string)))
    (:ag (seq-map #'p-search-query-ag--command (p-search-query-ag--term-regexp string)))
    (_ (error "Unsupported tool `%s'" tool))))


;;; Documentizer

;; A document in p-search is an alist of information retrieval (IR) properties.
;; An example of a document is as follows:
;;
;; ((id . (book . "123")) (title . "Othello") (content . "...."))
;;
;; The documentizer exists in order to provide a standard interface to
;; create documents of a given type, and to allow the interface to be
;; extendible, being the bridge between entities (be it on the
;; filesystem or in Emacs) and IR documents.

(defun p-search-def-property (type property-symbol function)
  "Define property PROPERTY-SYMBOL on TYPE by calling FUNCTION."
  (let* ((funcs (gethash type p-search-documentizer-functions))
         (newelt (cons property-symbol function)))
    (if (alist-get property-symbol funcs)
        (setf (alist-get property-symbol funcs) (cdr newelt))
      (push newelt funcs))
    (puthash type funcs p-search-documentizer-functions)))

(defun p-search-documentize (doc-id)
  "Given DOC-ID, return IR properties."
  (pcase-let ((`(,doc-type ,element) doc-id)
              (results `((id . ,doc-id))))
    (pcase-dolist (`(,prop-id . ,function) (gethash doc-type p-search-documentizer-functions))
      (let* ((property-fetcher (lambda () (funcall function element))))
        (push (cons prop-id property-fetcher) results)))
    (nreverse results)))

(defun p-search--size-from-content (doc-id)
  "Return the length of the content of document DOC-ID."
  (let* ((content (p-search-document-property doc-id 'content)))
    (length content))) ;; TODO - take into account multibyte

(p-search-def-property 'buffer 'title #'buffer-name)
(p-search-def-property 'buffer 'file-name #'buffer-file-name)
(p-search-def-property 'buffer 'content (lambda (buf) (with-current-buffer buf (buffer-string))))
(p-search-def-property 'buffer 'buffer #'identity)

(p-search-def-property 'file 'title #'identity)
(p-search-def-property 'file 'content #'p-search--file-text)
(p-search-def-property 'file 'file-name #'identity)
(p-search-def-property 'file 'size #'p-search--file-size)
(p-search-def-property 'file 'git-root #'p-search--file-git-root)

(p-search-def-property :default 'size #'p-search--size-from-content)


;;; Prior API

(defun p-search-candidates ()
  "Return the search candidates as map from id to document."
  (or p-search-candidates-cache
      (let ((candidates-set (make-hash-table :test 'equal))
            (generator-to-doc (make-hash-table :test 'equal)))
        (pcase-dolist (`(,gen . ,args) p-search-active-candidate-generators)
          (let* ((documents (funcall (p-search-candidate-generator-function gen) args)))
            (dolist (doc documents)
              (let* ((id (alist-get 'id doc)))
                (when (not (gethash id candidates-set))
                  (let ((key (cons gen args)))
                    (puthash key (cons id (gethash key generator-to-doc)) generator-to-doc))
                  (puthash id doc candidates-set))))))
        (setq p-search-candidates-cache candidates-set)
        (setq p-search-candidates-by-generator generator-to-doc)
        candidates-set)))

(defun p-search-document-property (document property)
  "Return PROPERTY of DOCUMENT."
  (unless (consp (car document))
    ;; document is an ID, fetch corresponding document
    (let* ((candidates (p-search-candidates)))
      (setq document (gethash document candidates))))
  (let ((id (alist-get 'id document))
        (elt (assoc property document)))
    (if elt
      (pcase-let ((`(_ . ,val) elt))
        (if (functionp val)
            (let* ((res (funcall val)))
              (setcdr elt res)
              res)
          val))
      (let* ((default-fns (gethash :default p-search-documentizer-functions))
             (default (alist-get property default-fns)))
        (when default
          (funcall default id))))))

(defun p-search-unique-properties (property)
  (let* ((candidates (p-search-candidates))
         (values '()))
    (maphash
     (lambda (document _)
       (let* ((val (p-search-document-property document property)))
        (when (not (member val values))
          (push val values))))
     candidates)
    values))

(defun p-search-candidate-with-properties-exists-p (properties)
  "Return non-nil if there exists a document will all of PROPERTIES."
  (catch 'done
    (let* ((documents (p-search-candidates)))
      (maphash
       (lambda (_ document)
         (when (seq-every-p
                (lambda (prop)
                  (p-search-document-property document prop))
                properties)
           (throw 'done t)))
       documents)
      nil)))

(defun p-search-candidates-with-properties (properties)
  "Return hashmap of documents with non-nil PROPERTIES."
  (let* ((documents (p-search-candidates))
         (res-hashmap (make-hash-table :test 'equal)))
    (maphash
     (lambda (id document)
       (catch 'not-applicable
         (when (seq-every-p
                (lambda (prop)
                  (p-search-document-property document prop))
                properties)
           (puthash id document res-hashmap))))
     documents)
    res-hashmap))

(defun p-search-reduce-document-property (prop-key init fn)
  "Reduce over all document properties' PROP-KEY value with FN.

INIT is the initial value given to the reduce operation."
  (let* ((x init))
    (maphash
     (lambda (_ doc)
       (let ((prop-val (p-search-document-property doc prop-key)))
         (setq x (funcall fn x prop-val))))
     (p-search-candidates))
    x))

(defun p-search-set-score (prior document value)
  "Set the score of DOCUMENT to VALUE for PRIOR."
  (let ((results-ht (p-search-prior-results prior)))
    (if (eql document :default)
        (puthash :default value results-ht)
      (unless (consp (car document))
        ;; document is an ID, fetch corresponding document
        (let* ((candidates (p-search-candidates)))
          (setq document (gethash document candidates))))
      (let* ((candidates (p-search-candidates))
             (results-ht (p-search-prior-results prior))
             (id (alist-get 'id document)))
        (when (and id (gethash id candidates))
          (puthash id value results-ht))))))

(defun p-search--remove-prior (prior)
  "Remove PRIOR from the current session, recalculating posteriors."
  (setq p-search-priors (cl-remove prior p-search-priors :test #'equal))
  (p-search-calculate))

(defun p-search--remove-candidate-generator (generator+args &optional no-recalc)
  "Remove GENERATOR+ARGS from the current session, recalculating posteriors."
  (setq p-search-active-candidate-generators
        (cl-remove generator+args p-search-active-candidate-generators :test #'equal))
  (when (not no-recalc)
    (p-search-restart-calculation)))


;;; Predefined Priors and Candidate Generators

(defconst p-search-candidate-generator-buffers
  (p-search-candidate-generator-create
   :name "BUFFERS"
   :input-spec '()
   :options-spec '()
   :function
   (lambda (_args)
     (seq-map
      (lambda (buf)
        (p-search-documentize `(buffer ,buf)))
      (buffer-list)))))

(defconst p-search-candidate-generator-filesystem
  (p-search-candidate-generator-create
   :name "FILESYSTEM"
   :input-spec `((base-directory . (p-search-infix-directory
                                    :key "d"
                                    :description "Directories"
                                    :default-value (lambda () default-directory)))
                 (filename-regexp . (p-search-infix-regexp
                                     :key "f"
                                     :description "Filename Pattern"
                                     :default-value ".*"))
                 (search-tool . (p-search-infix-choices
                                 :key "t"
                                 :description "search tool"
                                 :choices (:grep :rg :ag)
                                 :default-value ,(or p-search-default-search-tool :grep))))
   :options-spec '((ignore-pattern . (p-search-infix-regexp
                                      :key "-i" ;; TODO - allow multiple (?)
                                      :description "Ignore Pattern"))
                   (use-git-ignore . (p-search-infix-toggle
                                      :key "-g"
                                      :description "Git ls-files"
                                      :default-value on)))
   :function
   (lambda (args)
     (let-alist args
       (let* ((default-directory (expand-file-name .base-directory))
              (git-available-p (p-search--git-available-p)))
         (when (and .use-git-ignore (not git-available-p))
           (message "Cannot use git ignore for directory %s.  Falling back on all files." default-directory))
         (let* ((default-directory .base-directory)
                (file-candidates (if (and .use-git-ignore git-available-p)
                                     (string-split (shell-command-to-string "git ls-files") "\n" t "[\n ]")
                                   (string-split (shell-command-to-string "find . -type f") "\n" t "[\n ]")))
                (documents '()))
           (dolist (file file-candidates)
             (catch 'skip
               (when (string-prefix-p "./" file)
                 (setq file (substring file 2)))
               (unless (or (equal .filename-regexp ".*")
                           (string-match-p .filename-regexp file))
                 (throw 'skip nil))
               (when (and .ignore-pattern (string-match-p .ignore-pattern file))
                 (throw 'skip nil))
               (setq file (expand-file-name (file-name-concat default-directory file)))
               (push (p-search-documentize `(file ,file)) documents)))
           (nreverse documents)))))
   :term-frequency-function
   (cl-function
    (lambda (args query-term callback &key _case-insensitive)
      (let* ((search-tool (alist-get 'search-tool args))
             (file-counts (make-hash-table :test #'equal))
             (commands (p-search-query-commands query-term search-tool))
             (parent-buffer (current-buffer))
             (proc-complete-ct 0))
        (dolist (cmd commands)
          (let* ((buf (generate-new-buffer "*p-search rg")))
            (with-current-buffer buf
              (setq p-search-parent-session-buffer parent-buffer))
            (make-process
             :name "p-search-text-search"
             :buffer buf
             :command cmd
             :sentinel
             (lambda (proc event)
               (when (or (member event '("finished\n" "deleted\n"))
                         (string-prefix-p "exited abnormally with code" event)
                         (string-prefix-p "failed with code" event))
                 (with-current-buffer (process-buffer proc)
                   (let* ((files (string-split (buffer-string) "\n")))
                     (dolist (f files)
                       (when (string-prefix-p "./" f)
                         (setq f (substring f 2)))
                       (when (string-match "^\\(.*\\):\\([0-9]*\\)$" f)
                         (let* ((fname (match-string 1 f))
                                (count (string-to-number (match-string 2 f))))
                           (puthash (list 'file (file-name-concat default-directory fname)) count file-counts))))
                     (cl-incf proc-complete-ct)
                     (when (= proc-complete-ct (length commands))
                       (with-current-buffer p-search-parent-session-buffer
                         (funcall callback file-counts)))))))))))))))

;;; Generic priors

(defconst p-search-prior-title
  (p-search-prior-template-create
   :group 'general
   :name "title heading"
   :required-properties '(title)
   :input-spec '((title . (p-search-infix-string
                           :key "t"
                           :description "Document's Title")))
   :initialize-function
   (lambda (prior)
     (let* ((args (p-search-prior-arguments prior))
            (title (alist-get 'title args))
            (documents (p-search-candidates-with-properties '(title))))
       (maphash
        (lambda (_ document)
          (let* ((doc-title (p-search-document-property document 'title)))
            (when (string-search title doc-title)
              (p-search-set-score prior document p-search-score-yes))))
        documents)))))

(defconst p-search-prior-suffix
  (p-search-prior-template-create
   :group 'general
   :name "suffix of title"
   :required-properties '(title)
   :input-spec '((suffix . (p-search-infix-string
                           :key "s"
                           :description "Suffix")))
   :initialize-function
   (lambda (prior)
     (let* ((args (p-search-prior-arguments prior))
            (suffix (alist-get 'suffir args))
            (documents (p-search-candidates-with-properties '(title))))
       (maphash
        (lambda (_ document)
          (let* ((doc-title (p-search-document-property document 'title)))
            (when (string-suffix-p suffix doc-title)
              (p-search-set-score prior document p-search-score-yes))))
        documents)))))

;;; Buffer priors

(defconst p-search-prior-major-mode
  (p-search-prior-template-create
   :group 'emacs
   :name "major mode"
   :required-properties '(buffer)
   :input-spec '((major-mode . (p-search-infix-string
                                :key "-m"
                                :description "Buffer major mode")))
   :initialize-function
   (lambda (prior)
     (let* ((args (p-search-prior-arguments prior))
            (major-mode-sym (intern (alist-get 'major-mode args)))
            (documents (p-search-candidates)))
       (maphash
        (lambda (_ document)
          (let* ((buffer (p-search-document-property document 'buffer))
                 (ok (with-current-buffer buffer (eql major-mode-sym major-mode))))
            (when ok
              (p-search-set-score prior document p-search-score-yes))))
        documents)))))

;;; File system priors

(defconst p-search-prior-subdirectory
  (p-search-prior-template-create
   :group 'filesystem
   :name "subdirectory"
   :required-properties '(file-name)
   :input-spec '((include-directory . (p-search-infix-directory
                                         :key "d"
                                         :description "Directories")))
   :initialize-function
   (lambda (prior)
     (let* ((args (p-search-prior-arguments prior))
            (include-directory (alist-get 'include-directory args))
            (directory-expanded (expand-file-name include-directory))
            (documents (p-search-candidates-with-properties '(file-name))))
       ;; TODO - When an active prior exists, p-search-candidates should *by default* only
       ;;        return the candidates that have the specified properties
       (maphash
        (lambda (_ document)
          (catch 'out
            (let* ((file-name (p-search-document-property document 'file-name))
                   (file-expanded (expand-file-name file-name)))
              (if (string-prefix-p directory-expanded file-expanded)
                  (p-search-set-score prior document p-search-score-yes)
                (p-search-set-score prior document p-search-score-no)
                (throw 'out nil)))))
        documents)))))

(defconst p-search-prior-modification-date nil)

(defconst p-search-prior-file-size nil)

;;; Search priors

(defun p-search--prior-query-initialize-function (prior)
  "Initialization function for the text query priro.
Called with user supplied ARGS for the prior."
  (let* ((args (p-search-prior-arguments prior))
         (query-string (alist-get 'query-string args)))
    (p-search-query
     query-string
     (lambda (probs)
       (maphash
        (lambda (doc p)
          (p-search-set-score prior doc p))
        probs)
       (p-search-calculate))
     (hash-table-count (p-search-candidates))
     (p-search-reduce-document-property 'size 0 #'+))))

(defun p-search--text-search-hint (prior)
  "Mark places where the query args of PRIOR matches text in BUFFER."
  (let* ((args (p-search-prior-arguments prior))
         (query (alist-get 'query-string args)))
    (p-search-mark-query
     query
     (lambda (query)
       (let* ((terms (p-search-query-emacs--term-regexp query))
              (ress '()))
         (dolist (term terms)
           (save-excursion
             (goto-char (point-min))
             (let* ((case-fold-search (get-text-property 0 'p-search-case-insensitive term)))
               (while (search-forward-regexp term nil t)
                 (push (cons (match-beginning 0) (match-end 0)) ress)))))
         (setq ress (nreverse ress))
         ress)))))

(defconst p-search-prior-query
  (p-search-prior-template-create
   :group 'general
   :name "text query"
   :required-properties '()
   :input-spec '((query-string . (p-search-infix-string
                                  :key "q"
                                  :description "Query string")))
   :options-spec '()
   :initialize-function #'p-search--prior-query-initialize-function
   :result-hint-function #'p-search--text-search-hint))

;;; Git Priors

(defun p-search--available-git-authors ()
  "Return list of all authors for current session."
  (let* ((authors '())
         (git-roots (p-search2-unique-properties 'git-root)))
    (dolist (git-root git-roots)
      (let ((default-directory git-root))
        (setq authors (append authors (string-lines (shell-command-to-string "git log --all --format='%aN' | sort -u") t)))))
    authors))

(defun p-search--prior-git-author-initialize-function (prior)
  "Initialization function for the Git Author prior with PRIOR."
  (let* ((args (p-search-prior-arguments prior))
         (init-buf (current-buffer))
         (author (alist-get 'git-author args))
         (base-directories (p-search-unique-properties 'git-root))
         (git-command (format "git log --author=\"%s\" --name-only --pretty=format: | sort -u" author)))
    (dolist (default-directory base-directories)
      (let* ((buf (generate-new-buffer "*p-search-git-author*")))
        (make-process
         :name "p-seach-git-author-prior"
         :buffer buf
         :command `("sh" "-c" ,git-command)
         :sentinel (lambda (proc event)
                     (when (or (member event '("finished\n" "deleted\n"))
                               (string-prefix-p "exited abnormally with code" event)
                               (string-prefix-p "failed with code" event))
                       (let* ((content (with-current-buffer (process-buffer proc) (buffer-string)))
                              (root-dir (with-current-buffer (process-buffer proc) default-directory))
                              (commit-counts (make-hash-table :test #'equal))
                              (max-counts 0))
                         (with-current-buffer init-buf
                           (dolist (file (string-split content "\n"))
                             (when (> (length file) 0)
                               (let ((doc-id (list 'file (file-name-concat root-dir file))))
                                 (let ((count (1+ (gethash doc-id commit-counts 0))))
                                   (when (> count max-counts)
                                     (setq max-counts count))
                                   (puthash doc-id count commit-counts)))))
                           (maphash
                            (lambda (doc-id count)
                              (let ((p (+ 0.5 (* 0.2 (/ count max-counts)))))
                                (p-search-set-score prior doc-id p)))
                            commit-counts)
                           (p-search-set-score prior :default p-search-score-no)
                           (p-search-calculate))))))))))

(defconst p-search-prior-git-author
  (p-search-prior-template-create
   :group 'git
   :name "Git Author"
   :required-properties '(git-root)
   :input-spec '((git-author . (p-search-infix-choices
                                :key "a"
                                :description "Git Author"
                                :choices p-search--available-git-authors)))
   :options-spec '()
   :initialize-function #'p-search--prior-git-author-initialize-function))

(defun p-search--prior-git-commit-frequency-initialize-function (prior)
  "Initialization for git commit frequency for PRIOR."
  (let* ((base-dirs (p-search-unique-properties 'git-root))
         (args (p-search-prior-arguments prior))
         (n-commits (alist-get 'n-commits args)))
    (dolist (default-directory base-dirs)
      (let* ((last-commits-cmd (format "git log -%d --pretty=format:\"%%H\"" n-commits))
             (commits (string-lines (shell-command-to-string last-commits-cmd) t))
             (file-counts (make-hash-table :test #'equal))
             (max-count 0))
        (dolist (commit commits)
          (let* ((files (string-lines (shell-command-to-string (format "git show --pretty=format:\"\" --name-only %s" commit)) t)))
            (dolist (file files)
              ;; default of 1 for laplace smoothing
              (let ((count (1+ (gethash file file-counts 0))))
                (when (> count max-count)
                  (setq max-count count))
                (puthash file count file-counts)))))
        (maphash
         (lambda (file count)
           (let* ((p (+ 0.5 (* 0.2 (/ (float count) max-count)))))
             (p-search-set-score
              prior
              (list 'file (file-name-concat default-directory file))
              p)))
         file-counts)))
    (p-search-set-score prior :default p-search-score-no)))

(defconst p-search-prior-git-commit-frequency
  (p-search-prior-template-create
   :name "git commit frequency"
   :group 'git
   :input-spec
   '((n-commits . (p-search-infix-number
                   :key "n"
                   :description "Consider last N commits."
                   :default-value 20)))
   :initialize-function #'p-search--prior-git-commit-frequency-initialize-function))


;;; Queries

;; The query system is at the heart of p-search.  Most IR tasks
;; involve the user inputing some query term which they believe
;; corresponds with the relevant items.  p-search being a generic
;; search engine allows different backends to be plugged into the
;; system and allow seach.  A traditional IR system will use a reverse
;; index to quickly find documents.  p-searrch is more flexible in
;; that it doesn't mandate the use of a reverse index.
;;
;; Recall from previous sections that each document is composed of a
;; set of fields.  Buffer documents may have the :buffer field, while
;; filesystem documents may have the :file-name field.  The two fields
;; common to all documents are :content and :title.  p-search defaults
;; to reading the :content field and performing a search using Elisp
;; for the term.  While not performant, it is the fallback used.
;;
;; Seaches on filesystems with modern tools such as rg can be very
;; fast, negating the need of reverse indexes.  Since not all search
;; candidates may be on the filesystem, rg is only applicable in some
;; cases.  The determiner of which way to search for terms is
;; determined by the candidate generator.
;;
;; The candidate generator has two slots for dealing with content
;; queries: term-frequency-function and term-presence-function.  The
;; term-frequency-function is a function which takes a query and calls
;; a callback with the number of times a search term occurs in a
;; document.  The term-presence-function is a function that should
;; call a callback with the items which contain atleast one instance
;; of the given term.  The idea behind the two functions is that not
;; all document types may have an easy way of counting terms, but may
;; have an easy way of determining presence.  If presence is
;; determined, an elisp program can read the contents and determine
;; the exact count of occurrences.
;;
;; Due to the inherent async nature of the search system which may
;; need to call many processes, callbacks are heavily used.  The
;; program flow is as follows:
;;
;;                                             call candidate generator funcs'
;;                                                 term-frequency-function
;;
;; search-query        ----> query-component   /--> candidate-generator TF \
;;      |             /                       /                             \
;;      | parse      /-----> query-component -----> candidate-generator TF   --\
;;      v           /                         \                             /  |
;;  query-ast -----/-------> query-component   \--> candidate-generator TF /   |
;;                                          \                                  |
;;                                           ...                               |
;;                                             /                               |
;;                                    /--  <--/                                |
;; merge results for final scores <------  <----------------------------------/
;;       |                                   fan-in for query-component results
;;       |
;;       \--> noramlize score on 0.0 to 1.0 scale ---> final result for p-search
;;



;;; Posterior Calculation and Heap integration

;; Each search candidate document is assigned a probability based on
;; the user-defined prior distribution, and the users search
;; observations.
;;
;; The prior function is composed in two parts: first a probability is
;; assigned to each candidate based on how well the the prior provides
;; evidence of relevance.  For example, a user looking for a file with
;; "/tests/" in its path might assign a 0.7 to the file "/tests/a.el"
;; and 0.3 to "/src/a.el".  While yes or now priors may assign 0.7 and
;; 0.3 respectively, a hypothetical contains-vowels-in-name prior may
;; assign the gradations 0.7 to "/aaa/foo.el", 0.55 to "cdf/foo.el", 0.4 to
;; "cdf/a.el", and 0.3 to "cdf/xz.clj".
;;
;; After the "objective" probability is assigned, the user assigns a
;; level of importance to the prior, and based on the importance, the
;; "objective" probability is transformed to better reflect the users
;; beliefs of relevance.  So for example, if the above test assigns
;; 0.7 to file "/tests/a.el", but the user doesn't think this test is
;; that important, its probability shifts closer to 0.5, while if the
;; user thought that the test was vital, then it would shift closer to
;; 1.0.  This transformation currently is done via the BetaI function.
;;
;; After the prior distribution is calculated, the final posterior is
;; calculated based on the users observations.  A user can mark files
;; as observed.  Marking a file as observed will reduce the
;; probability of the file containing the thing being looked for but
;; it will not reduce it to 0.  Various gradation of observations can
;; exist, with more work-intensive observations reducing the documents
;; probability more than shorter observations.

(defun p-search--create-heap (&optional size)
  "Return a heap that has a sort first on probability, then on name.
If SIZE is provided create the heap with this size."
  (make-heap (lambda (a b) (if (= (cadr a) (cadr b))
                               (string> (format "%s" (car a)) (format "%s" (car b)))
                             (> (cadr a) (cadr b))))
             size))

(defun p-search-prior-modified-p (p importance)
  ;; TODO
  p)

(defun p-search-calculate (&optional no-reprint)
  "Calculate the posterior probabilities of all search candidates.
If NO-REPRINT is nil, don't redraw p-search buffer."
  (message "--- p-search-calculate")
  (let* ((documents (p-search-candidates))
         (priors p-search-priors)
         (marginal-p 0.0)
         (res (p-search--create-heap (hash-table-size documents))))
    (setq p-search-posterior-probs res)
    (maphash
     (lambda (id _)
       (let* ((probability 1.0))
         (dolist (prior priors)
           (let* ((prior-results (p-search-prior-results prior))
                  (default-result (or (gethash :default prior-results)
                                      0.5))
                  (importance (alist-get 'importance (p-search-prior-arguments prior) 'medium))
                  (complement (alist-get 'complement (p-search-prior-arguments prior)))
                  (doc-result (gethash id prior-results default-result))
                  (prior-p (p-search-prior-modified-p doc-result importance)))
             (when complement
               (setq prior-p (- 1.0 prior-p)))
             (setq probability (* probability prior-p))))
         (setq probability (* probability (gethash id p-search-observations 1.0)))
         (heap-add res (list id probability))
         (cl-incf marginal-p probability)))
     documents)
    (setq p-search-marginal marginal-p)
    (unless no-reprint
      (p-search--reprint))
    res))

(defun p-search-restart-calculation ()
  "Re-generate all candidates, and re-run all priors."
  (setq p-search-candidates-by-generator nil)
  (setq p-search-candidates-cache nil)
  (dolist (prior p-search-priors)
    (setf (p-search-prior-results prior) (make-hash-table :test #'equal))
    (let ((proc-thread (p-search-prior-proc-or-thread prior)))
      (when (processp proc-thread)
        (stop-process proc-thread))
      (when (threadp proc-thread)
        (thread-signal proc-thread nil nil)))
    (let* ((prior-template (p-search-prior-template prior))
           (init-func (p-search-prior-template-initialize-function prior-template))
           (init-res (funcall init-func prior)))
      (setf (p-search-prior-proc-or-thread prior) init-res)))
  (p-search-calculate))

(defun p-search-top-results ()
  "Return the top results of the posterior probs."
  (when p-search-posterior-probs
    (let* ((elts '()))
      (dotimes (i p-search-top-n)
        (let* ((newelt (heap-delete-root p-search-posterior-probs)))
          (push newelt elts)))
      (setq elts (nreverse elts))
      (setq elts (seq-filter #'identity elts))
      (dolist (elt elts)
        (heap-add p-search-posterior-probs elt))
      elts)))


;;; Transient Integration

;; The main vehicle for interacting with priors is transitent.
;; Transient is the key which allows for easy configuration of the
;; information retrieval system's parameters.  Due to the generic
;; nature of the search system the various transient menus are created
;; at runtime.
(defun p-search--transient-suffix-from-spec (name+spec &optional always-read default-value)
  "Return a transient suffix from a NAME+SPEC cons.
Pass value of ALWAYS-READ to transient object.  This is used for
inputs which must always have a value.  If DEFAULT-VALUE is non-nil,
use it as the :default-value slot."
  (let* ((name (car name+spec))
         (spec (cdr name+spec))
         (infix (car spec))
         (opts (cdr spec))
         (key (plist-get opts :key))
         (description (plist-get opts :description)))
    `(,key ,description
           ,infix
           :option-symbol ,name
           :always-read ,always-read
           ,@(if default-value `(:default-value ,default-value) '())
           ,@opts)))

(defun p-search-relevant-prior-templates ()
  "Return a list of prior templates which can apply to search candidates."
  (let* ((res '()))
    (dolist (template p-search-prior-templates)
      (let* ((reqs (p-search-prior-template-required-properties template)))
        (when (p-search-candidate-with-properties-exists-p reqs)
          (push template res))))
    (setq res (nreverse res))
    res))

(defun p-search-transient-dispatcher () "Placeholder for transient dispatch.")

(defun p-search-dispatch-transient (&rest config)
  "Dispatch an ad-hoc transient from CONFIG.
CONFIG should be provided simmilar to how `transient-define-prefix' is used."
  (pcase-let ((name 'p-search-transient-dispatcher)
              (`(,_class ,_slots ,suffixes ,_docstr ,_body)
               (transient--expand-define-args config nil)))
    (defalias 'p-search-transient-dispatcher
      (lambda ()
        (interactive)
        (transient-setup 'p-search-transient-dispatcher)))
    (put name 'transient--prefix
         (transient-prefix :command name))
    (put name 'transient--layout
         (seq-map 'eval (cl-mapcan (lambda (s) (transient--parse-child name s))
                                   suffixes)))
    (call-interactively name)))

(defun p-search--unique-prefix (elt elts)
  "Return a unique key prefix string for ELT compared to ELTS."
  (when (= (length elts) 1)
    (setq elts (cons "" elts)))
  (let* ((elts (seq-remove (lambda (x) (equal x elt)) elts))
         (normalize (lambda (str) (thread-last str
                                               downcase
                                               (string-replace "-" "")
                                               (string-replace " " ""))))
         (normalized-elt (funcall normalize elt))
         (normalized-elts (seq-map normalize elts))
         (prefix))
    (catch 'found
      (dotimes (i (length normalized-elt))
        (let* ((p (substring normalized-elt 0 i)))
          (when (not (seq-some (lambda (other-elt)
                                 (string-prefix-p p other-elt))
                               normalized-elts))
            (setq prefix p)
            (throw 'found prefix)))))
    (string-join (seq-map #'string (seq-into prefix 'list)) " ")))

(defun p-search--instantiate-prior (template args)
  "Create and return a prior according to TEMPLATE with ARGS.
This function will also start any process or thread described by TEMPLATE."
  (let* ((init-func (p-search-prior-template-initialize-function template))
         (prior (p-search-prior-create
                 :template template
                 :arguments args
                 :results (make-hash-table :test #'equal)))
         (init-res (funcall init-func prior)))
    (setf (p-search-prior-proc-or-thread prior) init-res)
    prior))

(defun p-search--validate-prior (prior args)
  "Throw an error if PRIOR is defined improperly with ARGS."
  (let* ((template (p-search-prior-template prior))
         (input-spec (p-search-prior-template-input-spec template)))
    ;; TODO - Implement cl-type checks for to be more robust
    (pcase-dolist (`(,id . _) input-spec)
      (unless (alist-get id args)
        (user-error "Input value `%s' not defined" id)))))

(defun p-search-transient-prior-create (template)
  ""
  (let* ((args (transient-args 'p-search-transient-dispatcher))
         (prior (p-search--instantiate-prior template args)))
    (p-search--validate-prior prior args)
    (setq p-search-priors (append p-search-priors (list prior)))
    ;; If the calculations have already been made, re-calculate
    (if (> (hash-table-count (p-search-prior-results prior)) 0)
        (p-search-calculate)
      (p-search--reprint))))

(defun p-search-transient-prior-edit (prior)
  "Replace PRIOR with new version given args from transient."
  (let* ((template (p-search-prior-template prior))
         (args (transient-args 'p-search-transient-dispatcher))
         (new-prior (p-search--instantiate-prior template args)))
    (p-search--validate-prior new-prior args)
    (setq p-search-priors (seq-map
                           (lambda (p)
                             (if (equal p prior)
                                 new-prior
                               p))
                           p-search-priors))
    ;; If the calculations have already been made, re-calculate
    (if (> (hash-table-count (p-search-prior-results new-prior)) 0)
        (p-search-calculate)
      (p-search--reprint))))

(defun p-search-transient-candidate-generator-create (generator)
  ""
  (let* ((args (transient-args 'p-search-transient-dispatcher)))
    (p-search--add-candidate-generator generator args)
    (p-search-restart-calculation)))

(defun p-search-transient-candidate-generator-edit (generator+args)
  "Edit the candidate generator of GENERATOR+ARGS with new transient args."
  (let* ((args (transient-args 'p-search-transient-dispatcher)))
    (p-search--replace-candidate-generator generator+args args)
    (p-search-restart-calculation)))

(defun p-search--resolve-spec (spec)
  "For each key in SPEC, if it is a function, call it and return resulting spec."
  (let* ((transient-type (car spec))
         (spec-props (cdr spec)))
    (cons
     transient-type
     (cl-loop for (key value) on spec-props by 'cddr
              append (list key (if (functionp value) (funcall value) value))))))

(defun p-search-read-default-spec-value (name+spec)
  (let* ((name (car name+spec))
         (spec (p-search--resolve-spec (cdr name+spec)))
         (default-value (plist-get (cdr spec) :default-value)))
    (or default-value
        (let* ((prompt (format "%s:" name))
               (reader (oref (get (car spec) 'transient--suffix) :reader)))
          (if reader
              (funcall reader prompt nil nil)
            (cond
             ;; TODO - rething how this is done
             ((p-search--choices-p (get (car spec) 'transient--suffix))
              (let* ((choices (plist-get (cdr spec) :choices)))
                (intern (completing-read prompt choices nil t))))))))))

(defun p-search-dispatch-add-prior (template)
  "Dispatch transient menu for prior template TEMPLATE."
  (let* ((input-specs (p-search-prior-template-input-spec template))
         (option-specs (p-search-prior-template-options-spec template)))
    (apply #'p-search-dispatch-transient
           `(["Input"
              ,@(seq-map
                 (lambda (name+spec)
                   (let* ((default-value (p-search-read-default-spec-value name+spec)))
                     (p-search--transient-suffix-from-spec name+spec t default-value)))
                         input-specs)]
             ["Options"
              ,@(seq-map (lambda (name+spec)
                           (let* ((name (car name+spec))
                                  (spec (p-search--resolve-spec (cdr name+spec))))
                             (p-search--transient-suffix-from-spec (cons name spec) nil)))
                         option-specs)
              ("-c" "complement"
               p-search-infix-toggle
               :init-state nil
               :option-symbol complement)
              ("-i" "importance"
               p-search-infix-choices
               :choices ,p-search-importance-levels
               :init-choice medium
               :option-symbol importance)]
             ["Actions"
              ("c" "create"
               (lambda ()
                 (interactive)
                 (p-search-transient-prior-create ,template)))]))))

(defun p-search-dispatch-edit-prior (prior)
  "Dispatch transient menu for PRIOR."
  (let* ((template (p-search-prior-template prior))
         (args (p-search-prior-arguments prior))
         (input-specs (p-search-prior-template-input-spec template))
         (option-specs (p-search-prior-template-options-spec template)))
    (apply #'p-search-dispatch-transient
           `(["Input"
              ,@(seq-map
                 (lambda (name+spec)
                   (let* ((name (car name+spec))
                          (spec (p-search--resolve-spec (cdr name+spec)))
                          (default-value (alist-get name args)))
                     (p-search--transient-suffix-from-spec (cons name spec) t default-value)))
                 input-specs)]
             ["Options"
              ,@(seq-map (lambda (name+spec)
                           (let* ((name (car name+spec))
                                  (spec (p-search--resolve-spec (cdr name+spec)))
                                  (default-value (alist-get name args)))
                             (p-search--transient-suffix-from-spec (cons name spec) nil default-value)))
                         option-specs)
              ("-c" "complement"
               p-search-infix-toggle
               :init-state nil
               :option-symbol complement)
              ("-i" "importance"
               p-search-infix-choices
               :choices ,p-search-importance-levels
               :init-choice medium
               :option-symbol importance)]
             ["Actions"
              ("e" "edit"
               (lambda ()
                 (interactive)
                 (p-search-transient-prior-edit ,prior)))]))))

(defun p-search-dispatch-add-candidate-generator (candidate-generator)
  "Dispatch transient menu for creating CANDIDATE-GENERATOR."
  (let* ((input-specs (p-search-candidate-generator-input-spec candidate-generator))
         (option-specs (p-search-candidate-generator-options-spec candidate-generator)))
    (apply #'p-search-dispatch-transient
           `(["Input"
              ,@(seq-map
                 (lambda (name+spec)
                   (let* ((name (car name+spec))
                          (spec (p-search--resolve-spec (cdr name+spec)))
                          (reader (oref (get (car spec) 'transient--suffix) :reader))
                          (default-value
                           (or (plist-get (cdr spec) :default-value)
                            (funcall reader (format "%s:" name) nil nil))))
                     (p-search--transient-suffix-from-spec (cons name spec) t default-value)))
                 input-specs)]
             ["Options"
              ,@(seq-map (lambda (name+spec)
                           (let* ((name (car name+spec))
                                  (spec (p-search--resolve-spec (cdr name+spec))))
                             (p-search--transient-suffix-from-spec (cons name spec) nil)))
                         option-specs)]
             ["Actions"
              ("c" "create"
               (lambda ()
                 (interactive)
                 (p-search-transient-candidate-generator-create ,candidate-generator)))]))))

(defun p-search-dispatch-edit-candidate-generator (generator+args)
  "Dispatch transient menu for editing GENERATOR+ARGS."
  (let* ((candidate-generator (car generator+args))
         (input-specs (p-search-candidate-generator-input-spec candidate-generator))
         (option-specs (p-search-candidate-generator-options-spec candidate-generator)))
    (apply #'p-search-dispatch-transient
           `(["Input"
              ,@(seq-map
                 (lambda (name+spec)
                   (let* ((name (car name+spec))
                          (spec (p-search--resolve-spec (cdr name+spec)))
                          (reader (oref (get (car spec) 'transient--suffix) :reader))
                          (default-value
                           (or (plist-get (cdr spec) :default-value)
                            (funcall reader (format "%s:" name) nil nil))))
                     (p-search--transient-suffix-from-spec (cons name spec) t default-value)))
                 input-specs)]
             ["Options"
              ,@(seq-map (lambda (name+spec)
                           (let* ((name (car name+spec))
                                  (spec (p-search--resolve-spec (cdr name+spec))))
                             (p-search--transient-suffix-from-spec (cons name spec) nil)))
                         option-specs)]
             ["Actions"
              ("e" "edit"
               (lambda ()
                 (interactive)
                 (p-search-transient-candidate-generator-edit (quote ,generator+args))))]))))

(defun p-search-dispatch-select-prior ()
  "Dispatch transient menu for items in PRIOR-TEMPLATES."
  (let* ((prior-templates (p-search-relevant-prior-templates))
         (all-group-names (seq-map (lambda (tmpl)
                                     (symbol-name (p-search-prior-template-group tmpl)))
                                   prior-templates))
         (grouped-priors (seq-map
                          (lambda (group+templates)
                            (let* ((templates (cdr group+templates))
                                   (template-names (seq-map #'p-search-prior-template-name templates))
                                   (group (car group+templates))
                                   (group-name (symbol-name group)))
                              ;; example of the format we're trying to put the data in:
                              ;; [["Buffer"
                              ;;   ("b n" "buffer name"
                              ;;    (lambda () (interactive) (myfunc)))]]
                              (vector
                               (seq-into
                                `(,group-name
                                  ,@(seq-map
                                     (lambda (template)
                                       (list (concat (p-search--unique-prefix
                                                      group-name
                                                      all-group-names)
                                                     " "
                                                     (p-search--unique-prefix
                                                      (p-search-prior-template-name template)
                                                      template-names))
                                             (p-search-prior-template-name template)
                                             `(lambda ()
                                                (interactive)
                                                (message "dispatching %s"
                                                         ,(p-search-prior-template-name template))
                                                (p-search-dispatch-add-prior
                                                 ,template))))
                                     templates))
                                'vector))))
                          (seq-group-by
                           #'p-search-prior-template-group
                           prior-templates))))
    (apply #'p-search-dispatch-transient grouped-priors)))


;;; Sections

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

(defun p-search-deepest-section-overlays-at-point ()
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
    (when-let* ((condenced-string (overlay-get overlay 'condenced-text))
                (info-ov (make-overlay ov-start eol-ov-start)))
      (overlay-put info-ov 'after-string condenced-string)
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
  (cl-with-gensyms (start end props key)
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


;;; Display of p-search Major Mode

;; This section contains the machinery for the p-search major mode.
;; The p-search major mode is for interacting with a search session.  The user
;; should be able to see an overview of what's being searched for and the
;; various priors being applied.  The p-search major mode is also used for
;; interacting with the various search results.

(defun p-search--document-hints (p-search-priors)
  "Return the documents hints for the current buffer."
  (let ((hints))
    (dolist (prior p-search-priors)
      (let ((prior-template (p-search-prior-template prior)))
        (when-let ((hint-func (p-search-prior-template-result-hint-function prior-template)))
          (let ((hint-ranges (funcall hint-func prior)))
            (setq hints (range-concat hints hint-ranges))))))
    hints))

(defun p-search--preview-from-hints (hints)
  "Return a string from current buffer highlighting the HINTS ranges."
  (let* ((output-string ""))
    (pcase-dolist (`(,start . ,end) hints)
      (add-text-properties start end '(face p-search-hi-yellow)))
    (let* ((added-lines '()))
      (pcase-dolist (`(,start . ,_end) hints)
        (goto-char start)
        (let* ((line-no (line-number-at-pos)))
          (when (not (member line-no added-lines))
            (let* ((line-str (buffer-substring (pos-bol) (pos-eol))))
              (push line-no added-lines)
              (setq output-string (concat output-string line-str "\n")))))))
    (concat
     (string-join
      (seq-take (string-split output-string "\n") p-search-document-preview-size)
      "\n")
     "\n")))

(defun p-search-document-preview (document)
  "Return preview string of DOCUMENT.
The number of lines returned is determined by `p-search-document-preview-size'."
  (let* ((document-contents (p-search-document-property document 'content))
         (buffer (generate-new-buffer "*test-buffer*"))
         (priors p-search-priors))
    (with-current-buffer buffer
      (insert document-contents)
      ;; propertize buffer according to filename
      (when (eql (car document) 'file)
        (let ((buffer-file-name (cadr document)))
          (set-auto-mode)))
      (goto-char (point-min))
      (let* ((hints (p-search--document-hints priors)))
        (if hints
            (progn
              (font-lock-fontify-region (point-min) (point-max))
              (p-search--preview-from-hints hints))
          ;; if there are no hints, just get the first n lines
          (let* ((start (point)))
            (forward-line p-search-document-preview-size)
            (font-lock-fontify-region start (point))
            (let ((res (buffer-substring start (point))))
              (if (and (> (length res) 0)
                       (eql (aref res (1- (length res))) ?\n))
                  res
                (concat res "\n")))))))))

(defun p-search--add-candidate-generator (generator args)
  "Append GENERATOR with ARGS to the current p-search session."
  ;; Ensure that there is an alist entry in ARGS for every item in
  ;; input-spec of generator.
  (pcase-dolist (`(,key . _) (p-search-candidate-generator-input-spec generator))
    (unless (alist-get key args)
      (error "Unable to create candidate generator %s, missing arg %s"
             (p-search-candidate-generator-name generator)
             key)))
  (setq p-search-candidates-cache nil)
  (setq p-search-candidates-by-generator nil)
  (setq p-search-active-candidate-generators
        (append p-search-active-candidate-generators
                (list (cons generator args)))))

(defun p-search--replace-candidate-generator (old-generator+args new-args)
  "Replace OLD-GENERATOR+ARGS with NEW-ARGS."
  (pcase-dolist (`(,key . _) (p-search-candidate-generator-input-spec (car old-generator+args)))
    (unless (alist-get key new-args)
      (error "Unable to create candidate generator %s, missing arg %s"
             (p-search-candidate-generator-name (car old-generator+args))
             key)))
  (setq p-search-candidates-cache nil)
  (setq p-search-candidates-by-generator nil)
  (setq p-search-active-candidate-generators
        (seq-map
         (lambda (generator+args)
           (if (equal generator+args old-generator+args)
               (cons (car old-generator+args) new-args)
             generator+args))
         p-search-active-candidate-generators)))

(defun p-search-initialize-session-variables ()
  "Instantiate the session-specific local variables."
  ;; (setq p-search-observations (make-hash-table :test 'equal))
  (setq p-search-observations (make-hash-table :test #'equal))
  (setq p-search-candidates-cache nil)
  (setq p-search-candidates-by-generator nil)
  (setq p-search-active-candidate-generators nil)
  (setq p-search-priors nil))

(defun p-search--setup-candidate-generators ()
  "Setup initial candidate generators for session."
  ;; TODO: figure out how I want the user to specify desired initial priors.
  (p-search--add-candidate-generator p-search-candidate-generator-buffers '()))

(defun p-search--display-columns ()
  "Return a list of two numbers: the start of column 2 and the end of column 2."
  (let* ((body-width (window-body-width))
         (page-width (min 100 body-width)))
    (list
     page-width
     (- page-width 12))))

(defun p-search--args-to-string (input-spec _options-spec args)
  "Return a string representing ARGS.
Use INPUT-SPEC and OPTIONS-SPEC for information on how to format
values of ARGS."
  (if (not args)
      ""
    (string-join
     (seq-map
      (pcase-lambda (`(,arg-sym . ,val))
        (format "%s: %s" arg-sym (propertize (format "%s" val) 'face 'p-search-value)))
      (seq-filter
       (pcase-lambda (`(,arg-sym . _))
         (assoc arg-sym input-spec))
       args))
     ", ")))

(defun p-search--insert-candidate-generator (generator+args)
  "Insert candidate GENERATOR+ARGS cons GENERATOR-ARGS into current buffer."
  (pcase-let* ((`(,generator . ,args) generator+args))
    (let* ((gen-name (p-search-candidate-generator-name generator))
           (in-spec (p-search-candidate-generator-input-spec generator))
           (opt-spec (p-search-candidate-generator-options-spec generator))
           (args-string (p-search--args-to-string in-spec opt-spec args))
           (docs (when p-search-candidates-by-generator (gethash (cons generator args) p-search-candidates-by-generator)))
           (heading-line (concat (propertize gen-name
                                             'face 'p-search-prior)
                                 (format " (%d)" (length docs)))))
      (p-search-add-section `((heading . ,heading-line)
                              (props . (p-search-candidate-generator ,(cons generator args)
                                        condenced-text ,(concat " (" args-string ")")))
                              (key . ,(cons generator args)))
        (pcase-dolist (`(,input-key . _) in-spec)
          (when-let (val (alist-get input-key args))
            (insert (format "%s: %s\n"
                            input-key
                            (propertize (format "%s" val) 'face 'p-search-value)))))
        (pcase-dolist (`(,opt-key . _) (append opt-spec '((complement . nil) (importance . nil))))
          (when-let (val (alist-get opt-key args))
            (insert (format "%s: %s\n"
                            opt-key
                            (propertize (format "%s" val) 'face 'p-search-value)))))))))

(defun p-search--insert-prior (prior)
  "Insert PRIOR into current buffer."
  (let* ((template (p-search-prior-template prior))
         (args (p-search-prior-arguments prior))
         (name (p-search-prior-template-name template))
         (in-spec (p-search-prior-template-input-spec template))
         (opt-spec (p-search-prior-template-options-spec template))
         (args-string (p-search--args-to-string in-spec opt-spec args))
         (importance (alist-get 'importance args))
         (importance-char (alist-get importance '((critical . "!")
                                                  (high . "H")
                                                  (medium . "M")
                                                  (low . "L")
                                                  (none . "-"))))
         (complement (alist-get 'complement args))
         (complement-char (if complement (propertize "-" 'face '(:weight extra-bold)) " "))
         (heading-line (concat complement-char (or importance-char " ") " "
                               (propertize name 'face 'p-search-prior)))
         (condenced (concat " (" args-string ")")))
    (p-search-add-section
        `((heading . ,heading-line)
          (props . (p-search-prior ,prior condenced-text ,condenced))
          (key . ,prior))
      (pcase-dolist (`(,input-key . _) in-spec)
        (when-let (val (alist-get input-key args))
          (insert (format "%s: %s\n"
                          input-key
                          (propertize (format "%s" val) 'face 'p-search-value)))))
      (pcase-dolist (`(,opt-key . _) (append opt-spec '((complement . nil) (importance . nil))))
        (when-let (val (alist-get opt-key args))
          (insert (format "%s: %s\n"
                          opt-key
                          (propertize (format "%s" val) 'face 'p-search-value))))))))

(defun p-search--insert-results ()
  "Insert the search results into current buffer."
  (when (not p-search-posterior-probs)
    (insert "no results calculated...\n"))
  (when p-search-posterior-probs
    (let* ((top-results (p-search-top-results))
           (page-dims (p-search--display-columns)))
      (p-search-add-section
          `((heading . ,(propertize
                         (format "Search Results (%d)" (heap-size p-search-posterior-probs))
                         'face 'p-search-section-heading))
            (props . (p-search-results t))
            (key . p-search-results-header))
        (when (= (length top-results) 0)
          (insert (propertize "No results exist.  Add a candidate generator with \"c\"\nto provide the candidates to search from." 'face 'shadow))) ;; TODO "c" to keybinding face
        (pcase-dolist (`(,document ,p) top-results)
          (let* ((doc-title (p-search-document-property document 'title))
                 (heading-line-1 (concat
                                  (substring (propertize (or doc-title "???") 'face 'p-search-header-line-key)
                                             (max (- (length doc-title) (cadr page-dims))
                                                  0))))
                 (heading-line (concat
                                heading-line-1
                                (make-string (- (cadr page-dims) (length heading-line-1)) ?\s)
                                (format "%.10f" (/ p p-search-marginal))))) ;; TODO Divide by marginal prob
            ;; TODO: figure out what to do with too long names
            (p-search-add-section `((heading . ,heading-line)
                                    (props . (p-search-result ,document))
                                    (key . ,doc-title))
              (let* ((preview (p-search-document-preview document)))
                (insert preview)))))))))

(defun p-search--reprint ()
  "Redraw the current buffer from the session's state."
  (unless (derived-mode-p 'p-search-mode)
    (error "Unable to print p-search state of buffer not in p-search-mode"))
  (let* ((inhibit-read-only t)
         (at-line (line-number-at-pos))
         (occlusion-states '()))
    ;; TODO - occlusion states
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'p-search-key)
        (push (cons (overlay-get ov 'p-search-key)
                    (overlay-get ov 'p-search-section-hidden))
              occlusion-states))
      (delete-overlay ov))
    (erase-buffer)
    (p-search-add-section
        `((heading . ,(propertize (format "Candidate Generators (%d)"
                                          (length p-search-active-candidate-generators))
                                  'face 'p-search-section-heading))
          (props . (p-search-section-id candidate-generators)))
      (when (= 0 (length p-search-active-candidate-generators))
        (insert (propertize "Press \"c\" to add a candidate generator.\n\n"
                            'face 'shadow)))
      (dolist (generator-args p-search-active-candidate-generators)
        (p-search--insert-candidate-generator generator-args)))
    (insert "\n")
    (p-search-add-section `((heading . ,(propertize (format "Priors (%d)" (length p-search-priors))
                                                    'face 'p-search-section-heading))
                            (props . (p-search-section-id priors)))
      (unless p-search-priors
        (insert (propertize "No priors currently being applied.
Press \"a\" to add new search criteria.\n" 'face 'shadow)))
      (dolist (prior p-search-priors)
        (p-search--insert-prior prior)))
    ;; TODO - Toggle occluded sections
    (insert "\n")
    (p-search--insert-results)
    (goto-char (point-min))
    (forward-line (1- at-line))
    (save-excursion
      (let* ((ovs (overlays-in (point-min) (point-max))))
        (dolist (ov ovs)
          (let* ((key (overlay-get ov 'p-search-key))
                 (is-hidden (alist-get key occlusion-states nil nil #'equal)))
            (when is-hidden
              (goto-char (overlay-start ov))
              (p-search-toggle-section))))))))

(defun p-search-setup-buffer ()
  "Initial setup for p-search buffer."
  (let* ((buffer (generate-new-buffer "p-search")))
    (with-current-buffer buffer
      (p-search-mode)
      (p-search-initialize-session-variables))
    (let ((win (display-buffer buffer nil)))
      (select-window win))
    (with-current-buffer buffer
      (p-search--setup-candidate-generators)
      (p-search--reprint)
      (p-search-calculate))
    buffer))


;;; Commands

(defun p-search-toggle-section ()
  "Toggle the visibility of the section under the point."
  (interactive)
  (let* ((ov (p-search-deepest-section-overlays-at-point)))
    (when ov
      (if (overlay-get ov 'p-search-section-hidden)
          (p-search-reveal-section ov)
        (p-search-occlude-section ov)))))

(defun p-search-add-prior ()
  "Add a new prior to the current session."
  (interactive)
  (unless (derived-mode-p 'p-search-mode)
    (error "No current p-search session found"))
  (p-search-dispatch-select-prior))

(defun p-search-kill-entity-at-point ()
  "Remove the entity at point, be it a prior or candidate generator."
  (interactive)
  (when-let* ((prior (get-char-property (point) 'p-search-prior)))
    (p-search--remove-prior prior))
  (when-let* ((prior (get-char-property (point) 'p-search-candidate-generator)))
    (p-search--remove-candidate-generator prior)))

(defun p-search-edit-dwim ()
  "Edit the entity at point, be it a prior or candidate generator."
  (interactive)
  (when-let* ((prior (get-char-property (point) 'p-search-prior)))
    (p-search-dispatch-edit-prior prior))
  (when-let* ((prior (get-char-property (point) 'p-search-candidate-generator)))
    (p-search-dispatch-edit-candidate-generator prior)))

(defun p-search-add-candidate-generator ()
  "Add a new candidate generator to the current session."
  (interactive)
  (unless (derived-mode-p 'p-search-mode)
    (error "No current p-search session found"))
  (let* ((selections (seq-map
                      (lambda (gen)
                        (cons
                         (p-search-candidate-generator-name gen)
                         gen))
                      p-search-candidate-generators))
         (selection (completing-read "Generator: " selections nil t))
         (selected-generator (alist-get selection selections nil nil #'equal)))
    (p-search-dispatch-add-candidate-generator selected-generator)))

(defun p-search-add-dwim ()
  "Add a new thing depending on where point is."
  (interactive)
  (unless (derived-mode-p 'p-search-mode)
    (error "No current p-search session found"))
  (let* ((val (get-char-property (point) 'p-search-section-id)))
    (if (eql val 'candidate-generators)
        (p-search-add-candidate-generator)
      (p-search-add-prior))))

(defun p-search-refresh-buffer ()
  "Redraw the buffer of current session."
  (interactive)
  (unless (derived-mode-p 'p-search-mode)
    (error "No current p-search session found"))
  (p-search--reprint))

(defconst p-search-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (keymap-set map "a" #'p-search-add-dwim)
    (keymap-set map "e" #'p-search-edit-dwim)
    (keymap-set map "C" #'p-search-add-candidate-generator)
    (keymap-set map "g" #'p-search-refresh-buffer)
    ;; (keymap-set map "i" #'p-search-importance)
    (keymap-set map "k" #'p-search-kill-entity-at-point)
    ;; (keymap-set map "n" #'p-search-obs-not-relevant)
    ;; (keymap-set map "r" #'p-search-reinstantiate-prior)
    (keymap-set map "P" #'p-search-add-prior)
    (keymap-set map "<tab>" #'p-search-toggle-section)
    ;; (keymap-set map "<return>" #'p-search-find-file)
    ;; (keymap-set map "C-o" #'p-search-display-file)
    ;; (keymap-set map "1" #'p-search-show-level-1)
    ;; (keymap-set map "2" #'p-search-show-level-2)
    ;; (keymap-set map "3" #'p-search-show-level-3)
    map)
  "Mode-map for p-search-mode.")

(defun p-search-post-command-hook ()
  "Post-command-hook for p-search mode."
  (p-search-highlight-point-section))

(define-derived-mode p-search-mode special-mode "p-search2"
  "Major mode for p-search."
  :group 'p-search
  (hack-dir-local-variables-non-file-buffer)
  (add-hook 'post-command-hook #'p-search-post-command-hook t t)
  (setq truncate-lines t))

(defun p-search ()
  "Start a p-search session."
 (interactive)
  (p-search-setup-buffer))


;;; Spec Helpers

;; A input/option specification is an alist of the form field-id to
;; cons of type and options, or visually as fallows:
;;
;; (name . (type . options-p-list))
;;
;; The following is an example specification:
;;
;; '((ignore-pattern . (regexp
;;                      :key "-i"
;;                      :description "Ignore Patterns"
;;                      :multiple t))  ;; TODO - implement multiple
;;   (use-git-ignore . (toggle
;;                      :key "-g"
;;                      :description "Git Ignore"
;;                      :default on)))
;;
;; Input refers to required parameters while options refers to
;; optional parameters.

;; always have :key, :description, :default-value
;; Input types:
;;   date+sigma
;;   number
;;   file
;;   memory
;;   regex
;;   choice :choices
;;   string
;;   toggle


(defun p-search--spec-default-arguments (spec)
  "Return default input and options of SPEC as one alist."
  (let* ((res '()))
    (pcase-dolist (`(,name . (,_type . ,options)) spec)
      (let* ((default (plist-get options :default))
             (default-val (if (functionp default) (funcall default) default)))
        (setq res (cons
                   (cons name default-val)
                   res))))
    (nreverse res)))


;;; Default Options

(add-to-list 'p-search-candidate-generators p-search-candidate-generator-buffers)
(add-to-list 'p-search-candidate-generators p-search-candidate-generator-filesystem)
(add-to-list 'p-search-prior-templates p-search-prior-major-mode)
(add-to-list 'p-search-prior-templates p-search-prior-title)
(add-to-list 'p-search-prior-templates p-search-prior-subdirectory)
(add-to-list 'p-search-prior-templates p-search-prior-query)
(add-to-list 'p-search-prior-templates p-search-prior-git-author)
(add-to-list 'p-search-prior-templates p-search-prior-git-commit-frequency)

(provide 'p-search)

;;; p-search.el ends here
