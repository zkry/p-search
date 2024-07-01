;;; p-search-query.el --- Query dispatcher for p-search -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(require 'range)

(declare-function p-search-document-size "p-search.el")
(declare-function p-search-generate-search-space "p-search.el")
(declare-function p-search-document-text "p-search.el")

(defun p-search-query--escape (string meta-chars)
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

(defun p-search-query-rg-escape (string)
  "Insert escape \\ characters in STRING based on Rust's regex parser (used in rg)."
  (p-search-query--escape string '(?\\ ?. ?+ ?\( ?\) ?| ?\[ ?\] ?\{ ?\} ?^ ?$ ?# ?& ?- ?~))) ;; ?* ?\?

(defun p-search-query-ag-escape (string)
  "Insert escape \\ characters in STRING based on PCRE regex pattern (used in ag)."
  (p-search-query--escape string '(?\\ ?^ ?$ ?. ?\[ ?| ?\( ?\) ?+ ?{))) ;; ?* ?\?

(defun p-search-query-grep-escape (string)
  "Insert escape \\ characters in STRING based on PCRE regex pattern (used in ag)."
  (p-search-query--escape string '(?\\ ?.)))

(defconst p-search-query-wildcards '((rg . "[^\w]")
                                     (ag . "[^\s]")
                                     (grep . "[^[:space:]]")))

(defun p-search-query--replace-wildcards (string)
  (let ((wildcard (or (alist-get p-search-query-tool p-search-query-wildcards)
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
  (let* ((escaped-string (p-search-query--replace-wildcards (p-search-query-grep-escape string))))
    (list (list (concat "\\<" escaped-string "\\>") :case-insensitive t)
          (concat "\\B"
                  (capitalize (substring escaped-string 0 1))
                  (substring escaped-string 1)))))

(defun p-search-query-rg--term-regexp (string)
  "Create a term regular expression from STRING.
A term regex is noted for marking boundary characters."
  (let* ((escaped-string (p-search-query--replace-wildcards (p-search-query-rg-escape string))))
    (list (list (concat "\\b" escaped-string "\\b") :case-insensitive t)
          (concat "[a-z]" ;; TODO - use not-in-word-boundary instead
                  (capitalize (substring escaped-string 0 1))
                  (substring escaped-string 1))))) ;; TODO - will these double-count camel case?

(defun p-search-query-ag--term-regexp (string)
  "Create a term regular expression from STRING for ag tool.
A term regex is noted for marking boundary characters."
  (let* ((escaped-string (p-search-query--replace-wildcards (p-search-query-ag-escape string))))
    (list (list (concat "\\b" escaped-string "\\b") :case-insensitive t)
          (concat "[a-z]" ;; TODO - use not-in-word-boundary instead
                  (capitalize (substring escaped-string 0 1))
                  (substring escaped-string 1)))))

(defun p-search-query-emacs--term-regexp (string)
  "Create a term regular expression from STRING.
A term regex is noted for marking boundary characters."
  (list (list (p-search-query--replace-wildcards (concat "\\<" string "\\>")) :case-insensitive t)
        (concat (capitalize (substring string 0 1))
                (substring string 1))))


(defun p-search-query-grep--command (term)
  "Return command line arguments for rg search of TERM."
  (let* ((case-insensitive (p-search-query--metadata-get term :case-insensitive)))
    `("grep" "-c" ,@(and case-insensitive '("--ignore-case")) ,(p-search-query--metadata-elt term) ".")))

(defun p-search-query-rg--command (term)
  "Return command line arguments for rg search of TERM."
  (let* ((case-insensitive (p-search-query--metadata-get term :case-insensitive)))
    `("rg" "--count-matches" "--color" "never" "-i" ,@(and case-insensitive '("-i")) ,(p-search-query--metadata-elt term))))

(defun p-search-query-ag--command (term)
  "Return command line arguments for ag search of TERM."
  (let* ((case-insensitive (p-search-query--metadata-get term :case-insensitive)))
    `("ag" "-c" "--nocolor" ,@(and case-insensitive '("-i")) ,(p-search-query--metadata-elt term))))

(defvar p-search-query-tool nil
  "Dynamic var used to determine which tool to use to dispatch commands.
Can take symbols `grep', `rg', or `ag'.")

(defvar p-seach-query-directories nil
  "Dynamic var used to determine which directories to search in.")

(defun p-search-query-commands (string)
  "Return list of runnable commands from STRING based on `p-search-query-tool'."
  (pcase p-search-query-tool
    ('grep (seq-map #'p-search-query-grep--command (p-search-query-grep--term-regexp string))) ;; TODO
    ('rg (seq-map #'p-search-query-rg--command (p-search-query-rg--term-regexp string)))
    ('ag (seq-map #'p-search-query-ag--command (p-search-query-ag--term-regexp string)))
    (_ (error "Unsupported tool `%s'" p-search-query-tool))))

(defun p-search-query-dispatch (string finalize-func)
  "Run query STRING.  Call FINALIZE-FUNC on obtained results."
  (let* ((directories (or p-seach-query-directories (list default-directory)))
         (commands (p-search-query-commands string))
         (file-counts (make-hash-table :test #'equal))
         (proc-complete-ct 0))
    (dolist (cmd commands)
      (dolist (default-directory directories)
        (let* ((buf (generate-new-buffer (format "*p-search %s*" p-search-query-tool)))) ;; TODO - better name
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
                   (when (= proc-complete-ct (* (length commands) (length directories)))
                     (funcall finalize-func file-counts))))))))))))

(defun p-search-query-and (results)
  "Return intersection of RESULTS, a vector of hash-tables."
  (let ((result (make-hash-table :test 'equal))
        (res1 (aref results 0)))
    (maphash
     (lambda (k v)
       (catch 'out
         (let* ((min-ct v)
                (i 1))
           (while (< i (length results))
             (let* ((val (gethash k (aref results i))))
               (unless val
                 (throw 'out nil))
               (when (< val min-ct)
                 (setq min-ct val)))

             (cl-incf i))
           ;; k is in all results
           (puthash k min-ct result))))
     res1)
    result))

;; let subqueries near dotimes

(defun p-search-query-near* (subqueries)
  "Return ranges in current buffer where SUBQUERIES are close to eachother."
  (let* ((ress '())
         (distance 15)) ;; TODO - make configurable
    (let* ((matches (seq-into (seq-map #'p-search-query-mark subqueries) 'vector)))
      (while (seq-every-p #'identity matches)
        (catch 'out
          (let* ((first-matches (seq-map #'car matches))
                 (ordered (seq-sort-by #'car #'< first-matches))
                 (betweens (seq-mapn (lambda (a b)
                                       (- (car b) (cdr a)))
                                     ordered (cdr ordered))))
            (dotimes (i (length betweens))
              ;; if any of the matching intervals are too big,
              ;; get the next matches of everything below the
              ;; too-big interval.
              (when (> (nth i betweens) distance)
                (dotimes (j (1+ i))
                  (let* ((elt (nth j ordered)))
                    (dotimes (k (length matches))
                      (when (equal elt (car (aref matches k)))
                        (aset matches k (cdr (aref matches k)))))))
                (throw 'out nil)))
            (push (cons (caar ordered)
                        (cdr (car (last ordered))))
                  ress)
            (dotimes (k (length matches))
              (when (equal (car ordered) (car (aref matches k)))
                (aset matches k (cdr (aref matches k)))))))))
    (setq ress (nreverse ress))
    ress))

(defun p-search-query-near (subqueries results)
  "Return subset of RESULTS where results of SUBQUERIES are close to eachother."
  (let* ((ress (make-hash-table :test #'equal))
         (intersect-results (p-search-query-and results)))
    (maphash
     (lambda (file _)
       (with-temp-buffer
         (insert-file-contents file)
         (let* ((matching-intervals (p-search-query-near* subqueries)))
           (puthash file (length matching-intervals) ress))))
     intersect-results)))

(defun p-search-query--metadata-add (elt md-key md-val)
  "Add metadata MD-KEY MD-VAL to ELT."
  (if (listp elt)
      (let* ((md (cdr elt))
             (new-md (plist-put md md-key md-val)))
        (cons elt new-md))
    (list elt md-key md-val)))

(defun p-search-query--metadata-get (elt md-key)
  "Return metadata value MD-KEY of ELT."
  (when (listp elt)
    (plist-get (cdr elt) md-key)))

(defun p-search-query--metadata-elt (elt)
  "Return original element of ELT."
  (if (listp elt)
      (car elt)
    elt))

(defun p-search-query-run (query &optional finalize-func)
  "Dispatch processes according to QUERY syntax tree.
All processes are concluded by calling FINALIZE-FUNC with
resulting data hashmap."
  (pcase query
    (`(terms . ,elts)
     (let ((results (make-vector (length elts) nil))
           (i 0))
      (dolist (elt elts)
        (p-search-query-run
         elt
         (lambda (result)
           (aset results i result)
           (cl-incf i)
           (when (= i (length elts))
             (funcall finalize-func results)))))))
    ((cl-type string)
     (p-search-query-dispatch query finalize-func))
    (`(and . ,elts)
     (let ((results (make-vector (length elts) nil))
           (i 0))
      (dolist (elt elts)
        (p-search-query-run
         elt
         (lambda (result)
           (aset results i result)
           (cl-incf i)
           (when (= i (length elts))
             (let* ((and-res (p-search-query-and results)))
               (funcall finalize-func and-res))))))))
    (`(near . ,elts)
     (let ((results (make-vector (length elts) nil))
           (i 0))
      (dolist (elt elts)
        (p-search-query-run
         elt
         (lambda (result)
           (aset results i result)
           (cl-incf i)
           (when (= i (length elts))
             (let* ((near-res (p-search-query-near elts results)))
               (funcall finalize-func near-res))))))))
    (`(not ,elt)
     (p-search-query-run
      elt
      (lambda (result)
        (funcall
         finalize-func
         (p-search-query--metadata-add
          result :calc-type 'not)))))
    (`(must ,elt)
     (p-search-query-run
      elt
      (lambda (result)
        (funcall
         finalize-func
         (p-search-query--metadata-add
          result :calc-type 'must)))))
    (`(must-not ,elt)
     (p-search-query-run
      elt
      (lambda (result)
        (funcall
         finalize-func
         (p-search-query--metadata-add
          result :calc-type 'must-not)))))
    (`(boost . ,rest)
     (let* ((boost-elt (car rest))
            (boost-amt (or (nth 1 rest) 1)))
       (p-search-query-run
        boost-elt
        (lambda (result)
          (funcall
           finalize-func
           (p-search-query--metadata-add
            result :boost boost-amt))))))))

(defun p-search-query-scan (query finalize-func)
  "Scan entire search-space to find QUERY.  Call FINALIZE-FUNC on obtained results."
  (let* ((docs (p-search-generate-search-space))
         (results (make-hash-table :test #'equal)))
    (dolist (doc docs)
      (let* ((doc-text (p-search-document-text doc)))
        (with-temp-buffer
          (insert doc-text)
          (let* ((intervals (p-search-query-mark query)))
            (puthash doc (length intervals) results)))))
    (funcall finalize-func (vector results))))

(defun p-search-query-bm25* (result-ht N total-size)
  "Calculate the BM25 scores of RESULT-HT, a map of file name to counts.
N is the total number of documents and TOTAL-SIZE is the sum of all files'
sizes."
  (let* ((docs-containing (hash-table-count (p-search-query--metadata-elt result-ht)))
         (total-counts 0)
         (scores (make-hash-table :test #'equal))
         (k1 1.2) ;; TODO - make customizable
         (b 0.75)) ;; TODO - make customizable
    (maphash (lambda (_ v) (cl-incf total-counts v)) (p-search-query--metadata-elt result-ht))
    (let* ((idf (log (+ (/ (+ N (- docs-containing) 0.5)
                           (+ docs-containing 0.5))
                        1)))
           (avg-size (/ (float total-size) N)))
      (maphash
       (lambda (doc count)
         (let* ((size (p-search-document-size doc))
                (score (* idf (/ (* count (+ k1 1))
                                 (+ count (* k1 (+ 1 (- b) (* b (/ (float size) avg-size)))))))))
           (puthash doc score scores)))
       (p-search-query--metadata-elt result-ht)))
    scores))

(defun p-search-query-bm25 (results N total-size)
  "Compute BM25 from RESULTS.
N is the number of documents and TOTAL-SIZE is the sum of the
sizes of all the documents."
  (let* ((total-scores (make-hash-table :test #'equal))
         (must-files (make-hash-table :test #'equal))
         (must-not-files (make-hash-table :test #'equal))
         (scores (seq-map (lambda (res)
                            (p-search-query-bm25* res N total-size))
                          results)))
    (dotimes (i (length scores))
      (let* ((count-ht (aref results i))
             (score-ht (nth i scores))
             (boost (or (p-search-query--metadata-get count-ht :boost) 1))
             (calc-type (p-search-query--metadata-get count-ht :calc-type)))
        (maphash
         (lambda (file score)
           (when (eql calc-type 'must)
             (puthash file t must-files))
           (when (eql calc-type 'must-not)
             (puthash file t must-not-files))
           (cond
            ((eql calc-type 'not)
             (puthash file (- (gethash file total-scores 0) (* score boost)) total-scores))
            (t
             (puthash file (+ (gethash file total-scores 0) (* score boost)) total-scores))))
         score-ht)
        (maphash
         (lambda (must-not-file _)
           (remhash must-not-file total-scores))
         must-not-files)
        (when (> (hash-table-count must-files) 0)
          (let* ((replace-total-scores (make-hash-table :test #'equal)))
            (maphash
             (lambda (must-file _)
               (puthash must-file (gethash must-file total-scores) replace-total-scores))
             must-files)
            (setq total-scores replace-total-scores)))))
    total-scores))



;;; Mark

(defun p-search-query-mark--term (string)
  "Find and return locations of STRING in buffer."
  (let* ((terms (p-search-query-emacs--term-regexp string))
         (ress '()))
    (dolist (term terms)
      (save-excursion
        (goto-char (point-min))
        (let* ((case-fold-search (p-search-query--metadata-get term :case-insensitive)))
          (while (search-forward-regexp (p-search-query--metadata-elt term) nil t)
            (push (cons (match-beginning 0) (match-end 0)) ress)))))
    (setq ress (nreverse ress))
    ress))

(defun p-search-query-mark (query)
  "Return intervals where QUERY matches content in current buffer."
  (pcase query
    (`(terms . ,elts)
     (let* ((ress '()))
       (dolist (elt elts)
         (let* ((res (p-search-query-mark elt)))
           (when res
             (setq ress (range-concat ress res)))))
       ress))
    ((cl-type string)
     (p-search-query-mark--term query))
    (`(and . ,elts)
     (let* ((ress '()))
       (dolist (elt elts)
         (let* ((res (p-search-query-mark elt)))
           (when res
             (setq ress (range-concat ress res)))))
       ress))
    (`(near . ,elts)
     (p-search-query-near* elts))
    (`(not ,_elt)
     (ignore))
    (`(must ,elt)
     (p-search-query-mark elt))
    (`(must-not ,_elt)
     (ignore))))

(defun p-search-query-scores-to-p-linear (scores)
  "Convert SCORES hashtable to hashtable of probabilities."
  ;; TODO - better algorithm...
  (let* ((max-score 0)
         (min-score most-positive-fixnum)
         (zero-prob 0.4)
         (max-prob 0.7)
         (results (make-hash-table :test #'equal)))
    (maphash
     (lambda (_doc score)
       (when (> score max-score)
         (setq max-score score))
       (when (< score min-score)
         (setq min-score score)))
     scores)
    (maphash
     (lambda (doc score)
       (if (= max-score min-score)
           (puthash doc max-prob results)
         (puthash doc (+ zero-prob
                         (* (/ (- score min-score) (- max-score min-score))
                            (- max-prob 0.5)))
                  results)))
     scores)
    (puthash :default zero-prob results)
    results))


;;; Query Parser

(defun p-search-query-tokenize (query-str)
  "Break QUERY-STR into consituent tokens."
  (let* ((tokens '()))
    (with-temp-buffer
      (insert query-str)
      (goto-char (point-min))
      (let* ((pos nil))
        (while (not (eobp))
          (cond
           ((and pos
                 (member (char-after (point)) '(?: ?\t ?\s ?~ ?@ ?! ?^ ?/ ?\" ?\( ?\)))) ;; don't stop at ?+ ?-
            (push `(TERM ,(buffer-substring-no-properties pos (point))) tokens)
            (setq pos nil))
           ((eql (char-after (point)) ?\t)
            (forward-char 1))
           ((eql (char-after (point)) ?\s)
            (forward-char 1))
           ((and (not pos) (member (char-after (point)) '(?+ ?-)))
            (push (intern (char-to-string (char-after (point)))) tokens)
            (forward-char 1))
           ((member (char-after (point)) '(?~ ?@ ?! ?^ ?/ ?:))
            (push (intern (char-to-string (char-after (point)))) tokens)
            (forward-char 1))
           ((eql (char-after (point)) ?\")
            (if (eql (char-after (1+ (point))) ?\")
                (forward-char 2) ;; Just skip the empty quoted string
              (let* ((start (1+ (point)))
                     (res (search-forward-regexp "[^\"]\"" nil t)))
                (if res
                    (push `(TERM ,(buffer-substring-no-properties start (1- (point))))
                          tokens)
                  (user-error "Unmatched quote at position %d" start)))))
           ((eql (char-after (point)) ?\()
            (push 'lparen tokens)
            (forward-char 1))
           ((eql (char-after (point)) ?\))
            (push 'rparen tokens)
            (forward-char 1))
           ((looking-at-p "AND\\>")
            (push 'and tokens)
            (forward-char 3))
           (t
            (when (not pos)
              (setq pos (point)))
            (forward-char 1))))
        (when pos
          (push `(TERM ,(buffer-substring-no-properties pos (point))) tokens))))
    (push 'eoq tokens)
    (nreverse tokens)))

(defvar p-search-query-parse--tokens nil
  "Variable to be used dynamically when parsing.
Stores the list of tokens being parsed.")
(defvar p-search-query-parse--idx nil
  "Variable to be used dynamically when parsing.
Indicates which token we are currently considering.")

(defun p-search-query-parse--at-token ()
  "When parsing p-search query, return the current token."
  (if (>= p-search-query-parse--idx (length p-search-query-parse--tokens))
      'eoq
    (aref p-search-query-parse--tokens p-search-query-parse--idx)))

(defun p-search-query-parse--peek-token ()
  "When parsing p-search query, return the peek token (i.e. one after at)."
  (if (>= p-search-query-parse--idx (1- (length p-search-query-parse--tokens)))
      'eoq
    (aref p-search-query-parse--tokens (1+ p-search-query-parse--idx))))

(defun p-search-query-parse--next-token ()
  "When parsing p-search query, move the parsing index forward by one token."
  (cl-incf p-search-query-parse--idx))

(defun p-search-query-parse-tokens (tokens)
  "Return query parse tree from TOKENS."
  (let* ((p-search-query-parse--tokens (seq-into tokens 'vector))
         (p-search-query-parse--idx 0)
         (statements '()))
    (while (not (eql (p-search-query-parse--at-token) 'eoq))
      (let* ((statement (p-search-query-parse--statement)))
        (when statement
          (push statement statements)))
      (p-search-query-parse--next-token))
    (setq statements (nreverse statements))
    `(terms ,@statements)))

(defun p-search-query-parse--statement ()
  "Parse a statement element (e.g. \"needle+\").
This method assumes that the parse context is setup (i.e. the
variables `p-search-query-parse--tokens' and
`p-search-query-parse--idx' are set)."
  (pcase (p-search-query-parse--at-token)
    ('!
     (p-search-query-parse--next-token)
     (let* ((statement (p-search-query-parse--statement)))
       `(not ,statement)))
    ('+
     (p-search-query-parse--next-token)
     (let* ((statement (p-search-query-parse--statement)))
       `(must ,statement)))
    ('-
     (p-search-query-parse--next-token)
     (let* ((statement (p-search-query-parse--statement)))
       `(must-not ,statement)))
    ('lparen
     (p-search-query-parse--next-token)
     (let* ((terms '()))
       (while (not (eql (p-search-query-parse--at-token) 'rparen))
         (pcase (p-search-query-parse--at-token)
           (`(TERM ,term)
            (push term terms))
           (token (error "Unexpected token %s" token)))
         (p-search-query-parse--next-token))
       (setq terms (nreverse terms))
       (p-search-query-parse--postfix terms)))
    (`(TERM ,term)
     (p-search-query-parse--postfix term))))

(defun p-search-query-parse--postfix (elt)
  "Parse any existing postfix elements of ELT.
If postfix elements exist, return ELT wrapped in the correct AST
structure."
  (when (and (listp elt) (= 1 (length elt)))
    (setq elt (car elt)))
  (let ((boost)
        (near))
    (while (member (p-search-query-parse--peek-token) '(~ ^))
      (pcase (p-search-query-parse--peek-token)
        ('~
         (setq near t)
         (p-search-query-parse--next-token))
        ('^
         (setq boost 1)
         (pcase (p-search-query-parse--peek-token)
           (`(TERM ,term)
            (when (string-match-p "[0-9]+" term)
              (setq boost (string-to-number term))
              (p-search-query-parse--next-token))))
         (p-search-query-parse--next-token))))
    (cond
     ((and boost near)
      (if (listp elt)
          `(boost (near ,@elt) ,boost)
        `(boost (loose ,elt) ,boost)))
     (boost
      (if (listp elt)
          `(boost (and ,@elt) ,boost)
        `(boost ,elt ,boost)))
     (near
      (if (listp elt)
          `(near ,@elt)
        `(loose ,elt)))
     (t
      (if (listp elt)
          `(and ,@elt)
        elt)))))



;;; API

(defun p-search-query-parse (query-string)
  "Parse QUERY-STRING and return its query AST."
  (let* ((tokens (p-search-query-tokenize query-string))
         (ast (p-search-query-parse-tokens tokens)))
    ast))

(defun p-search-query (query-string N total-size p-callback)
  "Dispatch query from QUERY-STRING.
QUERY-STRING's parse is used to dispatch process calls.  N is the
total number of files being considered and TOTAL-SIZE is the sum
of the size (in bytes) of all N files.

When all processes finish and results are combined, P-CALLBACK is
called with one argument, the hashmap of files to probabilities.

Callers of this function should bind `p-search-query-tool' to
determine which tool is used to search."
  (let* ((ast (p-search-query-parse query-string))
         (cb (lambda (res)
               (let* ((scores (p-search-query-bm25 res N total-size ))
                      (probs (p-search-query-scores-to-p-linear scores)))
                 (funcall p-callback probs)))))
    (if p-search-query-tool
        (p-search-query-run ast cb)
      (p-search-query-scan ast cb))))

(provide 'p-search-query)

;;; p-search-query.el ends here
