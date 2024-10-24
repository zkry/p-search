;;; p-search-query.el --- Support for querys -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:


;;; Declarations, requirements:

(require 'cl-lib)

(require 'range)

(declare-function 'p-search-document-property "p-search.el")
(declare-function 'p-search-candidate-generator-term-frequency-function "p-search.el")
(declare-function 'p-search-document-property "p-search.el")

(defvar p-search-active-candidate-generators "p-search.el")



;;; Customs:

(defcustom p-search-default-near-line-length 3
  "Default max number of line differences to count for a near query.

For example, the query (fox bear)~ with this variable set to 3
would indicate that if the line of a found \"fox\" match minus
the line of a found \"bear\" match is greater than 3, the
nearness match wouldn't count."
  :group 'p-search-query
  :type 'integer)

(defcustom p-search-default-boost-amount 1.3
  "Default max number of line differences to count for a near query.

For example, the query (fox bear)~ with this variable set to 3
would indicate that if the line of a found \"fox\" match minus
the line of a found \"bear\" match is greater than 3, the
nearness match wouldn't count."
  :group 'p-search-query
  :type 'integer)


;;; Variables:

(defvar p-search-query-parse--tokens nil
  "Variable to be used dynamically when parsing.
Stores the list of tokens being parsed.")

(defvar p-search-query-parse--idx nil
  "Variable to be used dynamically when parsing.
Indicates which token we are currently considering.")




;;; Term Expansion:

(defun p-search-query-break-term (term)
  "Break TERM into sub-words."
  (if (string-blank-p term)
      '()
    (let* ((i 1)
           (first-char (aref term 0))
           (term-part (if (eq (char-syntax first-char) ?w)
                          (string first-char)
                        ""))
           (terms '()))
      (while (< i (length term))
        (let* ((prev-char (aref term (1- i)))
               (prev-char-w-p (eq (char-syntax prev-char) ?w))
               (prev-char-up-p (and prev-char-w-p
                                    (eq (get-char-code-property prev-char 'general-category) 'Lu)))
               (at-char (aref term i))
               (at-char-w-p (eq (char-syntax at-char) ?w))
               (at-char-up-p (and at-char-w-p
                                  (eq (get-char-code-property at-char 'general-category) 'Lu))))
          (when (not prev-char-w-p)
            (when (not (string-blank-p term-part))
              (push term-part terms))
            (setq term-part ""))
          (when (and prev-char-w-p
                     (not prev-char-up-p)
                     at-char-up-p)
            (push term-part terms)
            (setq term-part ""))
          (when at-char-w-p
            (setq term-part (concat term-part (string at-char)))))
        (cl-incf i))
      (when (not (string-blank-p term-part))
        (push term-part terms))
      (nreverse terms))))

(defun p-search-query-expand-term (term) ;; TODO - rename to start with p-search-query-
  "Return the expansions of string TERM."
  (let* ((term-parts (p-search-query-break-term term)))
    (cond
     ((null term-parts)
      (error "nil base term"))
     ((= (length term-parts) 1)
      `((q ,(propertize term 'p-search-case-insensitive t))))
     (t
      (let* ((camel (propertize (string-join term-parts "") 'p-search-case-insensitive t))
             (snake (propertize (string-join term-parts "_") 'p-search-case-insensitive t))
             (kebab (propertize (string-join term-parts "-")  'p-search-case-insensitive t)))
        (seq-filter
         #'identity
         (append (list `(q ,term)
                       (and (not (equal term camel)) `(boost (q ,camel) 0.7))
                       (and (not (equal term snake)) `(boost (q ,snake) 0.7))
                       (and (not (equal term kebab)) `(boost (q ,kebab) 0.7))
                       ;; `(near ,@term-parts)
                       )
                 (seq-map
                  (lambda (term-part)
                    `(boost (q ,(propertize term-part 'p-search-case-insensitive t)) 0.3))
                  term-parts))))))))


;;; Query Runner:

(defun p-search-query-dispatch (term finalize-func)
  "Run query TERM  Call FINALIZE-FUNC on obtained results."
  (pcase-let*
      ((combined-tf (make-hash-table :test #'equal))
       (n (length p-search-active-candidate-generators))
       (i 0)
       (callback (lambda (doc-to-tf)
                   ;; Since candidate-generators should be mutually
                   ;; exclusive, puthash is overwriting value, not adding.
                   (cl-loop for k being the hash-keys of doc-to-tf
                            using (hash-values v)
                            do (puthash k v combined-tf))
                   (cl-incf i)
                   (when (= i n)
                     (funcall finalize-func combined-tf)))))
    (pcase-dolist (`(,generator . ,args) p-search-active-candidate-generators)
      (let* ((tf-func (p-search-candidate-generator-term-frequency-function generator)))
        (funcall tf-func args term callback :case-insensitive t))))) ;; TODO - does this case-insensitive do anythign?

(defun p-search-query-and (results)
  "Return intersection of RESULTS, a vector of hash-tables."
  (let ((result (make-hash-table :test 'equal))
        (res1 (aref (aref results 0) 0)))
    (maphash
     (lambda (k v)
       (catch 'out
         (let* ((min-ct v)
                (i 1))
           (while (< i (length results))
             (let* ((val (gethash k (aref (aref results i) 0))))
               (unless val
                 (throw 'out nil))
               (when (< val min-ct)
                 (setq min-ct val)))

             (cl-incf i))
           ;; k is in all results
           (puthash k min-ct result))))
     res1)
    result))

(defun p-search-query-near (sub-elts results)
  "Return a result from RESULTS array where SUB-ELTS are near one another."
  (if (equal (length results) 1)
      results
    (let* ((intersection-documents (make-hash-table :test #'equal))
           (result-ht (make-hash-table :test #'equal)))
      (maphash
       (lambda (doc ct)
         (cl-loop for i from 1 to (1- (length results))
                  always (gethash doc (aref results i))
                  finally (puthash doc 0 intersection-documents)))
       (aref results 0))
      ;; now intersection-documents contains possible candidate documents for near
      (maphash
       (lambda (document _)
         (catch 'done
           (let* ((contents (p-search-document-property document 'content)))
             (with-temp-buffer
               ; add spaces to make sure the next-single-char-property-change loop works
               (insert " " contents " ")
               (goto-char (point-min))
               ;; 1. Mark all matches for earch sub element in for the text of current document
               (dolist (elt sub-elts)
                 (let* ((ranges (p-search--mark-query*
                                 elt
                                 (lambda (query) ;; TODO This function is used somewhere else, maybe extract it
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
                   (pcase-dolist (`(,start . ,end) ranges)
                     (add-text-properties start end `(near-elt ,elt)))))
               ;; 2. Perform nearness check
               (let* ((pos (point-min))
                      (elts-set (make-hash-table :test #'equal)))
                 (while (< pos (point-max))
                   (let ((next-pos (next-single-char-property-change pos 'near-elt)))
                     (when (> (- (line-number-at-pos next-pos) (line-number-at-pos pos))
                              p-search-default-near-line-length)
                       (setq elts-set (make-hash-table :test #'equal)))
                     (let ((near-elt (get-char-property next-pos 'near-elt)))
                       (when near-elt
                         (puthash near-elt t elts-set)
                         (when (= (hash-table-count elts-set) (length sub-elts))
                           (puthash document 1 result-ht)
                           (throw 'done nil))))
                     (setq pos next-pos))))))))
       intersection-documents)
      result-ht)))

(defun p-search-query-run (query finalize-func)
  "Dispatch processes according to QUERY syntax tree.
All processes are concluded by calling FINALIZE-FUNC with
resulting data hashmap."
  (pcase query
    (`(q ,elt) ;; use quote to make sure no further expansion is done
     (p-search-query-dispatch elt finalize-func))
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
     (let* ((expansion-terms (p-search-query-expand-term query)))
       (p-search-query-run `(terms ,@expansion-terms) finalize-func)))
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
        (let* ((result (if (vectorp result)
                           (aref result 0)
                         result)))
          (funcall
           finalize-func
           (p-search-query--metadata-add
            result :calc-type 'must))))))
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
            (boost-amt (or (nth 1 rest) 1.3)))
       (p-search-query-run
        boost-elt
        (lambda (result)
          (let* ((result (if (vectorp result)
                             (aref result 0)
                           result)))
            (funcall
             finalize-func
             (p-search-query--metadata-add
              result :boost boost-amt)))))))))


;;; Metadata Objects:

;; The need arose for objects like hash-tables to be enhanced with
;; metadata for the final score calculation.  In this package, I am
;; making the convention that an object with metadata is a list with
;; the element as the first item and all metadata as subsequent plist
;; items.

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


;;; Scoring:

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
         (let* ((size (p-search-document-property doc 'size))
                (score (* idf (/ (* count (+ k1 1))
                                 (+ count (* k1 (+ 1 (- b) (* b (/ (float size) avg-size)))))))))
           (puthash doc score scores)))
       (p-search-query--metadata-elt result-ht)))
    scores))

(defun p-search-query--flatten-vector (elts)
  (seq-into
   (seq-mapcat
    (lambda (elt)
      (if (vectorp elt)
          (p-search-query--flatten-vector elt)
        (list elt)))
    elts)
   'vector))

(defun p-search-query-bm25 (results N total-size)
  "Compute BM25 from RESULTS.
N is the number of documents and TOTAL-SIZE is the sum of the
sizes of all the documents."
  (let* ((results (p-search-query--flatten-vector results))
         (total-scores (make-hash-table :test #'equal))
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

(defun p-search-query-scores-to-p-linear (scores)
  "Convert SCORES hashtable to hashtable of probabilities.

SCORES is a hash-table of files to (BM25) scores.  There is no
predetermined range of scores, so this function performs a
min-max normalization so that files with a score fall in the
range of min-prob to max-prob (as defined in the function), while
files with no match (i.e. not in the SCORES hash-table), are
given a value of zero-prob."
  (let* ((max-score 0)
         (min-score most-positive-fixnum)
         (zero-prob 0.3)
         (min-prob 0.5)
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
         (puthash doc (+ min-prob
                         (* (/ (- score min-score) (- max-score min-score))
                            (- max-prob min-prob)))
                  results)))
     scores)
    (puthash :default zero-prob results)
    results))


;;; Query Parser:

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
                    (push `(QUOTED-TERM ,(buffer-substring-no-properties start (1- (point))))
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
     (p-search-query-parse--postfix term))
    (`(QUOTED-TERM ,term)
     `(q ,(p-search-query-parse--postfix term)))))

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
         (setq boost p-search-default-boost-amount)
         (p-search-query-parse--next-token)
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

(defun p-search-query-parse (query-string)
  "Parse QUERY-STRING and return its query AST."
  (let* ((tokens (p-search-query-tokenize query-string))
         (ast (p-search-query-parse-tokens tokens)))
    ast))


;;; Mark

(defun p-search--mark-query* (query mark-function)
  "Return intervals where QUERY matches content in current buffer."
  (pcase query
    (`(q ,elt)
     (funcall mark-function elt))
    (`(boost . ,rest)
     (p-search--mark-query* (car rest) mark-function))
    (`(terms . ,elts)
     (let* ((ress '()))
       (dolist (elt elts)
         (let* ((res (p-search--mark-query* elt mark-function)))
           (when res
             (setq ress (range-concat ress res)))))
       ress))
    ((cl-type string)
     (let* ((expansion-terms (p-search-query-expand-term query)))
       (p-search--mark-query* `(terms ,@expansion-terms) mark-function)))
    (`(and . ,elts)
     (let* ((ress '()))
       (dolist (elt elts)
         (let* ((res (p-search--mark-query* elt mark-function)))
           (when res
             (setq ress (range-concat ress res)))))
       ress))
    ;; (`(near . ,elts)
    ;;  (p-search-query-near* elts))
    (`(not ,_elt)
     (ignore))
    (`(must ,elt)
     (p-search--mark-query* elt mark-function))
    (`(must-not ,_elt)
     (ignore))))

(defun p-search-mark-query (query mark-function)
  "Dispatch terms of QUERY by MARK-FUNCTION to return match ranges."
  (let* ((parsed (p-search-query-parse query)))
    (p-search--mark-query* parsed mark-function)))


;;; API Functions

(defun p-search-query (query-string p-callback N total-size)
  "Dispatch query from QUERY-STRING.

This function should be called with N being the total number of
documents and TOTAL-SIZE being the sum of all documents' size.

When all processes finish and results are combined, P-CALLBACK is
called with one argument, the hashmap of documents to
probabilities."
  (let* ((ast (p-search-query-parse query-string))
         (cb (lambda (res)
               (let* ((scores (p-search-query-bm25 res N total-size))
                      (probs (p-search-query-scores-to-p-linear scores)))
                 (funcall p-callback probs)))))
    (p-search-query-run ast cb)))

(provide 'p-search-query)
;;; p-search-query.el ends here
