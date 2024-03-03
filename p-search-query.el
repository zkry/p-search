;;; p-search-query.el --- Query dispatcher for p-search -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun p-search-query-escape-rg (string)
  (let* ((meta-chars '(?\\ ?. ?+ ?* ?\? ?\( ?\) ?| ?\[ ?\] ?\{ ?\} ?^ ?$ ?# ?& ?- ?~))
         (ret-str (make-string (* (length string) 2) 0))
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






"one two three"
'(terms "one" "two" "three")

"\"one two three\""
'(terms "one two three")

"student AND items"
'(terms (and "student" "items"))

"student !teacher"
'(terms "student" (not "teacher"))

"student^2"
'(terms (boost "student" 2))

"+student^"
'(terms (must (boost "student")))

"-(student teacher)"
'(terms (must-not (and "student" "teacher")))

"(student items)~"
'(terms (near "student" "items" ))

"(student items)~10w"
'(terms (near "student" "items" :distance 10 :unit word))

"title:.go"
'(terms (field "title" ".go"))

"string@items"
'(terms (syntax string "items"))

"teacher (string@items comment@this)~^ #title:(.html /.xmlx?/)"
'(terms "teacher"
        (boost
         (near (syntax string "items")
               (syntax comment "this")))
        (field "#title" (terms ".html"
                               (regexp ".xml?"))))

"comment@(this that)~"
'(terms (syntax comment (near "this" "that")))

;; terms: dispatch following terms
;;  "teacher": dispatch query for "teacher" ->
;;    <- a.txt: 2, b:txt: 3, y.txt: 1
;;  boost: run nested
;;    near: run each, find commonalities
;;      syntax string "items":
;;        <- test.go: 2, test2.go: 1
;;      syntax comment "this":
;;        <- test.go: 1, thest2.go: 3
;;      -> find intersection, manually look for near
;;      <- test.go: 1
;;    <- (test.go: 1 :modifiers (boost 2))
;;  field #title: run results and filter out manually
;;    terms:
;;      ".html": dispatch query for ".html"
;;        <- a.txt: 2, b.txt: 3
;;      (regexp ".xml?"): dispatch query for ".xml?"
;;        <- c.txt: 1
;;      <- a.txt: 2, b.txt: 3, c.txt: 1
;;     -> iterate results, running (terms ".html" (regexp ".xml?")) in emacs
;;        filtering if the match is in the correct field.

;; queries have two modes:
;; - locate :: must be able to return counts across search space.
;; - match  :: must be able to return match ranges for a particular item
;;
;; candidates must have the following methods:
;; - text :: return the text of candidate
;; - id   :: return the identifier of the candidate
;; - length :: return the length of the candidate
;;
;; Under normal circumstances, just the locate will be used for the
;; probability calculations.  Certain modifiers however may take the
;; union (terms) or intersection (and, near) and/or possibly run finer
;; grained filtering (syntax, field). The modified result counts will
;; be returned from this.  Take for example '(terms (syntax comment
;; (near "this" "that"))) The `near' directive will run queries on the
;; children elements, retrieving their file counts.  `near' will then
;; find their intersection and then for each element in the
;; intersection, call `match' for each element.  Then near will find
;; instances where they are close together, count them and return that
;; as its result.  Then, `syntax' will get the files where there are
;; near matches, go into them, call the match version of `near', and
;; then check if the match lies within the specified syntax.
;;
;; When the file results are displayed to the user, p-search will call
;; the match on the particular result, which would call the match
;; function of `syntax' which would call the match function of `near'.


(provide 'p-search-query)
;;; p-search-query.el ends here
