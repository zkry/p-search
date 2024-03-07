;;; p-search-query-tests.el --- tests for p-search-query.el -*- lexical-binding: t; -*-


;;; Code:

(ert-deftest p-search-query-test-tokenizer ()
  (pcase-dolist (`(,string ,want) '(("" (eoq))
                                    ("       " (eoq))
                                    ("some terms here" ((TERM "some") (TERM "terms") (TERM "here") eoq))
                                    ("\"one big term\"" ((TERM "one big term") eoq))
                                    ("student^" ((TERM "student") ^ eoq))
                                    ("student~" ((TERM "student") ~ eoq))
                                    ("student~^" ((TERM "student") ~ ^ eoq))
                                    ("+student~^ -teacher" (+ (TERM "student") ~ ^ - (TERM "teacher") eoq))
                                    ("(+in^ -parenthesis)^~" (lparen + (TERM "in") ^ - (TERM "parenthesis") rparen ^ ~ eoq))
                                    ("comment@text" ((TERM "comment") @ (TERM"text") eoq))
                                    ("title:text" ((TERM"title") : (TERM "text") eoq))
                                    ("plus-minus+not-end" ((TERM "plus-minus-not-end") eoq))
                                    ("test-term" ((TERM "test-term")))))
    (should (equal (p-search-query-tokenize string) want))))

(ert-deftest p-search-query-test-parser ()
  (pcase-dolist (`(,string ,want) '(("" (terms))
                                    ("test-term" (terms "test-term"))
                                    ("some terms here" (terms "some" "terms" "here"))
                                    ("\"one big term\"" (terms "one big term"))
                                    ("student^" (terms (boost "student" 1)))
                                    ("student~" (terms (loose "student")))
                                    ("student~^" (terms (boost (loose "student") 1)))
                                    ("+student~^ -teacher" (terms (must (boost (loose "student") 1)) (must-not "teacher")))

                                    ("(in parenthesis)^~" (terms (boost (near "in" "parenthesis") 1)))))
    (should (equal (p-search-query-parse string) want))))

(provide 'p-search-query-tests.el)
;;; p-search-query-tests.el ends here
