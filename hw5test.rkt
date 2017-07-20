#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "hw5.rkt")
(require rackunit)

(define tests
  (test-suite
   "Sample tests for Assignment 5"
   
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list)) (aunit) "racketlist->mupllist test")
   (check-equal? (racketlist->mupllist (list (int 3))) (apair (int 3) (aunit)) "racketlist->mupllist test")   
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")

   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (aunit)) (list) "racketlist->mupllist test")
   (check-equal? (mupllist->racketlist (apair (int 3) (aunit))) (list (int 3)) "racketlist->mupllist test")
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test")

   ;; tests struct int
   (check-equal? (eval-exp (int 3)) (int 3) "int test")


   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp (ifgreater (int 1) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
   (check-equal? (eval-exp (ifgreater (int 3) (int 3) (int 5) (int 9))) (int 9) "ifgreater test")
   (check-equal? (eval-exp (ifgreater (int 3) (int 2) (int 6) (int 2))) (int 6) "ifgreater test")
   ;(check-equal? (eval-exp (ifgreater (int 3) (var "x") (int 3) (int 2))) (int 2) "ifgreater test")

   ;; mlet test
   (check-equal? (eval-exp (mlet "x" (int 1) (var "x"))) (int 1) "mlet test")
   (check-equal? (eval-exp (mlet "xyz" (int 1) (add (int 5) (var "xyz")))) (int 6) "mlet test")
 
   ;; fun, closure, call test
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp (call (fun #f "x" (add (var "x") (int 7))) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp (call (mlet "myclos" (closure '() (fun #f "x" (add (var "x") (int 7)))) (var "myclos")) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp (call (fun "myfunc" "x" (ifgreater (var "x") (int 0) (int 8) (call (var "myfunc") (add (var "x") (int 1))))) (int 0))) (int 8) "call test")

    ;;apair test
   (check-equal? (eval-exp (apair (add (int 1) (int 0)) (int 2))) (apair (int 1) (int 2)) "apair test")

	;(begin (print "1st test!!!!"))
    ;;1st test
    (check-equal? 
      (eval-exp 
       (fst 
        (apair 
	   (add (int 1) (int 0)) (int 2)
	   )
	);fst
	)
	(int 1) 
	"fst test") 

    ;;snd test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")

      ;; isaunit test
      (check-equal? (eval-exp (aunit)) (aunit) "aunit test")
   (check-equal? 
      (eval-exp
       (isaunit 
         (closure '() (fun #f "x" (aunit))	 )
	 )
      )
       (int 0) "isaunit test")

     ; (check-equal? (eval-exp (aunit)) (aunit) "aunit test")
   (check-equal? 
      (eval-exp
       (isaunit 
        (call (fun #f "x" (aunit)) (int 1)	 )
	 )
      )
       (int 1) "isaunit test")
   
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   (check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3))) (int 2) "ifaunit test")

   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
   (check-equal? (eval-exp
     (mlet* 
       (list 
         (cons "x" (int 10))
	 (cons "y" (add (var "x") (int 5)))
	 (cons "z" (add (var "x") (var "y")))
         )

	 (add (add (var "x") (var "y")) (var "z"))
	 )) (int 50) "mlet* test")

   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")

   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)) "mupl-map test")

   

      ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   
   
#|
   (check-equal? 
   (compute-free-vars (var "x")) (set "x")  "compute-free-vars var test")
   (check-equal? (compute-free-vars (int 3)) (set) "int test")
   (check-equal? (compute-free-vars (ifgreater (var "y") (int 4) (int 3) (var "z"))) (set "y" "z") "ifgreater test")
   (check-equal? (compute-free-vars (mlet "x" (int 1) (var "y"))) (set "y") "mlet test")
   (check-equal? (compute-free-vars (mlet "xyz" (int 1) (add (int 5) (var "xyz")))) (set) "mlet test")
   (check-equal? (compute-free-vars (call (fun #f "x" (add (var "x") (int 7))) (int 1))) (set) "call test")
   (check-equal? (compute-free-vars (call (mlet "myclos" (fun #f "x" (add (var "x") (var "y"))) (var "myclos")) (int 1))) (set "y") "call test")
   (check-equal? (compute-free-vars (call (fun "myfunc" "x" (ifgreater (var "x") (int 0) (int 8) (call (var "myfunc") (add (var "x") (int 1))))) (int 0))) (set) "call test")
   (check-equal? (compute-free-vars (apair (add (int 1) (var "x")) (var "y"))) (set "x" "y") "apair test")
   (check-equal? 
      (compute-free-vars 
       (fst 
        (apair 
	   (add (var "x") (int 0)) (var "y")
	   )
	);fst
	)
	(set "x" "y") 
	"fst test") 

    ;;snd test
   (check-equal? (compute-free-vars (snd (apair (var "x") (var "y")))) (set "x" "y") "snd test")

      ;; isaunit test
      (check-equal? (compute-free-vars (aunit)) (set) "aunit test")
   (check-equal? 
      (compute-free-vars
       (isaunit 
         (call (fun #f "x" (aunit)) (int 1)	 )
	 )
      )
       (set) "isaunit test")


|#
    (check-equal? 
   (compute-free-vars (var "x")) (var "x")  "compute-free-vars var test")
   (check-equal? (compute-free-vars (int 3)) (int 3) "int test")
   (check-equal? (compute-free-vars (ifgreater (var "y") (int 4) (int 3) (var "z"))) (ifgreater (var "y") (int 4) (int 3) (var "z")) "ifgreater test")
   (check-equal? (compute-free-vars (mlet "x" (int 1) (var "y"))) (mlet "x" (int 1) (var "y")) "mlet test")
   (check-equal? (compute-free-vars (mlet "xyz" (int 1) (add (int 5) (var "xyz")))) (mlet "xyz" (int 1) (add (int 5) (var "xyz"))) "mlet test")
   (check-equal? (compute-free-vars (call (fun #f "x" (add (var "x") (int 7))) (int 1))) (call (fun-challenge #f "x" (add (var "x") (int 7)) (set)) (int 1)) "call test")
   (check-equal? (compute-free-vars (call (mlet "myfun" (fun #f "x" (add (var "x") (var "y"))) (var "myfun")) (int 1))) (call (mlet "myfun" (fun-challenge #f "x" (add (var "x") (var "y")) (set "y")) (var "myfun")) (int 1)) "call test")
   (check-equal? (compute-free-vars (call (fun "myfunc" "x" (ifgreater (var "x") (int 0) (int 8) (call (var "myfunc") (add (var "x") (int 1))))) (int 0))) (call (fun-challenge "myfunc" "x" (ifgreater (var "x") (int 0) (int 8) (call (var "myfunc") (add (var "x") (int 1)))) (set)) (int 0)) "call test")     
   (check-equal? (compute-free-vars (apair (add (int 1) (var "x")) (var "y"))) (apair (add (int 1) (var "x")) (var "y")) "apair test")
   (check-equal? 
      (compute-free-vars 
       (fst 
        (apair 
	   (add (var "x") (int 0)) (var "y")
	   )
	);fst
	)
	(fst 
        (apair 
	   (add (var "x") (int 0)) (var "y")
	   )
	);fst
	"fst test") 

    ;;snd test
   (check-equal? (compute-free-vars (snd (apair (var "x") (var "y")))) (snd (apair (var "x") (var "y"))) "snd test")

    ;; isaunit test
    (check-equal? (compute-free-vars (aunit)) (aunit) "aunit test")
   (check-equal? 
      (compute-free-vars
       (isaunit 
         (call (fun #f "x" (aunit)) (int 1)	 )
	 )
      )
       (isaunit 
         (call (fun-challenge #f "x" (aunit) (set)) (int 1)	 )
	 ) "isaunit test")



   (check-equal? (eval-exp-c (int 3)) (int 3) "int test")
   (check-equal? (eval-exp-c (ifgreater (int 1) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
   (check-equal? (eval-exp-c (ifgreater (int 3) (int 3) (int 5) (int 9))) (int 9) "ifgreater test")
   (check-equal? (eval-exp-c (ifgreater (int 3) (int 2) (int 6) (int 2))) (int 6) "ifgreater test")
   (check-equal? (eval-exp-c (mlet "x" (int 1) (var "x"))) (int 1) "mlet test")
   (check-equal? (eval-exp-c (mlet "xyz" (int 1) (add (int 5) (var "xyz")))) (int 6) "mlet test")
   ;(check-equal? (eval-exp-c (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")
   ;(print "---")
   (check-equal? (eval-exp-c (call (fun #f "x" (add (var "x") (int 7))) (int 1))) (int 8) "call test")
   ;(print "+++")
   (check-equal? (eval-exp-c (call (mlet "myclos" (fun #f "x" (add (var "x") (int 7))) (var "myclos")) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp-c (call (fun "myfunc" "x" (ifgreater (var "x") (int 0) (int 8) (call (var "myfunc") (add (var "x") (int 1))))) (int 0))) (int 8) "call test")
   (check-equal? (eval-exp-c (apair (add (int 1) (int 0)) (int 2))) (apair (int 1) (int 2)) "apair test")
    (check-equal? 
      (eval-exp-c 
       (fst 
        (apair 
	   (add (int 1) (int 0)) (int 2)
	   )
	);fst
	)
	(int 1) 
	"fst test") 
   (check-equal? (eval-exp-c (snd (apair (int 1) (int 2)))) (int 2) "snd test")
      (check-equal? (eval-exp-c (aunit)) (aunit) "aunit test")
   (check-equal? 
      (eval-exp-c
       (isaunit 
         (fun #f "x" (aunit))	 
	 )
      )
       (int 0) "isaunit test")
   (check-equal? 
      (eval-exp-c
       (isaunit 
        (call (fun #f "x" (aunit)) (int 1)	 )
	 )
      )
       (int 1) "isaunit test")

   (check-equal? (eval-exp-c (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   (check-equal? (eval-exp-c (ifaunit (aunit) (int 2) (int 3))) (int 2) "ifaunit test")

   (check-equal? (eval-exp-c (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
   (check-equal? (eval-exp-c
     (mlet* 
       (list 
         (cons "x" (int 10))
	 (cons "y" (add (var "x") (int 5)))
	 (cons "z" (add (var "x") (var "y")))
         )

	 (add (add (var "x") (var "y")) (var "z"))
	 )) (int 50) "mlet* test")

   (check-equal? (eval-exp-c (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp-c (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)) "mupl-map test")
   (check-equal? (mupllist->racketlist
   (eval-exp-c (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")

   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
