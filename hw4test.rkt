#lang racket
;; Programming Languages Homework4 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
(require "hw4.rkt")

(require rackunit)

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)

(define tests
  (test-suite
   "Sample tests for Assignment 4"
   
   ; sequence test
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test")
   (check-equal? (sequence 0 5 2) (list 0 2 4) "Sequence test")
   (check-equal? (sequence 0 5 5) (list 0 5) "Sequence test")
   (check-equal? (sequence 0 5 6) (list 0) "Sequence test")
   (check-equal? (sequence 0 1 1) (list 0 1) "Sequence test")
   (check-equal? (sequence 0 1 2) (list 0) "Sequence test")
   (check-equal? (sequence 0 0 1) (list 0) "Sequence test")
   (check-equal? (sequence 1 1 2) (list 1) "Sequence test")
   (check-equal? (sequence 2 1 1) (list) "Sequence test")
   (check-equal? (sequence 2 1 2) (list) "Sequence test")

   ; string-append-map test
   (check-equal? (string-append-map 
                  (list "dan" "dog" "curry" "dog2") 
                  ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test")
   (check-equal? (string-append-map 
                  (list) 
                  ".jpg") '() "string-append-map test")
   (check-equal? (string-append-map 
                  (list "dan") 
                  "") '("dan") "string-append-map test")
		  
   ; list-nth-mod test
   ;(check-equal? (list-nth-mod (list 0 1 2 3 4) -1) "list-nth-mod: negative number" "list-nth-mod test")
   ;(check-equal? (list-nth-mod (list) 0) "list-nth-mod: empty list" "list-nth-mod test")
   (check-equal? (list-nth-mod (list 0) 0) 0 "list-nth-mod test")
   (check-equal? (list-nth-mod (list 0) 1) 0 "list-nth-mod test")
   (check-equal? (list-nth-mod (list 0 1) 0) 0 "list-nth-mod test")
   (check-equal? (list-nth-mod (list 0 1) 1) 1 "list-nth-mod test")
   (check-equal? (list-nth-mod (list 0 1) 2) 0 "list-nth-mod test")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 5) 0 "list-nth-mod test")
   
   ; stream-for-n-steps test
   (check-equal? (stream-for-n-steps ones 0) (list) "stream-for-n-steps test")
   (check-equal? (stream-for-n-steps ones 1) (list 1) "stream-for-n-steps test")
   (check-equal? (stream-for-n-steps ones 2) (list 1 1) "stream-for-n-steps test")
   
   ; funny-number-stream test
   (check-equal? (stream-for-n-steps funny-number-stream 0) (list) "funny-number-stream test")
   (check-equal? (stream-for-n-steps funny-number-stream 1) (list 1) "funny-number-stream test")
   (check-equal? (stream-for-n-steps funny-number-stream 16) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test")

   ; dan-then-dog test
   (check-equal? (stream-for-n-steps dan-then-dog 1) (list "dan.jpg") "dan-then-dog test")
   (check-equal? (stream-for-n-steps dan-then-dog 2) (list "dan.jpg" "dog.jpg") "dan-then-dog test")
   (check-equal? (stream-for-n-steps dan-then-dog 3) (list "dan.jpg" "dog.jpg" "dan.jpg") "dan-then-dog test")

   ; stream-add-zero test
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 2) (list (cons 0 1) (cons 0 1)) "stream-add-zero test")
   (check-equal? (stream-for-n-steps (stream-add-zero funny-number-stream) 1) (list (cons 0 1)) "stream-add-zero test")
   (check-equal? (stream-for-n-steps (stream-add-zero funny-number-stream) 2) (list (cons 0 1) (cons 0 2)) "stream-add-zero test")
   (check-equal? (stream-for-n-steps (stream-add-zero dan-then-dog) 1) (list (cons 0 "dan.jpg")) "stream-add-zero test")

   ; cycle-lists test
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 1) (list (cons 1 "a")) "cycle-lists test") 
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 2) (list (cons 1 "a") (cons 2 "b")) "cycle-lists test")
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")) "cycle-lists test")
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 4) (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b")) "cycle-lists test")
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 5) (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b") (cons 2 "a")) "cycle-lists test")
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 6) (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b") (cons 2 "a") (cons 3 "b")) "cycle-lists test")
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 7) (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b") (cons 2 "a") (cons 3 "b") (cons 1 "a")) "cycle-lists test")

   ; vector-assoc test
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test")
   (check-equal? (vector-assoc 0 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1) #f)) #f "vector-assoc test")
   (check-equal? (vector-assoc 4 (vector)) #f "vector-assoc test")

   ; cached-assoc tests
   (let ([f (cached-assoc (list (cons 1 2) (cons 3 4)) 1)])
     (begin 
             ;(print "cached-assoc test begin!")
            ;(print f)
	    ;(print "cached-assoc test begin2!")
            (check-equal? (f 1) (cons 1 2) "abc")
            (check-equal? (f 2) #f "abc")
	    (check-equal? (f 1) (cons 1 2) "abc")
	    (check-equal? (f 3) (cons 3 4) "abc")
	    (check-equal? (f 4) #f "abc")
	    ))

   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test")
   (check-equal? ((cached-assoc (list) 1) 3) #f "cached-assoc test")
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 2) 2) #f "cached-assoc test")
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 1) 1) (cons 1 2) "cached-assoc test")

   ; while-less test
   (check-equal? (while-less 7 do (begin (set! a (+ a 1)) a)) #t "while-less test")
   (set! a 2)
   (check-equal? (while-less 7 do (begin (print "a") (set! a (+ a 1)) a)) #t "while-less test")
   
     
  ))

  (require rackunit/text-ui)
;; runs the test
(run-tests tests)