
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
      ; low int high int stride int
      ; low <=> high stride >= 1
      (if (> low high)
        '()
	(cons low (sequence (+ low stride) high stride))
      );if
    );body end

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs);body
 
  )
   
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(= (length xs) 0)  (error "list-nth-mod: empty list")]
        [#t
  (let ([i (remainder n (length xs))])
    (car (list-tail xs i)))]
   )
  )

(define (stream-for-n-steps stream n)
  (if (= n 0)
      (list)
      (let ([pr (stream)])
      (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1)))
        )
   )
  )

(define (funny-number-stream)
  (letrec
      ([f (lambda (prev) (let* ([cur (+ prev 1)]
                               [cur_final (if (= (remainder cur 5) 0) (- 0 cur) cur)]
                               )
                           (cons cur_final (lambda () (f cur)));cons as let body
                                                );let as lambda body
                          
            );lambda
          ]
       )
    (f 0)
   )
  )

(define (dan-then-dog)
  (letrec
      ([f (lambda (prev)
            (let ([pic_name
                   (if (= (remainder prev 2) 0)
                       "dan.jpg"
                       "dog.jpg"
                       )]
                  )
              (cons pic_name (lambda () (f (+ prev 1))));let body
             );lambda body
           );lambda
        ]
       )
  (f 0)
  ))

(define (stream-add-zero stream)
  (let (
   [
   pr (stream)]
   )
   (lambda () (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))
   );body
  )

(define (cycle-lists xs ys)
  (letrec (
           [f (lambda (as bs)
                (let ([cs (if (null? as) xs as)]
                      [ds (if (null? bs) ys bs)]
                      )
                  (cons (cons (car cs) (car ds))         (lambda () (f (cdr cs) (cdr ds)))           )
                 );let as body
                )
            ]
      )
   (lambda () (f xs ys))
   ))

(define (vector-assoc v vec)
  (letrec ([f
            (lambda (i)
              (cond [(not (< i (vector-length vec))) #f]
	            [(not (pair? (vector-ref vec i))) #f]
		    [(equal? v (car (vector-ref vec i))) (vector-ref vec i)]; Bug: don't use =, use equal. 
                    [#t (f (+ i 1))]
               )
              )
            ]
           )
    (f 0)
   )
  )

(define (cached-assoc xs n)
  (let ([vec (make-vector n #f)]
        [cur_i 0])
  (lambda (v)
    (begin
      ;(print "!the returned lambda begins to work!")
    (let ([res_fast (vector-assoc v vec)])
    (begin ;debug
      ;(print "we got res_fast now!")
      ;(print res_fast)
    (if 
         (not (equal? #f res_fast))
         res_fast
         (let ([res_slow (assoc v xs)])
	   (begin ;debug
	    ;(print res_slow)
           (if
            (not (equal? #f res_slow))
            (begin (vector-set! vec cur_i res_slow) (set! cur_i (remainder (+ cur_i 1) n)) res_slow)
            res_slow)
	    );begin
           );let res_slow
           
     );if
     );begin
    )));let & begin &lambda
  ));let & define

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([v1 e1]
           ;[v2 e2];at least once
           
       [loop (lambda (v2)
               (if
        (< v2 v1)
        (loop e2);go on
        #t;break
        ))]
       )
       (loop e2)
      
      );macro body
     ]
    ));sytax-rules & define-syntax