;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)

(define (racketlist->mupllist xs)
  (if
   (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))
   )
  )

;; Problem 2
(define (mupllist->racketlist xs)
  (if
   (aunit? xs)
      (list)
      (if
        (apair? xs)
	(cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))
	(error "FUNC: mupllist->racketlist, argument NOT struct pair. and this is it: " xs)
	)
   )
  )


(define (add-to-env env pr)
  (;let go of var shadowing
   cons pr env
   ))

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

(define (env-from-fv env fv)
  (env-from-fv-util env (set->list fv)

  )
  )

(define (env-from-fv-util env fv-list)
  (if (null? fv-list)
      null
      (cons (cons 
                 (car fv-list)
		 (envlookup env (car fv-list))
	     )
            (env-from-fv-util env (cdr fv-list))
      )
  )
)


;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond
        [(var? e) 
         (envlookup env (var-string e))]
        
         [(int? e)
         e         ]
         
         [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(ifgreater? e)
         (let (
               [v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)]
               ;[v3 ]
               ;[v4 ]
               )
           (if
            (and (int? v1)
                 (int? v2)
                 ;(int? v3)
                 ;(int? v4)
             )
            (if (> (int-num v1) (int-num v2)) (eval-under-env (ifgreater-e3 e) env) (eval-under-env (ifgreater-e4 e) env))
            ((error "MUPL ifgreater applied to non-number"))
            )
           )]

        [(mlet? e)
         (
          let ([var (mlet-var e)]
                [val (eval-under-env (mlet-e e) env)])
          (eval-under-env (mlet-body e) (add-to-env env (cons var val))   )
            
         )]

        [(fun? e)
         (let ([fname (fun-nameopt e)]
               [fpara (fun-formal e)])
           (if (and (or (string? fname) (equal? fname #f))
                    (string? fpara))
              ; (let ([fclosure (closure (add-to-env env (cons (string-append fname "_para") fpara)) (fun-body e) )]
              ;       )
                 ;(if (equal? fname #f)
                  ;   fclosure
                  ;   (closure (add-to-env (closure-env fclosure) (cons fname fclosure)) (fun-body e))
                  ;)
               ;  )
               (closure env e)
               (error "MUPL struct fun: function name and/or function parameter is/are NOT string (or #f for function name")
               )
         )]

        [(closure? e)
         e
         ]

        [(call? e) ;notice: no direct call, except recursive call
         (let ([cloj (eval-under-env (call-funexp e) env)]
                [argu (eval-under-env (call-actual e) env)]

                )
           (if (closure? cloj)
              (let* ( [cloj_env (closure-env cloj)]
                     [cloj_fun (closure-fun cloj)]
                     ;[ (envlookup cloj_env )
                     [pr_arg (cons (fun-formal cloj_fun) argu)]
                     [cloj_env_with_argu (add-to-env cloj_env pr_arg)]
                    )
               (eval-under-env
                 (fun-body cloj_fun) 
                (if (equal? (fun-nameopt cloj_fun) #f)
                    cloj_env_with_argu
                   (add-to-env cloj_env_with_argu (cons (fun-nameopt cloj_fun) cloj))
                   )
                       )
                )
              (error "MUPL call error: not a clojure")
              )
         )]

        [(apair? e)
         (let (
               ;[v0 (begin (print "===in apair, get e1 now---")(apair-e1 e))]
               [v1 ;(begin (print "===in apair, eval e1 now---")
                          (eval-under-env (apair-e1 e) env)]
               [v2 ;(begin (print "===in apair, eval e2 now---")
                (eval-under-env (apair-e2 e) env)]
               )
           (begin ;(print "===in apair, before making the pair---")
                  (apair v1 v2)
                  ;(print "===in apair, after making the pair---")
                  )
          )
         ]
       ; [(fst? e)
         ;(print "hello fst")
          ;(eval-under-env e env)
       ;  ]
        
         [(fst? e)
         (let (;[bug1 (begin (print "ffffffffffffffffff") 1)]
               [v 
                         (eval-under-env (fst-e e) env)]                             
               ;[bug2 (begin (print "gggggggggggggggggggg") 1)]
               )
           (if (apair? v)
           (apair-e1 v)
           (error "MUPL fst expects an apair")
           )
          )
         ]
         [(snd? e)
         (let (
               [v  (eval-under-env (snd-e e) env)]              
               )
           (if (apair? v)
           (apair-e2 v)
           (error "MUPL fst expects an apair")
           )
          )
         ]
         [
          (aunit? e)
          e
]
         [
          (isaunit? e)
          (let ([v (eval-under-env (isaunit-e e) env)])
          (if (aunit? v)
              (int 1)
           (int 0)
           ))
                   ]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

(define funcA (lambda () (funcB)))
(define funcB (lambda () 1))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)

  (ifgreater (isaunit e1) (int 0) e2 e3)
   )

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))
   )
  )


;(mlet var e e2)
 ; )

(define (ifeq e1 e2 e3 e4)

 ;ifgreater e1 e2 e4
 ;          (ifgreater e2 e1 e4 e3)

  ;or use mlet*
  
 (mlet "_x" e1
       (mlet "_y" e2
             (ifgreater (var "_x") (var "_y") e4
                        (ifgreater (var "_y") (var "_x") e4 e3))))
 
  )

;; Problem 4

(define mupl-map

(fun "mupl-map" "mupl-fun"
     (fun "mupl-map-curried" "mupl-list"
          (ifaunit (var "mupl-list")
                   (aunit)
                   (apair
                      (call (var "mupl-fun") (fst  (var "mupl-list")))
                      (call (var "mupl-map-curried") (snd (var "mupl-list")))
                   )
           )
      )
 )

  )

(define mupl-mapAddN 
  (mlet "map" mupl-map
        ;"CHANGE (notice map is now in MUPL scope)"
        (fun "mupl-mapAddN" "mupl-mapAddN-int"
          (call (var "map") (fun #f "mupl-mapAddN-item" (add (var "mupl-mapAddN-item") (var "mupl-mapAddN-int")))
           )
         )
        ))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment




(define (compute-free-vars e)
  (letrec ([compute-free-vars-util (lambda (e)
(cond



        [(var? e) (set (var-string e)   )]
        
         [(int? e) (set) ]
         
         [(add? e) (set-union (compute-free-vars-util (add-e1 e)) (compute-free-vars-util (add-e2 e)) )]
         
        ;; CHANGE add more cases here
        [(ifgreater? e) (set-union (compute-free-vars-util (ifgreater-e1 e)) (compute-free-vars-util (ifgreater-e2 e)) (compute-free-vars-util (ifgreater-e3 e)) (compute-free-vars-util (ifgreater-e4 e))
           )]

        [(mlet? e)
         (let ([var (mlet-var e)]
               [fv-var (compute-free-vars-util (mlet-e e))]
	       [fv (compute-free-vars-util (mlet-body e))])
	      (set-remove (set-union fv-var fv) var)    )]

        [(fun? e) (if (equal? (fun-nameopt e) #f)
                      (set-remove (compute-free-vars-util (fun-body e)) (fun-formal e))
                      (set-remove (set-remove (compute-free-vars-util (fun-body e)) (fun-formal e)) (fun-nameopt e)) )]
        ;[(closure? e) e ]
        [(call? e) (set-union (compute-free-vars-util (call-funexp e)) (compute-free-vars-util (call-actual e))          )]

        [(apair? e) (set-union (compute-free-vars-util (apair-e1 e))(compute-free-vars-util (apair-e2 e))    )]        
         [(fst? e) (compute-free-vars-util (fst-e e))]  
         [ (snd? e) (compute-free-vars-util (snd-e e))]
         [(aunit? e) (set)]
         [(isaunit? e) (compute-free-vars-util (isaunit-e e))]
   
        
        [#t (error (format "compute-free-vars-util: bad MUPL expression: ~v" e))]))])
    ;(compute-free-vars-util e)    ))


   
(cond

        [(var? e) e   ]
        
         [(int? e) e ]
         
         [(add? e) (add (compute-free-vars (add-e1 e)) (compute-free-vars (add-e2 e)) )]
         
        ;; CHANGE add more cases here
        [(ifgreater? e) (ifgreater (compute-free-vars (ifgreater-e1 e)) (compute-free-vars (ifgreater-e2 e)) (compute-free-vars (ifgreater-e3 e)) (compute-free-vars (ifgreater-e4 e))
           )]

        [(mlet? e) (mlet (mlet-var e) (compute-free-vars (mlet-e e)) (compute-free-vars (mlet-body e)) )    ]

        [(fun? e) (fun-challenge (fun-nameopt e) (fun-formal e) (compute-free-vars (fun-body e)) (compute-free-vars-util e))     ]
        ;[(closure? e) e ]
        [(call? e) (call (compute-free-vars (call-funexp e)) (compute-free-vars (call-actual e))         )]

        [(apair? e) (apair (compute-free-vars (apair-e1 e)) (compute-free-vars (apair-e2 e))    )]        
         [(fst? e) (fst (compute-free-vars (fst-e e)))]  
         [ (snd? e) (snd (compute-free-vars (snd-e e)))]
         [(aunit? e) e]
         [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
   
        
        [#t (error (format "compute-free-vars: bad MUPL expression: ~v" e))])))






;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) 
  (cond
        [(var? e) 
         (envlookup env (var-string e))]
        
         [(int? e)
         e         ]
         
         [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(ifgreater? e)
         (let (
               [v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)]
               ;[v3 ]
               ;[v4 ]
               )
           (if
            (and (int? v1)
                 (int? v2)
                 ;(int? v3)
                 ;(int? v4)
             )
            (if (> (int-num v1) (int-num v2)) (eval-under-env-c (ifgreater-e3 e) env) (eval-under-env-c (ifgreater-e4 e) env))
            ((error "MUPL ifgreater applied to non-number"))
            )
           )]

        [(mlet? e)
         (
          let ([var (mlet-var e)]
                [val (eval-under-env-c (mlet-e e) env)])
          (eval-under-env-c (mlet-body e) (add-to-env env (cons var val))   )
            
         )]

        [(fun-challenge? e)
         (let ([fname (fun-challenge-nameopt e)]
               [fpara (fun-challenge-formal e)])
           (if (and (or (string? fname) (equal? fname #f))
                    (string? fpara))
              ; (let ([fclosure (closure (add-to-env env (cons (string-append fname "_para") fpara)) (fun-challenge-body e) )]
              ;       )
                 ;(if (equal? fname #f)
                  ;   fclosure
                  ;   (closure (add-to-env (closure-env fclosure) (cons fname fclosure)) (fun-challenge-body e))
                  ;)
               ;  )
               ;(closure env e)
	       (closure (env-from-fv env (fun-challenge-freevars e)) e)
               (error "MUPL struct fun-challenge: function name and/or function parameter is/are NOT string (or #f for function name")
               )
         )]

        [(closure? e)
         e
         ]

        [(call? e) ;notice: no direct call, except recursive call
         (let ([cloj (eval-under-env-c (call-funexp e) env)]
                [argu (eval-under-env-c (call-actual e) env)]

                )
           (if (closure? cloj)
              (let* ( [cloj_env (closure-env cloj)]
                     [cloj_fun (closure-fun cloj)]
                     ;[ (envlookup cloj_env )
                     [pr_arg (cons (fun-challenge-formal cloj_fun) argu)]
                     [cloj_env_with_argu (add-to-env cloj_env pr_arg)]
                    )
               (eval-under-env-c
                 (fun-challenge-body cloj_fun) 
                (if (equal? (fun-challenge-nameopt cloj_fun) #f)
                    cloj_env_with_argu
                   (add-to-env cloj_env_with_argu (cons (fun-challenge-nameopt cloj_fun) cloj))
                   )
                       )
                )
              (error "MUPL call error: not a clojure")
              )
         )]

        [(apair? e)
         (let (
               ;[v0 (begin (print "===in apair, get e1 now---")(apair-e1 e))]
               [v1 ;(begin (print "===in apair, eval e1 now---")
                          (eval-under-env-c (apair-e1 e) env)]
               [v2 ;(begin (print "===in apair, eval e2 now---")
                (eval-under-env-c (apair-e2 e) env)]
               )
           (begin ;(print "===in apair, before making the pair---")
                  (apair v1 v2)
                  ;(print "===in apair, after making the pair---")
                  )
          )
         ]
       ; [(fst? e)
         ;(print "hello fst")
          ;(eval-under-env-c e env)
       ;  ]
        
         [(fst? e)
         (let (;[bug1 (begin (print "ffffffffffffffffff") 1)]
               [v 
                         (eval-under-env-c (fst-e e) env)]                             
               ;[bug2 (begin (print "gggggggggggggggggggg") 1)]
               )
           (if (apair? v)
           (apair-e1 v)
           (error "MUPL fst expects an apair")
           )
          )
         ]
         [(snd? e)
         (let (
               [v  (eval-under-env-c (snd-e e) env)]              
               )
           (if (apair? v)
           (apair-e2 v)
           (error "MUPL fst expects an apair")
           )
          )
         ]
         [
          (aunit? e)
          e
]
         [
          (isaunit? e)
          (let ([v (eval-under-env-c (isaunit-e e) env)])
          (if (aunit? v)
              (int 1)
           (int 0)
           ))
                   ]
        
        [#t (error (format "-c version: bad MUPL expression: ~v" e))]))




;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
