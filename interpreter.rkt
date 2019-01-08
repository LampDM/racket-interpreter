#lang racket
;Datatypes
(struct const (int) #:transparent)
(struct bool (boolean) #:transparent)
(struct interval (a b) #:transparent)
(struct pair (e1 e2) #:transparent)
(struct nil () #:transparent)

;Structures
(struct if-then-else (b e1 e2) #:transparent)
(struct is-const? (e))
(struct is-bool? (e))
(struct is-interval? (e))
(struct is-pair? (e))
(struct is-nil? (e))
(struct negate (e))
(struct add (e1 e2))
(struct multiply (e1 e2))
(struct exponentiate (e))
(struct left (e))
(struct right (e))
(struct greater (e1 e2))
(struct intersect (e1 e2))

;Okolje
(struct with (vars e) #:transparent)
(struct valof (s))

;Funkcije, skripte, klici
(struct closure (env f) #:transparent)
(struct function (name farg body) )
(struct script (name body) #:transparent)
(struct call (e arg) #:transparent)

(define (iv e okolje)
  (letrec ([intr (lambda (e env)
    (cond
      [(const? e)
       (if (integer? (const-int e)) e (error "Syntax error - invalid const"))
       ]
      [(bool? e)
       (if (boolean? (bool-boolean e)) e (error "Syntax error - invalid bool"))
       ]
      [(interval? e)
       (if (<=(interval-a e)(interval-b e)) e (error "Syntax error - interval [a,b] must have a<=b"))
       ]
      [(pair? e) e]
      [(nil? e) e]
      ;-----------------------is-xxxx?----------------BEGIN
      [(is-bool?? e)
       (let ([ib-test (intr(is-bool?-e e) env) ])
         (if (bool? ib-test) (bool #t) (bool #f))
         )]
      [(is-const?? e)
       (let ([ic-test (intr(is-const?-e e) env)])
         (if (const? ic-test) (bool #t) (bool #f)))]
      [(is-nil?? e)
       (let ([in-test (intr(is-nil?-e e) env)])
         (if (nil? in-test) (bool #t) (bool #f)))]
      [(is-interval?? e)
       (let ([ii-test (intr(is-interval?-e e) env)])
         (if (interval? ii-test) (bool #t) (bool #f)))]
      [(is-pair?? e)
       (let ([ip-test (intr(is-pair?-e e) env)])
         (if (pair? ip-test) (bool #t) (bool #f)))]
      ;-----------------------is-xxxx?----------------END
      ;-----------------------if-then-else----------------BEGIN
      [(if-then-else? e)
       (let ([itec (intr(if-then-else-b e) env)])
         (if (bool? itec)
             (if (bool-boolean itec)
                 (intr (if-then-else-e1 e) env)
                 (intr (if-then-else-e2 e) env))
             (error "Syntax error - if statement - condition not boolean")))]
      ;-----------------------if-then-else----------------END

      ;-----------------------negate----------------BEGIN
      [(negate? e)
       ( let([ cont (negate-e e)])
          (cond
            [(bool? cont)
             (if (bool-boolean cont) (bool #f) (bool #t))]
            [(const? cont)
             (const (-(const-int cont)))]
            [(interval? cont)
             (interval (-(interval-b cont)) (-(interval-a cont)))]
            [(greater? cont)
             (letrec ([gc (intr cont env) ])
                 (intr (negate gc) env)
               )
             ] 
            [(if-then-else? cont)
             (letrec ([itec (intr cont env) ])
                 (intr (negate itec) env)
               )
             ]
            [#t (error "Syntax error - invalid negate argument")]
            ))]
                    
      ;-----------------------negate----------------END
      ;-----------------------add----------------BEGIN
      [(add? e)
       (let ([ v1 (intr (add-e1 e) env)]
             [ v2 (intr (add-e2 e) env)])
         (cond
           [(and (const? v1) (const? v2))
            (const (+(const-int v1)(const-int v2)))
            ]
           [(and (interval? v1) (interval? v2))
            (interval (+(interval-a v1)(interval-a v2)) (+(interval-b v1)(interval-b v2)))]
           [(and (interval? v1) (const v2))
            (interval (+(interval-a v1)(const-int v2))(+(interval-b v1)(const-int v2)))]
           [(and (const? v1) (interval? v2))
            (interval (+(interval-a v2)(const-int v1))(+(interval-b v2)(const-int v1)))]
           [#t (error "Syntax error - invalid add arguments")]
           ))]
      ;-----------------------add----------------END
      ;-----------------------multiply----------------BEGIN
      [(multiply? e)
       (let ([v1 (intr (multiply-e1 e) env)]
             [v2 (intr (multiply-e2 e) env)])
         (cond
           [(and (const? v1) (const? v2))
            (const (*(const-int v1)(const-int v2)))
            ]
           [(and (interval? v1) (interval? v2))
            (let ([ac (* (interval-a v1)(interval-a v2))]
                  [ad (* (interval-a v1)(interval-b v2))]
                  [bc (* (interval-b v1) (interval-a v2))]
                  [bd (* (interval-b v1) (interval-b v2))])
              (interval (min ac ad bc bd) (max ac ad bc bd)))
            ]
           [#t (error "Syntax error - invalid multiply arguments")]
                 
           ))]
      ;-----------------------multiply----------------END
      ;-----------------------exponentiate----------------BEGIN
      [(exponentiate? e)
       (let ([v (intr (exponentiate-e e) env)])
         (cond
           [(const? v) (const (expt (const-int v) (const-int v)))]
           [(interval? v) (interval (expt (interval-a v) (interval-a v)) (expt (interval-b v) (interval-b v)))]
           [#t (error "Sytax error - invalid exponentiate arguments")]
           ))]
      ;-----------------------exponentiate----------------END
      ;-----------------------extraction----------------BEGIN
      [(left? e)
       (let ([v (intr(left-e e) env)])
         (cond
           [(pair? v) (intr (pair-e1 v) env)]
           [(interval? v) (const(interval-a v))]
           [#t (error "Syntax error - invalid extraction left argument")]
           )
           
         )]
      [(right? e)
       (let ([v (intr(right-e e) env)])
         (cond
           [(pair? v) (intr(pair-e2 v) env)]
           [(interval? v) (const(interval-b v))]
           [#t (error "Syntax error - invalid extraction right argument")]
           )
         )]
      ;-----------------------extraction----------------END
      ;-----------------------greater----------------BEGIN
      [(greater? e)
       (let ([v1 (intr(greater-e1 e) env)]
             [v2 (intr(greater-e2 e) env)])
         (cond
           [(and(const? v1)(const? v2)) (if (>(const-int v1)(const-int v2)) (bool #t) (bool #f))]
           [(and(interval? v1)(interval? v2))
            (let ([w1 (-(interval-b v1)(interval-a v1))]
                  [w2 (-(interval-b v2)(interval-a v2))]
                  )
              (if (> w1 w2) (bool #t) (bool #f))
              )]
           [#t (error "Syntax error - invalid greater arguments")]
           ))]
      ;-----------------------greater----------------END
      ;-----------------------intersect----------------BEGIN
      [(intersect? e)
       (let ([v1 (intr(intersect-e1 e) env)]
             [v2 (intr(intersect-e2 e) env)])
         (cond
           [(and(interval? v1)(interval? v2))
            (let ([f2 (interval-b v1)]
                  [s1 (interval-a v2)]
                  [f1 (interval-a v1)]
                  [s2 (interval-b v2)]
                  )
              (if (or(> s1 f2)(> f1 s2)) (nil)
                  (interval (max f1 s1) (min f2 s2)))
              )]
           [#t (error "Syntax error - invalid intersect arguments")]
           ))]
      ;-----------------------intersect----------------END
      ;-----------------------with in valof----------------BEGIN  
      [(with? e)
       (if (hash? (with-vars e))(letrec ([updatevars (lambda (hl nh)
                              (if (not(empty? hl))
                                  (if (function? (cdr(car hl)))
                                  (updatevars (cdr hl) (hash-set nh (car(car hl)) (cdr(car hl))))
                                  (updatevars (cdr hl) (hash-set nh (car(car hl)) (intr (cdr(car hl)) env)))
                                  )
                                  nh))])                    
       (intr (with-e e) (updatevars (hash->list(with-vars e)) env))
         )
           (error "Syntax error - invalid vars at with")
           )
       ]
      [(valof? e)
       (if (hash-has-key? env (valof-s e))
       (hash-ref env (valof-s e))
       (nil))
       ]
      ;-----------------------with in valof----------------END
      ;-----------------------funkcije,skripte in klici----------------BEGIN
      [(function? e)
       (if (or(string?(function-name e))(not(function-name e)))
        (intr (with (hash (function-name e) e) (closure env e)) env)
        (error "Syntax error - invalid function name"))
        ]
      [(closure? e) 
       (letrec ([fun (closure-f e)]
                [optimiseshadowed (lambda (envir frgs)
                                    (if (empty? frgs)
                                        envir
                                        (optimiseshadowed (hash-remove envir (car frgs)) (cdr frgs))
                                        )
                                    )
                                    ]
                ); TODO optimization remove shadowed OK- remove not used inside lol? kaj pe če ima funkcija enako ime kot notranje uporabljena spremenljivka?
                 ;TODO neka funkcija ki ugotovi used inside
        (closure (optimiseshadowed env (function-farg fun)) fun)
        
        )
        ]
      [(script? e)
       (if (or(string?(script-name e))(not(script-name e)))
            e
           (error "Syntax error - invalid script name")
           )
       ]
      [(call? e)
       (letrec ([cont (intr(call-e e) env)]
                [args (call-arg e)]
                )
        (cond
          [(closure? cont)
            (letrec ([fargs (function-farg (closure-f cont))]
                     [setargs (lambda (as fas ht)
                                (if (=(length as)(length fas))
                                (if (or(empty? as)(empty? fas)) ht
                                    (setargs (cdr as) (cdr fas) (hash-set ht (car fas) (intr (car as) env))))
                                (error "Syntax error - invalid number of arguments")
                                ))]
                     [nenv (setargs args fargs (closure-env cont))]
                     [body (function-body (closure-f cont))]
                     )
              (intr (with nenv body) env)
              )
            ]
          [(function? cont)
           (intr (call (intr cont env) args) env)
           ]
          [(script? cont)
           (intr (script-body cont) env)
           ]
          ))
        ]     
      ;-----------------------funkcije,skripte in klici----------------END
      [#t (error "Syntax error - recieved unknown construct")]
      ))])
  (intr e okolje)))

;-----------------------definicije makrov----------------BEGIN
(define (subtract e1 e2) (add e1 (negate e2) ))
(define (lower e1 e2)  (greater e2 e1))
(define (equal e1 e2) (if-then-else (negate (greater e1 e2)) (if-then-else (negate (greater e2 e1)) (bool #t) (bool #f)) (bool #f)))
(define (andalso e1 e2) (if-then-else e1
                                      (if-then-else e2
                                                    (bool #t) (bool #f)) (bool #f)))
(define (oralso e1 e2) (negate (andalso (negate e1) (negate e2))))
(define (encloses i1 i2)
         (andalso (oralso (lower (left i1) (left i2)) (equal (left i1)(left i2)) ) (oralso (greater (right i1)(right i2)) (equal (right i1) (right i2)) ))
                
                
  )
         
;(iv (subtract (const 5) (const 5))(hash))
;(iv (lower (const 3) (const 5))(hash))
;(iv  (lower (const 22)(const 23))(hash))
;(iv (equal (add(const 5)(const 5)) (const 11)) (hash))
;(iv (if-then-else (bool #t) (subtract (const 3) (const 3)) (const 5))(hash))
;(iv (encloses (interval 1 100) (interval -5 5))(hash))
;(iv (andalso (bool #t) (bool #t))(hash))
;(iv (oralso (bool #f) (bool #f))(hash))

;-----------------------definicije makrov----------------END
;Invalid tests
;(iv (const #t)(hash))
;(iv (bool 3)(hash))

;is-bool? tests
;(iv (is-bool? (bool #t))(hash))
;(iv (is-bool? (bool #f))(hash))
;(iv (is-bool? (const 3))(hash))
;(iv (is-bool? (nil))(hash))

;is-const? tests
;(iv (is-const? (const 3))(hash))
;(iv (is-const? (const 5))(hash))
;(iv (is-const? (bool #f))(hash))
;(iv (is-const? (nil))(hash))

;is-nil? tests
;(iv (is-nil? (nil))(hash))
;(iv (is-nil? (const 5))(hash))

;is-interval? tests
;(iv (is-interval? (interval 1 2))(hash))
;(iv (is-interval? (pair 1 2))(hash))
;(iv (interval 3 2)(hash)) 

;is-pair? tests
;(iv (is-pair? (pair 1 2))(hash))
;(iv (is-pair? (interval 1 2))(hash))

;if-then-else tests
;(iv (if-then-else (bool #t) (const 3) (const 5))(hash))
;(iv (if-then-else (bool #f) (const 3) (const 5))(hash))
;(iv (if-then-else (bool #f) (const 3) (if-then-else (bool #t) (const 3) (const 5)))(hash))

;negate tests
;(iv (negate (bool #t))(hash))
;(iv (negate (bool #f))(hash))
;(iv (negate (const 3))(hash))
;(iv (negate (const -3))(hash))
;(iv (negate (const 0))(hash))
;(iv (negate (interval 1 2))(hash))
;(iv (negate (interval -1 -2))(hash))
;(iv (negate (pair 1 2))(hash))

;add tests
;(iv (add (const 3) (const 5))(hash))
;(iv (add (const 3) (const -5))(hash))
;(iv (add (interval 1 2) (interval 1 2))(hash))
;(iv (add (interval 1 2) (interval -3 -5))(hash))
;(iv (add (interval 1 2) (const 2))(hash))
;(iv (add (const 2) (interval 1 2))(hash))
;(iv (add (bool #t) (bool #f))(hash))

;multiply tests
;(iv (multiply (const 3) (const 3))(hash))
;(iv (multiply (interval 1 2) (interval 3 4))(hash))

;exponentiate tests
;(iv (exponentiate (const 3))(hash))
;(iv (exponentiate (interval 2 3))(hash))

;extraction tests
;(iv (left (pair (bool #t) (const 3)))(hash))
;(iv (left (interval 1 2))(hash))
;(iv (right (pair (const 2) (bool #f)))(hash))
;(iv (right (interval 1 2))(hash))

;greater tests
;(iv (greater (const 3) (const 5))(hash))
;(iv (greater (const 5) (const 3))(hash))
;(iv (greater (interval 1 2) (interval 1 100))(hash))
;(iv (greater (interval 1 100) (interval 1 2))(hash))
;(iv (greater (interval 1 2) (const 3))(hash))

;intersect tests
;(iv (intersect (interval 1 5) (interval 3 7))(hash))
;(iv (intersect (interval 3 7) (interval 1 5))(hash))
;(iv (intersect (interval 1 5) (interval 6 7))(hash))
;(iv (intersect (interval 6 7) (interval 1 3))(hash))
;(iv (intersect (interval -100 100) (interval 1 13))(hash))
;(iv (intersect (interval 1 13) (interval -100 100) )(hash))
;(iv (intersect (pair (const 3) (const 5)) (interval 1 3))(hash))

;variables tests with in valof
;(iv (with (hash "a" (const 3)) (valof "a")))
;(iv (with (hash "a" (const 3) "b" (const 5) "c" (add (const 5)(const 13)) ) (add (valof "c") (valof "a")))(hash))
;(iv (with (hash "a" (const 3)) (valof "b"))(hash))
;(iv (with (hash "a" (const 3)) (add (add (with (hash "b" (const 3)) (valof "b")) (with (hash "c" (const 3)) (valof "c")))(valof "a")))(hash))
;(iv (with (hash "a" (const 1)) (add (with (hash "a" (const 2)) (add (valof "a")(valof "a"))) (valof "a")))(hash))

;Ne dela? Feature?
                  ;(iv (with (hash "a" (const 3) "b" (const 5) "c" (add (valof "a")(const 13)) ) (add (valof "c") (valof "a")))(hash))

;(iv (with (hash "a" (const 3)) (with (hash "b" (const 3)) (add (valof "b") (valof "a")))))
;(iv (with (hash "a" (const 3)) (with (hash "b" (const 5) "a" (const 6) ) (add (valof "a")(valof "b"))) )  (hash))
;(iv (with (hash "a" (const 3)) (with (hash "b" (const 5) "a" (add (valof "a") (const 100)) ) (add (valof "a")(valof "b"))) )  (hash))    

;functions tests
;(iv (with (hash "b" (const 5))(with (hash "a" (const 3)) (function "testfun" (list "i") (const 5))))(hash))

;(iv (with (hash "var" (const 1337) "k" (function "testfun" (list "a" "b" "c") (add(valof "a")(valof "b")))) (call (valof "k") (list (const 1) (add(valof "var")(const 3)) (const 3))))(hash))

;(iv (with (hash "addfun" (function "addfun" (list "a" "b") (add(valof "a")(valof "b"))) ) (call (valof "addfun") (list (const 15) (const 15))))(hash))
;(iv (with (hash "a" 3 "fun" (function "fun" (list "a" "b") (add(valof "a")(valof "b"))) ) (call (valof "fun") (list (const 15) (const 15))))(hash))
;(iv (with (hash "fun" (function "fun" (list "a" "b") (if-then-else (greater (valof "a")(valof "b")) (call (valof "fun") (list (add (valof "a") (const -1))(valof "b")))(valof "a")) ))  (call (valof "fun") (list (const 20) (const 15))))(hash))
;Fibonacci test
;(iv (with (hash "fib" (function "fib" (list "n")                                   (if-then-else (greater (valof "n") (const 1)) (add (call (valof "fib") (list (add (valof "n") (const -1) )  ) )(call (valof "fib")(list (add (valof "n") (const -2) ) ))(hash))                                                 (if-then-else (greater (valof "n") (const 0)) (const 1) (const 0))))                  )                          (call (valof "fib") (list (const 13)))))
;(iv (with (hash   "fib" (function "fib" (list "n") (greater (valof "n") (const 5)))   )  (call (valof "fib") (list (const 3)))  )(hash))
;(iv    (with     (hash "n" (const 5))    (greater (valof "n") (const 3))     ))
;(iv      (with (hash) (call (function "lolcat" (list "a" "b") (add(valof "a")(valof "b"))) (list (const 5) (const 8))))(hash))

;1st working recursive function
;(iv (with (hash)      (call        (function "lolcat" (list "a" "b")                 (if-then-else (greater (valof "a")(valof "b"))                              (call (valof "lolcat") (list (add(valof "a")(const -1)) (valof "b") ))                             (valof "a")                                )                                    )        (list (const 222) (const 100))        )) (hash))

;Second fibonacci with macros + optimizacija dela z rekurzivnimi funkcijami
(iv (with (hash "n" (const 3)) (call
        (function "fib" (list "n")
                  (if-then-else (equal (valof "n") (const 0)) (const 0)
                                (if-then-else (equal (valof "n") (const 1)) (const 1)
                                              (add (call (valof "fib") (list (subtract (valof "n") (const 1))  )) (call (valof "fib") (list (subtract (valof "n") (const 2))  )) )
                                              ))
                  )
        (list (const 13))
        )
       )
       (hash))


;(iv (function "lolcat" (list "a" "b") (add(valof "a")(valof "b")))(hash))
;Invalid number of args
;(iv (with (hash "fun" (function "fun" (list "a") (valof "a"))) (call (valof "fun") (list  (const 5) (const 5)) ))             (hash))
;(iv (with (hash "fun" (function "fun" (list "a" "b") (valof "a"))) (call (valof "fun") (list  (const 5) ) ))             (hash))

;Invalid name of function
;(iv  (function "fun" (list "a" "b") (const 5))(hash))
;(iv  (function (const 5) (list "a" "b") (const 5))(hash))
;(iv  (function #t (list "a" "b") (const 5))(hash))
;(iv  (function #f (list "a" "b") (const 5))(hash))

;Script tests
;(iv     (with (hash "a" (const 5) "b" (const 10)) (script "test1" (const 5))          )     (hash))
;(iv (with (hash "a" (const 1) "b" (const 2)) (call (script "test1" (add(valof "a")(valof "b"))) (list) ) )(hash))

;(iv (call (function "lolcat" (list "a" "b") (call (valof "lolcat") (list (const 5)(const 5)))) (list (const 5) (const 5)))(hash))

;Optimization tests
(iv (with (hash "a" (const 1) "b" (const 2) "c" (const 3) "k" (const 4))  (function #f (list "a" "b") (valof "k") ))(hash))

;Jaka tests
(iv (call    (function "test_recursion" (list "a" "b") (if-then-else (greater (valof "b") (const 0))                                                           (multiply (valof "a") (call (valof "test_recursion") (list (valof "a") (add (valof "b") (const -1)))))                                                                                                                   (const 1)                                                           ))    (list (const 2) (const 3)))     (hash))

;Vprasanje glede (with (hash ))) tests
(iv
 (with (hash "a" (const 3) "b" (add (const 3)(valof "a")))
       (valof "b") )
 (hash))