(

(define (null? list)
    (eq? nil list))

(define (map f list)
    (cond (null? list) nil
          't (cons (f (car list)) (map f (cdr list)))))

(define (filter f list)
    (cond (null? list) nil
          (f (car list)) (cons (car list) (filter f (cdr list)))
          't (filter f (cdr list))))

; have (apply fn (arg1 ... argn))
; want (fn arg1 ... argn), and for it to be evaluated
(define (apply f args)
    (__eval__ 
        (apply-prep f args)
        ; evaluate the expression within the current environment
        __env__))

(define (apply-prep f args)
        ; make sure that we don't evaluate the arguments while constructing the lambda
        (rev-reduce (cons f nil) 
                    (lambda (s arg) (cons (quote arg) s)) 
                    args))

; provide a function from (state elem) to state
(define (reduce state f data)
    (cond (null? data) state
          't (reduce (f state (car data)) f (cdr data))))

; just a reduce with a reverse at the end
(define (rev-reduce state f data)
    (reverse (reduce state f data)))

(define (reverse list)
    (reduce nil (lambda (rev x) (cons x rev)) list))



(main (apply reverse '((1 2 3))))

)