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

; have (apply fn (arg+))
; want (fn arg1 ... argn)
; (define (apply fn args)
;     (cond (null? args) nil
;           (cons)))

(define (reverse list)
    (reverse-tail-rec list nil))

(define (reverse-tail-rec list rev)
    (cond (null? list) rev
          't (reverse-tail-rec (cdr list) (cons (car list) rev))))

; (incorrectly) concatenate two lists
(define (concat l1 l2)
    (reverse 
        (cond (null? l1) l2
              't (concat (cdr l1) (cons (car l1) l2)))))

(main (concat '(1 2) '(3 4 5)))

)