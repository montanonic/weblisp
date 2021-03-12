(define rl (lambda () (load "src/rootlisp.ss")))

(define lookup (lambda (val env)
    (cond
        ((eq? (caar env) val) (cadar env))
        (#t (lookup val (cdr env))))))

(define oldbool (lambda (x) 
    (cond 
        ((eq? x #t) 't)
        ((eq? x #f) 'f)
        (#t (write "error in oldbool!")))))

(define atom-eq? (lambda (x y)
    (oldbool (eq? x y))))

(define atom-atom? (lambda (x)
    (oldbool (atom? x))))

(define eval (lambda (exp env)
    (cond
        ((number? exp) exp)
        ((atom? exp) (lookup exp env))
        ; match procedure calls
        ((atom? (car exp))
            (cond
                ((eq? 'quote (car exp)) (cadr exp))
                ((eq? 'atom? (car exp)) (atom-atom? (eval (cadr exp) env)))
                ((eq? 'eq? (car exp)) (atom-eq? (eval (cadr exp) env) 
                                                (eval (caddr exp) env)))
                ((eq? 'car (car exp)) (car (eval (cadr exp) env)))
                ((eq? 'cdr (car exp)) (cdr (eval (cadr exp) env)))
                ((eq? 'cons (car exp)) (cons (eval (cadr exp) env)
                                             (eval (caddr exp) env)))
                ((eq? 'cond (car exp)) (eval-cond (cdr exp) env))
            )
        )
    )
))

; different than usual cond, one fewer levels of nesting, expects 't or 'f from
; predicates, and returns 'error for anything else.
;
; error-handling is intentionally not useful here because I intend to get some
; static typing / compilation support in so that we don't have to ever write a
; bad cond (and we can use helper functions like "truthy" to ensure that dynamic
; values become statically verifiable as either 't or 'f, thus ensuring that our
; restricted cond can still work with dynamic values at the cost of explicit
; coercion)
;
; exps is (pred-exp1 val-exp1 ... pred-expn val-expn)
(define eval-cond (lambda (exps env)
    (if (or (eq? '() exps) (eq? '() (cdr exps)))
        ; if we don't have at least two expressions, left, we have an error
        'error
        ; otherwise continue as normal
        (let ((pred (eval (car exps) env)) (val (eval (cadr exps) env)))
            (cond ((eq? pred 't) val)
                ; if it's false we recurse
                ((eq? pred 'f) (eval-cond (cddr exps) env))
                ; if it's neither 't nor 'f, we have an error
                (#t 'error)))
    )
))

; ; allows pattern matching of lists
; ; (x ..) ; matches first
; ; (.. x) ; matches last
; ; exps is a list of expression-value pairs 
; (define match (lambda value exps)
;     (let ((pattern (car exps)))
;         (cond
;             (eq? '.. pattern)
;     ))
; )

; ; We want to turn a pattern into a sequence of operations that will pull out the
; ; expected value from the given data structure. I think I'd like to have loose
; ; matching, where (x) matches a list with one element, or just the first
; ; element. It fails only on empty lists. (x ...) would thus match exactly the
; ; same way as (x). Well, this this case maybe I should only use immediate
; ; matching, but then I wouldn't be able to support ((x) ...ys) which gets all
; ; other elements in the list as ys
; ;
; ; (x ...) ; matches first (... x) ; matches last ((x ...))
; (define compile-pattern (lambda (pattern code)
;     ()
; ))

; if a match represents a rest param
(define is-rest-param (lambda (word)
    (equal? (substring (symbol->string word) 0 3) "...")))

; ..ys -> ys
(define get-rest-param (lambda (word)
    ; turn it to a list, drop first three elems, turn it into a symbol
    (pipe `(',word (symbol->string) (string->list) (cdddr) (list->string) (string->symbol)))))

; takes quoted expressions; expects literal function calls, not function names
(define pipe (lambda (qexprs)
    (if (eq? (cdr qexprs) '()) ; we want to end up with a single expression
        (eval (car qexprs))
        ; turn (val (thing x y) ...) into ((thing x y val) ...)
        (let ([rolled (append (cadr qexprs) (list (car qexprs)))])
            (pipe (cons rolled (cddr qexprs)))))))
