(define rl (lambda () (load "src/rootlisp.ss")))

; returns false if value is not found in environment
(define lookup (lambda (val env)
    (cond
        ((eq? env '()) #f)
        ((eq? (caar env) val) (cadar env))
        (#t (lookup val (cdr env))))))

; turns the special symbols #t and #f into just atoms, like in McCarthy's OG.
(define oldbool (lambda (x) 
    (cond 
        ((eq? x #t) 't)
        ((eq? x #f) 'f)
        (#t (write "error in oldbool!")))))

(define atom-eq? (lambda (x y)
    (oldbool (eq? x y))))

(define atom-atom? (lambda (x)
    (oldbool (atom? x))))

(define read-program (lambda () 
    (read (open-input-file "src/my.lisp"))))

; runs my lisp
(define run (lambda ()
    (debug-program)
    (eval-program (read-program)
                  '((nil ()))
                  '())))

(define debug-program (lambda ()
    (let ((program (read-program)))
        (cond
            ((eq? '() (filter 
                        (lambda (x) (eq? 'main (car x)))
                        program))
                (write "NO MAIN FOUND")
            )))))

(define list? (lambda (x)
    (not (atom? x))))

; Like eval but designed to evaluate in the context of an entire program's definitions. So, it takes a list of sexps, and we have a few special forms we support at the top-level. (define ...) will be our main workhorse, as it lets us create variable values and most importantly, functions.
; we expect one (and only one) "main" form, (main exp), where the code in exp is eval-ed within the environment. This evaluation is always done after the entire environment has been constructed.
(define eval-program (lambda (exps env main)
    ; (pretty-print env)
    (cond
        ; the base case is that we should evaluate the main function, which
        ; should hopefully be defined
        ((eq? '() exps) 
            ; (pretty-print "EVAL PROGRAM:")
            ; add the environment to the environment, so that it can be looked up!
            (eval main (cons (list '__env__ env) env)))
        ((eq? 'define (caar exps))
            (cond 
                ; ((define name exp) ...)
                ((atom? (cadar exps))
                    (eval-program (cdr exps) 
                                (cons (list (cadar exps) (caddar exps)) env)
                                main))
                ; ((define (name args) body)), this is the function form
                ((list? (cadar exps))
                    (eval-program (cdr exps) 
                                (cons (get-env-pair-for-define-function-shorthand (car exps)) env)
                                main))
                (#t (write "ERRRORRRR"))
            ))
        ; ((main exp) ...)
        ; add the main definition to the body but continue evaluating the environment
        ((eq? 'main (caar exps))
            (eval-program (cdr exps)
                          env
                          (cadar exps)))
        (#t 'error)
    )
))

; convert the body of the main form into a no-argument lambda so that our eval
; will actually evaluate it
(define main-body-to-function (lambda (body)
    (list (list 'lambda '() body))))

; returns a name/lambda pair defining the function, to be added to the
; environment
(define get-env-pair-for-define-function-shorthand (lambda (exp)
    ; exp == (define (funcname args*) body)
    (list (caadr exp) (list 'lambda (cdadr exp) (caddr exp)))))

(define eval (lambda (exp env)
    ; (pretty-print exp)
    ; (pretty-print env)
    (cond
        ((number? exp) exp)
        ((atom? exp) (lookup exp env))
        ; match procedure calls
        ((atom? (car exp))
            (cond
                ((eq? '+ (car exp)) (apply + (eval-list (cdr exp) env)))
                ((eq? '* (car exp)) (apply * (eval-list (cdr exp) env)))
                ((eq? 'quote (car exp)) (cadr exp))
                ((eq? 'atom? (car exp)) (atom-atom? (eval (cadr exp) env)))
                ((eq? 'eq? (car exp)) (atom-eq? (eval (cadr exp) env) 
                                                (eval (caddr exp) env)))
                ((eq? 'car (car exp)) (car (eval (cadr exp) env)))
                ((eq? 'cdr (car exp)) (cdr (eval (cadr exp) env)))
                ((eq? 'cons (car exp)) (cons (eval (cadr exp) env)
                                             (eval (caddr exp) env)))
                ((eq? 'cond (car exp)) (eval-cond (cdr exp) env))
                ; don't evaluate lambdas directly; this is useful so that we can
                ; support functions or forms that bind lambdas to names, like
                ; (let (f (lambda (x) (* x x)))).
                ((eq? 'lambda (car exp)) exp)
                ; allow the language to itself call eval with a given environment, strictly evaluating both arguments first
                ((eq? '__eval__ (car exp)) (eval (eval (cadr exp) env) (eval (caddr exp) env)))
                ; we don't recognize the atom as a built-in, so look it up in
                ; the environment (it will be whatever a user has labeled), and
                ; evaluate again with a call to whatever was looked up
                (#t (let ((lookup-val (lookup (car exp) env)))
                        (if lookup-val
                            (eval 
                                (cons lookup-val (cdr exp))
                                env)
                            `(lookup-failed-on-value: ,(car exp)))))
            )
        )
        ; the last part is for the matching of lambdas and define.
        ;
        ; The invocation of a lambda: ((lambda (params*) body) args*). We'll
        ; want to replace the parameters in the body of the lambda with the
        ; values from the arguments, but we'll also *only* replace parameters
        ; used in the body if they are present without evaluation: this is what
        ; it means to be lexically scoped: only parameters that are explicitly
        ; visible within the scope of the lambda will be replaced, no other
        ; effect will occur.
        ((eq? 'lambda (caar exp)) 
            ; we simply pass our lambda expression and its arguments over to our
            ; helper, which returns the lambda code body with lexical
            ; replacements made. this code body can then itself be evaluated.
            ; note that we *do* evaluate the arguments passed to the function
            ; first, then make replacements (possibly using those values), and
            ; then finally evaluate the code body
            ; (pretty-print "exp:")
            ; (pretty-print exp)
            ; (pretty-print "replace-lambda-args:")
            ; (pretty-print (replace-lambda-args (car exp) (cdr exp)))
            (eval (replace-lambda-args (car exp) (cdr exp)) env)
        )

            ; OLD CODE
            ; (eval (caddar exp) 
            ;       ; zip the params up with the arguments and add them to the
            ;       ; environment. But *also*, the arguments to the function
            ;       ; themselves need to be evaluated
            ;       (append (zip (cadar exp) 
            ;                    (eval-list (cdr exp) env))
            ;                env))

        ; with a label we simply add the lambda to the environment so that it
        ; can be referred to, and then we evaluate the lambda in that
        ; environment using the arguments passed to label
        ;
        ; ((label name lambda) args*)
        ((eq? 'label (caar exp)) (eval (cons (caddar exp) (cdr exp))
                                       (cons (list (cadar exp) (caddar exp)) env)))

        ; the first expression failed to match, so we try to evaluate it and then re-apply eval. this lets us use functions calls at the beginning of expressions, which so long as they compute something that is a valid eval form, will work.
        (#t (eval (cons (eval (car exp) env) (cdr exp)) env))
    )
))

; for evaluating lambdas, we want to replace its arguments at call time with the
; values that they should be replaced to. our implementation does not support shadowing local variables, but top-level variables *can* be shadowed.
(define replace-lambda-args (lambda (exp args)
    ; (lambda (params*) code)
    (replace-lambda-args-2
        (zip (cadr exp) args)
        (caddr exp))))

; ltable is the lookup table that we can lookup a parameter in the code body of the lambda and get its argument value; the lookup table only contains lookups for the function params, and holds no other environment context
(define replace-lambda-args-2 (lambda (ltable code)
    (cond
        ((eq? '() code) '())
        ((atom? code) (lookup-or-preserve code ltable))
        ((atom? (car code))
            (cons
                (lookup-or-preserve (car code) ltable)
                (replace-lambda-args-2 ltable (cdr code))))
        ; otherwise the head of the code list is itself a list, so we want to recursively evaluate lookups for it and continue on
        (#t (cons
                ; recursively evaluate list
                (replace-lambda-args-2 ltable (car code))
                ; and add it back onto the rest of the list
                (replace-lambda-args-2 ltable (cdr code))))
    )
))

; replace the value with its lookup value if it exists, or keep it as-is otherwise
(define lookup-or-preserve (lambda (value table)
    (if (lookup value table)
        (lookup value table)
        value)))

(define eval-list (lambda (exps env)
    ; construct a new list of the evaluated expressions
    (if (eq? exps '())
        '()
        (cons (eval (car exps) env) (eval-list (cdr exps) env)))))

(define zip (lambda (l1 l2)
    (if (or (eq? l1 '()) (eq? l2 '()))
        '()
        (cons (list (car l1) (car l2)) (zip (cdr l1) (cdr l2))))))

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

; (define compile (lambda (exps)
;     (cond 
;         ((number? exp) exp)
;         ((atom? exp) (lookup exp env))
;         ; match procedure calls
;         ((atom? (car exp))
;             (cond 
;                 ((eq? 'cons (car exp)) (comp-cons (cdr exp))))))))

; (define comp-cons (lambda (exp)
;     (car exp)))

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
