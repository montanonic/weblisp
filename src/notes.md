# Helpful Compiler

## Compilation Debugging

### Error when source code refers to unbound variable

If variables could be defined at runtime this wouldn't work, but I currently don't see a good argument for runtime-defined variables when macros+maps can be used to emulate the same. Thus, we should be able to do a scope analysis on all unquoted atoms and verify if they are bound in the global or lexical environment, which will help avoid issues with typos.

# Language Semantics

## Lexical Scope

Consider `(lambda (x) (lambda (x) (* x x))`. This is a function that when called with a value, returns a squaring function. In many closure-supporting languages, this is a perfectly valid program, but it is not "good code" because the outer lambda's `x` is never used. This is because everywhere the variable `x` is referenced, it is within the inner lambda, which itself binds a variable `x`. This second `x` is said to "shadow" the first, meaning that within the context that it is bound, the other `x` cannot be referred to. Thus `f(3)` returns `(lambda (x) (* x x))` and *not* `(lambda (x) (* 3 3))`.

### Immediate Variable Replacement

One approach to implementing lexical scope is to focus on what evaluation means syntactically. To evaluate a lambda, we fully evalate its arguments (call-by-value semantics) and replace all each parameter within the body with their corresponding argument values. So in lisp, we might think to implement evaluation by just swapping out the parameters in the body with their bound values, and then evaluating. In other words, take `((lambda (x) (+ x 3)) 19)` and run code that turns that into `(+ 19 3)` and then continue evaluating. That clearly works, but how would you write the code? Well, first we pair up the parameters with arguments, getting `((x 19))`, then we do replacements in the body. Done. 

Except now consider: `(lambda (x) (lambda (x) (* x x)))`, which as mentioned above is a valid albeit not well-coded function. Let's evaluate `((lambda (x) (lambda (x) (* x x))) 42)` following the same procedure. Well, our body is `(lambda (x) (* x x))`, and so after replacements gives `(lambda (42) (* 42 42))`, which is not only invalid code, but the body of the lambda itself is also wrong conceptually: the second `x` is what is being referred to (which admittedly is a presumption around our language semantics, but it is ubiquitous for deeper scope to get precedence in naming conflicts), and yet our replacement strategy fully assumed that all occurences of the outer-most variables were fully replaceable. This was an incorrect assumption given what we wanted. Now, if we did *not* support any variable bindings within a lambda body, this would be fine. Alternatively, we could just dissallow shadowing, and again, this would be fine. Observe: `((lambda (x) (lambda (y) (* y y))) 42)` becomes `(lambda (y) (* y y))`, which is exactly what we wanted but failed to achieve earlier in the case of `x` being repeated.

Now, while disallowing local shadowing (top-level values can still be shadowed) is a perfectly valid design choice to make, and frankly would allieviate some confusion, it also presents us with a harder time of letting our user know that they have incorrectly overridden a variable, as we must look for all possible binding forms in the code (lambda, let, and tons of macro forms) and check if shadowing occurred. In other words, we have to be syntax-aware of anything that has the semantics of variable binding.

### Using a local environment

This is a technique that ameliorates the need to be syntactically aware. That is to say, instead of replacing parameters with their arguments, and needing to know the syntax of variable binding forms so that we can give good errors to our users if they try to rebind without shadowing, like in `(lambda (x) (let (x 19) x))` which requires that we understand to look for `let` in our code when doing lambda evaluation to ensure either `x` gets shadowed correctly (being bound to `19`), or the user gets an error that the variable `x` is already in use and cannot be rebound.

Notice again how we must always be on the lookout for variable binding forms. Suppose we create a macro `(state (x) (set! x (+ x 1)) x)` which returns the value of the last expression, performing mutations throughout, we need to be on the lookout for *two* bindings here, first `(state (x) ...)`, and the second `(set! x ...)`, neither of which have meaning if `x` gets syntactically replaced with a value.

So with locals, we jump outside of the immediacy of textual replacement and instead say that the act of variable binding itself does nothing textually, but rather adds to the semantic construct of the local environment. This environment will be distinguished from the top-level environment. Re-imagine now in a language where shadowing is valid how `((lambda (x) (let (x 19) x)) 42)` constructs the local environment `((x 19) (x 42))` where the first match from the left is the current value. Now our evaluation can avoid explicit replacement, proceeding into `(let (x 19) x)` with an environment `((x 42))` and then proceeding into `x` with `((x 19) (x 42))`. This value then can simply be resolved by lookup, and with similar simplicity if shadowing is to be disallowed the local environment can be scanned such that adding a repeat binding immediately triggers an error.

Now let's consider this in light of the subtlety of dynamic scope. By creating an environment, we have allowed for the following. `(define (map f list) ...)`, `(define (process data) (map (lambda (x) (car list)) data))`. Let's evaluate. First we evaluate the arguments to `map` and add them to the environment `((f ...) (list data))`. Now, within `map` our lambda is called, like so `(f (car list))`, and this function gets evaluated within the current environment, so we get env: `((x ~(car list)) (f ...) (list data))`. Turns out that this code will be valid despite the lamdba referring to a list that is nowhere to be found lexically or at the top level. This is dynamic scope: `map` itself binds `list` in its environment at the time of its calling, and the lambda function goes deeper within that environment, but evaluates within it and therefore can lookup values within it! We don't want to allow for this, we want `list` and `f` as bindings to *only* be available in the lexical context of map's own definition.

So what must we do? Well, we must evaluate `(lambda (x) (car list))` within a context where only `data` is locally bound.

### Closure

Once we get into this idea of closure we get some interesting consequences in a language with mutation. Consider: `(lambda (count) (lambda () (inc! count)))`. This basically is a counter-creator: give it an initial value, and you now have a function that has as its environment that value. `(f 10)` has `((count 10))` as its environment, and the inner lambda by referring to `count` would be constructed with reference to the enclosing environment. It thus has a "private" variable known only to itself.

While this technique allows for creating many patterns such as object-orientation, private state, and other things, I have always had some distaste for it. It feels more like a fun trick than a deep idea. It is a sort of implementation detail leaking into how we treat the semantics.

Now, non-mutable closure on the otherhand 

# State and Mutation

For purposes of prototyping mutation in a "pure" language, a mutation macro might be implemented like:

```lisp
(defmacro (mutate bindings code) (mutate-1 bindings code '()))
(defmacro (mutate-1 bindings code code-out)
    (match code
        (set! @x @y) (mutate-1 (update x y bindings) (cdr code) code-out)
        (return @x)  (lookup x bindings)
        _            (mutate-1 bindings              (cdr code) (cons (car code) code-out))))

(define (mutate (x 0) ())
```
The semantics are simple enough (uhh, are they though?)

## Lexical Scope Part 2: What feature do we *really* want?

So I was brainstorming out loud to Dee about how Python doesn't have closures but JavaScript does. This means in Python, everything global and top-level can be referred to within a function/method body, but you can't define and return a function from within a function that references any interior state. However, you could of course write a function that takes extra arguments corresponding to the local variables, and returns a function inside of itself that receives those values and then only requires the remaining ones, and then now we have local variable access within, right?

Well, yes, but in a different way than JS. See, when this function is called, it is evaluated *at that point in time*, which means that the values of those variables at that time, and only at that time, are used. If they ever change later, that won't be reflected in our target function unless this variable-passing constructor function is called again.

The real meat of closures is that they do allow for an active reference of the value as it currently is. In python this works just fine if that value is global, but again, it doesn't apply to local values. In this way, closures have often been used as ways of defining local environments, local states, to provide an alternative means to encapsulation than Object Oriented style. The author of the You Don't Know JS series talks a lot about the module pattern in JavaScript, which is largely just using a function as a way of grouping together common state and functionality and exposing a module-like interface. The differences between that pattern and using modern modules are slight, so while a useful pattern, it's worth recognizing that just implementing a module system semantically might be cleaner. When the interpreter for a language is easily hackable, we can truly just implement new features.

# Functions, Macros, DSLs, or Interpreter Hacking?

There are multiple levels in which we can solve abstraction problems in lispy, I'm curious about developing an intuition for when interpreter extension is preferrable to macros, and I expect to gain more perspective as I hack on it.