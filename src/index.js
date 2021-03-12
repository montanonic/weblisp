import parse from "s-expression"

const namespace = {}

const exprs = [
    "((lambda (x) (+ x 3 4)) 99)"
]

console.log(exprs.map(x => parse(x)))

function evalExpr(sexp) {
    if (typeof sexp === "array") {
        evaluateFunction(sexp[0], sexp)
    } else {
        return sexp
    }
}

// /// exp is the whole expression, including the name
// function evaluateFunction(name, exp) {
//     if ("defun" === name) {
//         // (defun (name args*) body)
//         let [_, [name, args], body] = exp
//         let func = function() {
//             for (arg in args) {
//                 if (arguments)
//             }
//         }
//         namespace[name] = 
//     }
// }
