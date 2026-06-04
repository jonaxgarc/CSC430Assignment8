# Assignment 8 - C430 Core Interpreter

## Overview
This project implements a small C-inspired interpreter in Typed Racket. It follows the same core structure as the higher-order interpreter assignment: parse source syntax into an AST, interpret with an environment, serialize values, and test the behavior with RackUnit.

## Implemented
- Numbers, booleans, and strings
- Variables through local `var` bindings
- `if` expressions
- Higher-order functions and closures
- Function calls where primitives are values
- Primitive operators: `+`, `-`, `*`, `/`, `<=`, `equal?`, `substring`, `strlen`, and `error`
- Helpful error messages containing `C430`
- RackUnit tests for parsing, serialization, lookup, primitives, functions, and errors

## Example
```racket
(top-interp
 '(var ([x = 3] [y = 4])
    do
    (+ x y)))
```

This evaluates to `"7"`.
