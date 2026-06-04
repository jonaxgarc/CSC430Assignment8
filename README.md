# Assignment 8 - C-like Language in Typed Racket

## Overview
This project implements a small C-like language using Typed Racket. The source language uses Racket s-expressions so parsing stays manageable, but the supported features are based on core C ideas.

## Implemented Features
- Numbers, booleans, strings, and `null`
- Variables and assignment
- Blocks, `if`, and `while`
- First-class functions and function calls
- Arithmetic, comparison, equality, and boolean operators
- Address-of, dereference, and pointer assignment
- `malloc` and `free`-style memory operations
- Arrays with creation, indexing, and mutation
- Structs with field lookup and field mutation
- RackUnit tests in `Assignment8.rkt`

## Example
```racket
(top-interp
 '(var x 7
    (var p (& x)
      (block
        (ptr-set! p 99)
        (deref p)))))
```

This evaluates to `"99"`.
