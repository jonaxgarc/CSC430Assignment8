# Assignment 8 - Recreating the C Language in Racket

## Overview
This project recreates the Assignment 4 interpreter style in Typed Racket. It implements a small VEBG-style language with parsing, interpretation, mutation, primitive operations, and array-backed memory.

> [!NOTE]
> This version includes a runnable interpreter and RackUnit test coverage.

## Features
**Implemented:**
- Integer, string, and boolean values
- Variables through environment/store bindings
- Mutation with `:=`
- `if`, `fn`, `given`, and function calls
- Primitive operations including arithmetic, comparison, equality, strings, input/output, sequencing, and concatenation
- Array creation, indexing, and mutation with `make-array`, `array`, `aref`, and `aset!`
- Higher-order examples for `while` and `in-order`
- RackUnit tests in `Assignment8.rkt`

