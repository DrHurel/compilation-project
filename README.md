# Lisp VM Compiler

This is a Lisp VM and compiler that can execute Lisp code by compiling it to custom assembly instructions.

## Getting Started

### Prerequisites

- CLISP (Common Lisp implementation)

### Running the Examples

1. Create a file `factorial.lisp` with the following Lisp code:

```lisp
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 5)
```