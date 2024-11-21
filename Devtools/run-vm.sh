#!/bin/bash

# Path to CLISP binary
LISP="clisp"

# Run CLISP and load project files
$LISP -q -i "src/utils/hello-world.lisp" -i "src/app/vm.lisp" -x "(main)"
