#!/bin/bash

# Path to CLISP binary
LISP="clisp"

# Run CLISP and load project files
$LISP -q -i -i "src/app/vm.lisp"
