;; Define the main package
(defpackage :compilation-project-vm.main
  (:use :cl :hello-module)) ;; Import the hello-module package

(in-package :compilation-project-vm.main)

;; Entry point function
(defun main ()
  (say-hello)) ;; Call the function from the module
