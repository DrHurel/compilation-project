;; Define the main package
(defpackage :my-hello-project.main
  (:use :cl :hello-module)) ;; Import the hello-module package

(in-package :my-hello-project.main)

;; Entry point function
(defun main ()
  (say-hello)) ;; Call the function from the module