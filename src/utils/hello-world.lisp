;; Define the package
(defpackage :hello-module
  (:use :cl)        ;; Import Common Lisp symbols
  (:export :say-hello)) ;; Export the function to make it available to other packages

(in-package :hello-module)

;; Function implementation
(defun say-hello ()
  (format t "Hello, World!~%"))