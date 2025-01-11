(require "compiler.lisp")
(require "vm.lisp")


(defun get-command-line-args ()
  #+clisp ext:*args*
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+clozure (cdr ccl:*command-line-argument-list*)
  #+ecl (loop for i from 1 below (si:argc) collect (si:argv i))
  #+allegro (cdr (system:command-line-arguments))
  #-(or clisp sbcl clozure ecl allegro)
  (error "get-command-line-args not implemented for this Lisp implementation"))

(defun exit-program (&optional (status 0))
  #+sbcl (sb-ext:exit :code status)
  #+clozure (ccl:quit status)
  #+clisp (ext:exit status)
  #+ecl (si:quit status)
  #+allegro (excl:exit status :quiet t)
  #-(or sbcl clozure clisp ecl allegro)
  (error "exit-program not implemented for this Lisp implementation"))

(defun print-usage ()
  (format t "Usage: lisp --load vm-cli.lisp -- [options] <command>~%")
  (format t "Commands:~%")
  (format t "  run <lisp-expression>    Compile and run a Lisp expression~%")
  (format t "  load <file>             Load and run a Lisp source file~%")
  (format t "Options:~%")
  (format t "  --debug                 Enable debug output~%")
  (format t "  --size <number>         Set VM memory size (default: 1000)~%")
  (format t "Example:~%")
  (format t "  lisp --load vm-cli.lisp -- --debug run \"(+ 2 3)\"~%"))

(defun parse-args (args)
  (let ((options '())
        (command nil)
        (command-args nil))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((string= arg "--debug")
           (push '(:debug . t) options))
          ((string= arg "--size")
           (if args
               (push (cons :size (parse-integer (pop args))) options)
               (error "Missing value for --size")))
          ((or (string= arg "run") (string= arg "load"))
           (setf command arg)
           (setf command-args args)
           (return))
          (t (error "Unknown argument: ~A" arg)))))
    (values options command command-args)))

(defun read-file (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

;; Update app.lisp's run-vm function:
(defun run-vm (expr options)
  (let* ((vm-size (or (cdr (assoc :size options)) 1000))
         (debug-mode (cdr (assoc :debug options)))
         (vm (make-vm vm-size)))
    
    (when debug-mode
      (attr-set vm :DEBUG t))

    (format t "Compiling expression: ~A~%" expr)
    (let* ((parsed-expr (read-from-string expr))
           (compiled-code (compile-i parsed-expr)))
      
      (format t "Loading program...~%")
      (vm-load vm compiled-code)
      
      (format t "Executing...~%")
      (vm-execute vm)
      
      (let ((result (attr-get vm :R0)))
        (format t "Result: ~A~%" result)))))

(defun main ()
  (let ((args (get-command-line-args)))
    (if (null args)
        (print-usage)
        (multiple-value-bind (options command command-args)
            (parse-args args)
          (handler-case
              (cond
                ((string= command "run")
                 (if command-args
                     (run-vm (car command-args) options)
                     (error "Missing expression for run command")))
                ((string= command "load")
                 (if command-args
                     (run-vm (read-file (car command-args)) options)
                     (error "Missing filename for load command")))
                (t (print-usage)))
            (error (e)
              (format t "Error: ~A~%" e)
              (exit-program 1)))))))

;; Only run main when loaded as the primary script
(when *load-pathname*
  (main)(exit))