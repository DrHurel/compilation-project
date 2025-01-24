(require "compiler.lisp")
(require "vm.lisp")


(defun get-command-line-args ()
  #+clisp ext:*args*
  #-clisp
  (error "get-command-line-args not implemented for this Lisp implementation"))

(defun exit-program (&optional (status 0))
  #+clisp (ext:exit status)
  #-clisp
  (error "exit-program not implemented for this Lisp implementation"))

(defun print-usage ()
  (format t "Usage: lisp --load vm-cli.lisp -- [options] <command>~%")
  (format t "Commands:~%")
  (format t "  run <lisp-expression>    Compile and run a Lisp expression~%")
  (format t "  load <file>             Load and run a Lisp source file~%")
  (format t "Options:~%")
  (format t "  --debug                 Enable debug output~%")
  (format t "  --exit                 Enable exit on program completed~%")
  (format t "  --size <number>         Set VM memory size (default: 1000)~%")
  (format t "Example:~%")
  (format t "  lisp --load vm-cli.lisp -- --debug run \"(+ 2 3)\"~%"))

;; In app.lisp, update parse-args:
(defun parse-args (args)
  (let ((options '())
        (command nil)
        (command-args nil))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((string= arg "--exit")
           (push '(:exit . t) options))
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
         (exit-mode (cdr (assoc :exit options)))
         (vm (make-vm vm-size)))

    (when debug-mode
      (attr-set vm :DEBUG t))
    
    (format t "Compiling expressions: ~A~%" expr)
    
    (dolist (single-expr (read-from-string (format nil "(~A)" expr)))
      (let* ((compiled-code (compile-i single-expr)))
        (format t "Loading program...~%")
        (vm-load vm compiled-code)

        ))
    (format t "Executing...~%")
        (format t "Start time: ~A~%" (get-universal-time ))
        (vm-execute vm)
        (format t "End time: ~A~%" (get-universal-time))
    (let ((result (attr-get vm :R0)))
      (format t "Result: ~A~%" result))
    (when exit-mode
      (exit)
    ))

)

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
  (main))