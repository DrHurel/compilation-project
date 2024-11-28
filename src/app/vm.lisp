;; Load the module manually
(load "src/utils/assembly.lisp")
(load "src/utils/vm-functions.lisp")

;; Main function
(defun main ()
  (say-hello)
  (setq "is_running" T)
  (setq "instruction" nil)
  (print_start_info)

  (loop while is_running do (
    (fetch instruction)
    (if (evaluate instruction)
    (run instruction is_running))
    )
  )
  
)