(defun vm-init (name)
  "Initialize a new virtual machine state with a name."
  (list :name name :stack '() :pc 0 :fp 0 :instructions '()))

(defun vm-reset (vm)
  "Reset the virtual machine state."
  (setf (getf vm :stack) '())
  (setf (getf vm :pc) 0)
  (setf (getf vm :fp) 0)
  (setf (getf vm :instructions) '())
  vm)

(defun vm-load (vm instructions)
  "Load instructions into the virtual machine."
  (setf (getf vm :instructions) instructions))

(defun vm-execute (vm)
  "Execute instructions in the virtual machine."
  (loop while (< (getf vm :pc) (length (getf vm :instructions)))
        do (let ((instruction (nth (getf vm :pc) (getf vm :instructions))))
             (setf (getf vm :pc) (+ (getf vm :pc) 1))
             (case (car instruction)
               ('MOVE (push (cadr instruction) (getf vm :stack)))
               ('PUSH (push (car (getf vm :stack)) (getf vm :stack)))
               ('POP (pop (getf vm :stack)))
               ('ADD (let ((a (pop (getf vm :stack)))
                          (b (pop (getf vm :stack))))
                       (push (+ a b) (getf vm :stack))))
               ('SUB (let ((a (pop (getf vm :stack)))
                          (b (pop (getf vm :stack))))
                       (push (- a b) (getf vm :stack))))
               ('MULT (let ((a (pop (getf vm :stack)))
                           (b (pop (getf vm :stack))))
                        (push (* a b) (getf vm :stack))))
               ('DIV (let ((a (pop (getf vm :stack)))
                          (b (pop (getf vm :stack))))
                      (push (/ a b) (getf vm :stack))))
               ('JUMP (setf (getf vm :pc) (cadr instruction)))
               ('JSR (progn
                      (push (getf vm :pc) (getf vm :stack))
                      (setf (getf vm :pc) (cadr instruction))))
               ('LABEL nil)
               ('CMP (let ((a (pop (getf vm :stack)))
                          (b (pop (getf vm :stack))))
                      (push (equal a b) (getf vm :stack))))
               ('JEQ (when (pop (getf vm :stack))
                       (setf (getf vm :pc) (cadr instruction))))))))
