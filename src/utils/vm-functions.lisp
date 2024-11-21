(defun make_vm (name &rest options) 
  "Creates a virtual machine with a given name and optional options."
  ;; Function body here
)

(defun vm_load (vm code)
  "Loads the provided code into the specified virtual machine."
  ;; Function body here
)

(defun vm_exec (vm)
  "Executes the code in the specified virtual machine."
  ;; Function body here
)

(defun vm_load_exec (vm code)
  (
    (vm_load vm code)
    (vm_exec vm)
  )
)