(defun attr-get(vm attr)
  (get vm attr))

(defun attr-set(vm attr value)
  (setf (get vm attr) value))