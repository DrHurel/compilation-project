;; Attribute management functions
(defun attr-set (vm attr value)
  (setf (gethash attr vm) value))

(defun attr-get (vm attr)
  (gethash attr vm))

(defun attr-array-init (vm attr size)
  (attr-set vm attr (make-array size :initial-element nil)))
