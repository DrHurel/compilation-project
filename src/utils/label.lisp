;; Définir une variable globale

;; Fonction pour incrémenter la variable
(defun incrementerCPT (cpt-label)
  (incf cpt-label))

(defun new-label (cpt-label)
    (let ((res (write-to-string  cpt-label)))
        (setf cpt-label (+ 1 cpt-label))
        (concatenate 'string "label-" res)
    )
)

