;; Définir une variable globale
(defparameter labelCPT 0)

;; Fonction pour incrémenter la variable
(defun incrementerCPT ()
  (incf labelcpt))

(defun new-label ()
    (let ((res (write-to-string  labelcpt)))
        (incrementercpt)
        (concatenate 'string "label-" res)
    )
)

