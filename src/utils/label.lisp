;; Définir une variable globale

;; Fonction pour incrémenter la variable
(defun incrementerCPT (cpt-label)
  (incf cpt-label))

(defun new-label ()
        (gensym "LABEL-")
)

