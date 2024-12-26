;;Pour les label en lisp. J'utilise gensym pour générer un nouveau label complétement indépendant

;; Fonction pour incrémenter la variable
(defun incrementerCPT (cpt-label)
  (incf cpt-label))

(defun new-label ()
        (gensym "LABEL-")
)

