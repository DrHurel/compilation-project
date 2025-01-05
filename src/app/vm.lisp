;; Load the module manually
(require "src/utils/assembly.lisp")
(require "src/utils/vm-functions.lisp")


(defun vm-load (vm program)
    ;; Détermine l'adresse de départ pour charger le programme
  (let ((initial-pc (- (or (var-basse-get vm +last-code-id+) (+ (pc-get vm) 1)) 1)))
        ;; Charge les instructions et les labels
        (loop for instruction in program do
            (if (is-label instruction)
                ;; Si c'est un label, stocke son adresse dans la table des labels
                (etiq-set vm (string (second instruction)) initial-pc)
                ;; Sinon, stocke l'instruction en mémoire et met à jour initial-pc
                (progn
                    (mem-set vm initial-pc instruction)
                    (setq initial-pc (- initial-pc 1)))))

        ;; Met à jour :LAST_CODE
        (var-basse-set vm +last-code-id+ (+ initial-pc 1))

        ;; Mise à jour des adresses pour les sauts
        (update-labels-for-jumps vm)))

(defun vm-execute (vm)
  (loop while (and (>= (pc-get vm) (var-basse-get vm +last-code-id+)) (is-running vm)) do
    (let ((instruction (mem-get vm (pc-get vm))))
      (when (is-debug vm) (format t "~A " instruction))
      (match (first instruction)
        (('LOAD) (vm-LOAD vm (second instruction) (third instruction)))
        (('STORE) (vm-STclearORE vm (second instruction) (third instruction)))
        (('MOVE) (vm-MOVE vm (second instruction) (third instruction)))
        (('ADD) (vm-ADD vm (second instruction) (third instruction)))
        (('SUB) (vm-SUV vm (second instruction) (third instruction)))
        (('MUL) (vm-MUL vm (second instruction) (third instruction)))
        (('DIV) (vm-DIV vm (second instruction) (third instruction)))
        (('INCR) (vm-INCR vm (second instruction)))
        (('DECR) (vm-DECR vm (second instruction)))
        (('PUSH) (vm-PUSH vm (second instruction)))
        (('POP) (vm-POP vm (second instruction)))
        (('JMP) (vm-JMP vm (second instruction)))
        (('CMP) (vm-CMP vm (second instruction) (third instruction)))
        (('JSR) (vm-JSR vm (second instruction)))
        (('JGT) (vm-JGT vm (second instruction)))
        (('JGE) (vm-JGE vm (second instruction)))
        (('JLT) (vm-JLT vm (second instruction)))
        (('JLE) (vm-JLE vm (second instruction)))
        (('JEQ) (vm-JEQ vm (second instruction)))
        (('JNE) (vm-JNE vm (second instruction)))
        (('TEST) (vm-TEST vm (second instruction)))
        (('JNIL) (vm-JNIL vm (second instruction)))
        (('JTRUE) (vm-JTRUE vm (second instruction)))
        (('NOP) (vm-NOP vm))
        (('HALT) (vm-HALT vm))
        (_
          (format t "Unknown instruction: ~A~%" instruction)
        )
      )
      (pc-decr vm)
      (when (is-debug vm)
        (format t "R0: ~A R1: ~A R2: ~A SP: ~A FP: ~A Stack: ~A~%"
                (attr-get vm :R0)
                (attr-get vm :R1)
                (attr-get vm :R2)
                (attr-get vm :SP)
                (attr-get vm :FP)
                (stack-get vm))))))


(defun vm-reset(vm &optional (size 1000))
  (let ((size (max size 1000)) (variablesBasse 30) (tailleZones (- (max size 1000) 30)))
    (attr-set vm :R0 0)
    (attr-set vm :R1 0)
    (attr-set vm :R2 0)
    (attr-set vm :MAX_MEM size)          ;; Définition de la taille de la VM
    (attr-array-init vm :MEM size)       ;; Définition de la mémoire
    (var-basse-set vm +start-code-id+ (- size 1))
    (var-basse-set vm +etiq-id+ (make-hash-table))
    (pc-set vm (- size 1))               ;; puis on va diminuer dans la mémoire, ça permet de ne pas trop se faire de soucis
    (bp-set vm 30)                       ;; Le BP lui est défini après les variables basses.
    (sp-set vm (bp-get vm))              ;; Le stack pointer est de base sur BP.
    (fp-set vm (sp-get vm))
    (ms-set vm (+ variablesBasse (/ tailleZones 2))) ;; s'en suit la valeur maximum du stack qu'on ne doit pas dépasser
    (set-running vm 1)))                 ;; Ainsi pour une VM taille 1000: BP = 30, SP = 30, MS = 224, MAX_MEM = 1000

(defun vm-init(vm &optional (size 1000))
  (attr-set vm :NAME vm)
  (vm-reset vm))