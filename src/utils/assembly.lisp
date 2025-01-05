;; (LOAD <src> <dest>) chargement de mémoire à registre
(defun vm-LOAD (vm src dest)
  `(progn (setf ,dest (read-from-memory ,src))))

;; (STORE <src> <dest>) chargement de registre à mémoire
(defun vm-STORE (vm src dest)
  `(progn (write-to-memory ,dest ,src)))

;; (MOVE <src> <dest>) mouvement de registre à registre
(defun vm-MOVE (vm src dest)
  `(setf ,dest ,src))

;; (ADD <src> <dest>) addition
(defun vm-ADD (vm src dest)
  `(setf ,dest (+ ,dest ,src)))

;; (SUB <src> <dest>) soustraction
(defun vm-SUB (vm src dest)
  `(setf ,dest (- ,dest ,src)))

;; (MUL <src> <dest>) multiplication
(defun vm-MUL (vm src dest)
  `(setf ,dest (* ,dest ,src)))

;; (DIV <src> <dest>) division
(defun vm-DIV (vm src dest)
  `(setf ,dest (/ ,dest ,src)))

;; (INCR <dest>) incrément
(defun vm-INCR (vm dest)
  `(setf ,dest (1+ ,dest)))

;; (DECR <dest>) décrément
(defun vm-DECR (vm dest)
  `(setf ,dest (1- ,dest)))

;; (PUSH <src>) empiler
(defun vm-PUSH (vm src)
  `(push ,src *stack*))

;; (POP <dest>) dépiler
(defun vm-POP (vm dest)
  `(setf ,dest (pop *stack*)))

;; (LABEL <label>) déclaration d’étiquette
(defun vm-LABEL (label)
  `(progn ,@(declare-label label)))

;; (JMP <label>) saut inconditionnel à une étiquette
(defun vm-JMP (vm label)
  `(go ,label))

;; (JSR <label>) saut avec retour
(defun vm-JSR (vm label)
  `(progn (push *program-counter* *stack*)
          (go ,label)))

;; (RTN) retour
(defun vm-RTN (vm)
  `(setf *program-counter* (pop *stack*)))

;; (CMP <src1> <src2>) comparaison
(defun vm-CMP (vm src1 src2)
  `(setf *compare-result* (compare ,src1 ,src2)))

;; (JGT <label>) saut si plus grand
(defun vm-JGT (vm label)
  `(when (> *compare-result* 0) (go ,label)))

;; (JGE <label>) saut si plus grand ou égal
(defun vm-JGE (vm label)
  `(when (>= *compare-result* 0) (go ,label)))

;; (JLT <label>) saut si plus petit
(defun vm-JLT (vm label)
  `(when (< *compare-result* 0) (go ,label)))

;; (JLE <label>) saut si plus petit ou égal
(defun vm-JLE (vm label)
  `(when (<= *compare-result* 0) (go ,label)))

;; (JEQ <label>) saut si égal
(defun vm-JEQ (vm label)
  `(when (= *compare-result* 0) (go ,label)))

;; (JNE <label>) saut si différent
(defun vm-JNE (vm label)
  `(when (/= *compare-result* 0) (go ,label)))

;; (TEST <src>) comparaison à NIL
(defun vm-TEST (vm src)
  `(setf *compare-result* (not (null ,src))))

;; (JTRUE <label>) saut si non-NIL
(defun vm-JTRUE (vm label)
  `(when *compare-result* (go ,label)))

;; (JNIL <label>) saut si NIL
(defun vm-JNIL (vm label)
  `(when (null *compare-result*) (go ,label)))

;; (NOP) rien
(defun vm-NOP (vm)
  nil)

;; (HALT) arrêt
(defun vm-HALT (vm)
  `(error "Program halted"))
