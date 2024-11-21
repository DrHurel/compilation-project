;; (LOAD <src> <dest>) chargement de mémoire à registre
(defun LOAD (src dest)
  `(progn (setf ,dest (read-from-memory ,src))))

;; (STORE <src> <dest>) chargement de registre à mémoire
(defun STORE (src dest)
  `(progn (write-to-memory ,dest ,src)))

;; (MOVE <src> <dest>) mouvement de registre à registre
(defun MOVE (src dest)
  `(setf ,dest ,src))

;; (ADD <src> <dest>) addition
(defun ADD (src dest)
  `(setf ,dest (+ ,dest ,src)))

;; (SUB <src> <dest>) soustraction
(defun SUB (src dest)
  `(setf ,dest (- ,dest ,src)))

;; (MUL <src> <dest>) multiplication
(defun MUL (src dest)
  `(setf ,dest (* ,dest ,src)))

;; (DIV <src> <dest>) division
(defun DIV (src dest)
  `(setf ,dest (/ ,dest ,src)))

;; (INCR <dest>) incrément
(defun INCR (dest)
  `(setf ,dest (1+ ,dest)))

;; (DECR <dest>) décrément
(defun DECR (dest)
  `(setf ,dest (1- ,dest)))

;; (PUSH <src>) empiler
(defun PUSH (src)
  `(push ,src *stack*))

;; (POP <dest>) dépiler
(defun POP (dest)
  `(setf ,dest (pop *stack*)))

;; (LABEL <label>) déclaration d’étiquette
(defun LABEL (label)
  `(progn ,@(declare-label label)))

;; (JMP <label>) saut inconditionnel à une étiquette
(defun JMP (label)
  `(go ,label))

;; (JSR <label>) saut avec retour
(defun JSR (label)
  `(progn (push *program-counter* *stack*)
          (go ,label)))

;; (RTN) retour
(defun RTN ()
  `(setf *program-counter* (pop *stack*)))

;; (CMP <src1> <src2>) comparaison
(defun CMP (src1 src2)
  `(setf *compare-result* (compare ,src1 ,src2)))

;; (JGT <label>) saut si plus grand
(defun JGT (label)
  `(when (> *compare-result* 0) (go ,label)))

;; (JGE <label>) saut si plus grand ou égal
(defun JGE (label)
  `(when (>= *compare-result* 0) (go ,label)))

;; (JLT <label>) saut si plus petit
(defun JLT (label)
  `(when (< *compare-result* 0) (go ,label)))

;; (JLE <label>) saut si plus petit ou égal
(defun JLE (label)
  `(when (<= *compare-result* 0) (go ,label)))

;; (JEQ <label>) saut si égal
(defun JEQ (label)
  `(when (= *compare-result* 0) (go ,label)))

;; (JNE <label>) saut si différent
(defun JNE (label)
  `(when (/= *compare-result* 0) (go ,label)))

;; (TEST <src>) comparaison à NIL
(defun TEST (src)
  `(setf *compare-result* (not (null ,src))))

;; (JTRUE <label>) saut si non-NIL
(defun JTRUE (label)
  `(when *compare-result* (go ,label)))

;; (JNIL <label>) saut si NIL
(defun JNIL (label)
  `(when (null *compare-result*) (go ,label)))

;; (NOP) rien
(defun NOP ()
  nil)

;; (HALT) arrêt
(defun HALT ()
  `(error "Program halted"))
