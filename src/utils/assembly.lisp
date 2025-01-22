(require "src/utils/tools.lisp")

(defun asm-jump (vm insn)
  (let ((label (second insn)))
    ;;(format t "JUMP: Label ~A~%" label)
    (let ((target (etiq-get vm label)))
      (if target
          (attr-set vm :PC target)
          (error "Undefined label: ~a" label)))))

(defun asm-add (vm insn)
  (let ((val1 (attr-get vm (second insn)))
        (val2 (attr-get vm (third insn))))
    (unless (and (numberp val1) (numberp val2))
      (error "ADD requires numeric operands"))
    (let ((reg1 (second insn))
          (reg2 (third insn)))
      (let ((val1 (attr-get vm reg1))
            (val2 (attr-get vm reg2)))
        ;;(format t "ADD: Register1 ~A, Value1 ~A, Register2 ~A, Value2 ~A~%" reg1 val1 reg2 val2)
        (let ((result (+ val1 val2)))
          ;;(format t "ADD Result: ~A~%" result)
          (attr-set vm reg1 result))))))

(defun asm-sub (vm insn)
  (let ((reg1 (second insn))
        (reg2 (third insn)))
    (let ((val1 (attr-get vm reg1))
          (val2 (attr-get vm reg2)))
      ;;(format t "SUB: Register1 ~A, Value1 ~A, Register2 ~A, Value2 ~A~%" reg1 val1 reg2 val2)
      (let ((result (- val1 val2)))
        ;;(format t "SUB Result: ~A~%" result)
        (attr-set vm reg1 result)))))

(defun asm-mul (vm insn)
  (let ((reg1 (second insn))
        (reg2 (third insn)))
    (let ((val1 (attr-get vm reg1))
          (val2 (attr-get vm reg2)))
      ;;(format t "MUL: Register1 ~A, Value1 ~A, Register2 ~A, Value2 ~A~%" reg1 val1 reg2 val2)
      (let ((result (* val1 val2)))
        ;;(format t "MUL Result: ~A~%" result)
        (attr-set vm reg1 result)))))

(defun asm-div (vm insn)
  (let ((reg1 (second insn))
        (reg2 (third insn)))
    (let ((val1 (attr-get vm reg1))
          (val2 (attr-get vm reg2)))
      ;;(format t "DIV: Register1 ~A, Value1 ~A, Register2 ~A, Value2 ~A~%" reg1 val1 reg2 val2)
      (if (zerop val2)
          (error "Division by zero")
          (let ((result (/ val1 val2)))
            ;;(format t "DIV Result: ~A~%" result)
            (attr-set vm reg1 result))))))

(defun asm-jmp (vm insn)
  (let ((label (second insn)))
    ;;(format t "JMP: Label ~A~%" (+ label 1))
    (if (numberp label)
        (attr-set vm :PC (+ label 1))
        (if (fboundp (intern (string-upcase label)))
            (progn
              ;; Si label est une fonction Lisp, récupérer les arguments et appeler la fonction
              (let ((args '()))
                ;; Récupérer le nombre d'arguments du stack
                (let ((arg-count (mem-get vm (attr-get vm :SP))))
                  ;; Récupérer les arguments du stack
                  (dotimes (i arg-count)
                    (let ((arg-value (mem-get vm (- (attr-get vm :SP) (+ i 1)))))
                      (if (is-debug vm)
                          (format t "Arg: ~A~%" arg-value))
                      (push arg-value args)))
                  ;; Appeler la fonction Lisp avec les arguments et stocker le résultat dans R0
                  (let ((result (apply (intern (string-upcase label)) args)))
                    (attr-set vm :R0 result)))))
            ;; Sinon, signaler une erreur
            (error "Etiquette non définie: ~a" label)))))

(defun asm-jsr (vm insn)
   ;; Extraire l'étiquette de l'instruction
  (let ((label (second insn)))
    (if (or (numberp label) (is-etiq-set vm label))
        (progn
          ;;(format t "JSR: Label ~A~%" label)
          ;; Si l'étiquette est définie, continuer avec l'exécution normale
          (attr-set vm :R1 (- (pc-get vm) 1))
          (asm-push vm '(PUSH :R1))
          (asm-jmp vm insn))
        ;; Gérer le cas où l'étiquette n'est pas définie
        (if (fboundp (intern (string-upcase label)))
            (progn
              ;; Si label est une fonction Lisp, récupérer les arguments et appeler la fonction
              (let ((args '()))
                ;; Récupérer le nombre d'arguments du stack
                (let ((arg-count (mem-get vm (attr-get vm :SP))))
                  ;; Récupérer les arguments du stack
                    (dotimes (i arg-count)
                    (let ((arg-value (mem-get vm (- (attr-get vm :SP) (+ i 1)))))
                      (if (is-debug vm)
                          (format t "Arg: ~A~%" arg-value))
                      (push arg-value args)))
                  ;; Appeler la fonction Lisp avec les arguments et stocker le résultat dans R0
                  (let ((result (apply (intern (string-upcase label)) args)))
                    (attr-set vm :R0 result)))))
            ;; Sinon, signaler une erreur
            (error "Etiquette non définie: ~a" label)))))


(defun get-label (vm label)
  (etiq-get vm (string label)))

(defun asm-cmp (vm insn)
  (let ((reg1 (second insn))
        (reg2 (third insn)))
    (let ((val1 (cond
                  ((is-const reg1) (second reg1))
                  ((keywordp reg1) (attr-get vm reg1))))
          (val2 (cond
                  ((is-const reg2) (second reg2))
                  ((keywordp reg2) (attr-get vm reg2)))))
      ;;(format t "CMP: Register1 ~A, Value1 ~A, Register2 ~A, Value2 ~A~%" reg1 val1 reg2 val2)
      ;; Gérer les cas où val1 ou val2 sont t ou nil
      (cond
        ((or (eq val1 't) (eq val1 'nil) (eq val2 't) (eq val2 'nil))
             ;; Comparaison d'égalité seulement
             (attr-set vm :FEQ (if (eq val1 val2) 1 0))
             ;;(format t "FEQ set to ~A~%" (attr-get vm :FEQ))
            
          (t
             ;; Comparaison numérique
             (attr-set vm :FEQ (if (= val1 val2) 1 0))
             (attr-set vm :FLT (if (< val1 val2) 1 0))
             (attr-set vm :FGT (if (> val1 val2) 1 0))
             ;;(format t "FEQ set to ~A, FLT set to ~A, FGT set to ~A~%" (attr-get vm :FEQ) (attr-get vm :FLT) (attr-get vm :FGT)
        )
      )
    )
  )
) )

(defun asm-jgt (vm insn)
  (if (eq (attr-get vm :FGT) 1)
      (asm-jmp vm insn)))

(defun asm-jge (vm insn)
  (if (or (eq (attr-get vm :FGT) 1) (eq (attr-get vm :FEQ) 1))
      (asm-jmp vm insn)))

(defun asm-jlt (vm insn)
  (if (eq (attr-get vm :FLT) 1)
      (asm-jmp vm insn)))

(defun asm-jle (vm insn)
  (if (or (eq (attr-get vm :FLT) 1) (eq (attr-get vm :FEQ) 1))
      (asm-jmp vm insn)))

(defun asm-jeq (vm insn)
  (if (eq (attr-get vm :FEQ) 1)
      (asm-jmp vm insn)))

(defun asm-jne (vm insn)
  (if (eq (attr-get vm :FEQ) 0)
      (asm-jmp vm insn)))

(defun asm-test(vm insn)
  (let ((dst (second insn)))
    (let ((v (cond
            ((is-const dst) (second dst))
            ((keywordp dst) (attr-get vm dst)))))
      (attr-set vm :FNIL (null v)))))

(defun asm-jtrue (vm insn)
  (if (not (attr-get vm :FNIL))
    (asm-jmp vm insn)))

(defun asm-jnil (vm insn)
  (if (attr-get vm :FNIL)
    (asm-jmp vm insn)))

(defun asm-move (vm insn)
  (let* ((source (second insn))
         (dest (third insn))
         (value (cond ((numberp source) source)
                      ((symbolp source) (attr-get vm source))
                      (t (error "Invalid source for MOVE: ~A" source)))))
    ;;(if (is-debug vm)
        ;;(format t "MOVE: Source ~A, Value ~A, Destination ~A~%" source value dest))
    (attr-set vm dest value)))

(defun asm-incr-decr (vm insn op)
  (let ((attr (second insn)))
    (if (keywordp attxr)
        (attr-set vm attr (funcall op (attr-get vm attr) 1)))))

(defun asm-incr (vm insn)
  (asm-incr-decr vm insn #'+))


(defun asm-ret (vm insn)
  (let ((return-addr (mem-get vm (- (attr-get vm :SP) 1))))
    (pc-set vm return-addr)
    (asm-pop vm '(POP :R0))))

(defun asm-decr (vm insn)
  (asm-incr-decr vm insn #'-))

(defun asm-push (vm insn)
  (let ((reg (second insn)))
    (when (>= (attr-get vm :SP) (attr-get vm :MAX_MEM))
      (error "Stack overflow"))
    (let ((value (attr-get vm reg)))
      ;;(format t "PUSH: Register ~A, Value ~A~%" reg value)
      (let ((sp (attr-get vm :SP)))
        (mem-set vm sp value)
        (attr-set vm :SP (+ sp 1))))))

(defun asm-load (vm insn)
  (let* ((source (second insn))
         (dest (third insn))
         (value (if (listp source)
                    (mem-get vm (+ (attr-get vm (first source)) (second source)))
                    (mem-get vm source))))
    ;;(if (is-debug vm)
        ;;(format t "LOAD: Source ~A, Value ~A, Destination ~A~%" source value dest)
    ;;)
    (attr-set vm dest value)))

(defun asm-store (vm insn)
  (let ((src (second insn)) (dst (third insn)))
    (let ((srcMapped (cond
                      ((is-const src) (second src))
                      ((keywordp src) (attr-get vm src))
                      ;;(t (format t "La source doit être soit une constante, soit un registre: ~A~%" insn))
                      ))
          (dstMapped (cond
                      ((numberp dst) dst)
                      ((keywordp dst) (attr-get vm dst))
                      ((is-offset dst) (+ (third dst) (attr-get vm (second dst))))))
          (isGlobalVar (is-global-var dst)))
      (if isGlobalVar
          (etiq-set vm (second dst) srcMapped)
          (mem-set vm dstMapped srcMapped)))))

(defun asm-pop (vm insn)
  (let ((reg (second insn)))
    (let ((sp (attr-get vm :SP)))
      (let ((value (mem-get vm sp)))
        ;;(format t "POP: Register ~A, Value ~A~%" reg value)
        (attr-set vm reg value)
        (attr-set vm :SP (- sp 1))))))

(defun asm-nop (vm insn)
  nil)

(defun asm-halt (vm insn)
  (set-running vm 0))

(defun asm-mult (vm insn)
  (let ((reg1 (second insn))
        (reg2 (third insn)))
    (let ((val1 (attr-get vm reg1))
          (val2 (attr-get vm reg2)))
      (attr-set vm reg1 (* val1 val2)))))
