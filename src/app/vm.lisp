(require "src/utils/assembly.lisp")
(require "src/utils/attribute.lisp")


;; Constants
(defconstant +SOC+ 'SOC)  ; Start of Code
(defconstant +EOC+ 'EOC)  ; End of Code
(defconstant +EOF+ 'EOF)  ; End of File
(defconstant +ETIQ+ 'ETIQ)  ; Labels/etiquettes table


;; Memory access functions
(defun mem-set (vm addr value)
  (when (or (< addr 0) (>= addr (length (attr-get vm :MEM))))
    (error "Memory access out of bounds: ~A" addr))
  ;;(format t "Setting memory at address ~A to value ~A~%" addr value)
  (setf (aref (attr-get vm :MEM) addr) value))

(defun mem-get (vm addr)
  
  (let ((value (aref (attr-get vm :MEM) addr)))
    value))

;; Variable management
(defun vm-variable-set (vm var value)
  ;;(format t "Setting variable ~A to value ~A~%" var value)
  (attr-set vm var value))

(defun vm-variable-get (vm var)
  (let ((value (attr-get vm var)))
    ;;(format t "Getting variable ~A: ~A~%" var value)
    value))

;; Program counter management4
(defun pc-set (vm value)
  (attr-set vm :PC value))

(defun pc-get (vm)
  (attr-get vm :PC))

(defun pc-decr (vm)
  (pc-set vm (- (pc-get vm) 1)))

;; Stack pointer management
(defun sp-set (vm value)
  (attr-set vm :SP value))

(defun sp-get (vm)
  (attr-get vm :SP))

;; Base pointer management
(defun bp-set (vm value)
  (attr-set vm :BP value))

(defun bp-get (vm)
  (attr-get vm :BP))

;; Frame pointer management
(defun fp-set (vm value)
  (attr-set vm :FP value))

(defun fp-get (vm)
  (attr-get vm :FP))

;; Memory size management
(defun ms-set (vm value)
  (attr-set vm :MS value))

(defun ms-get (vm)
  (attr-get vm :MS))

(defun update-labels-for-jumps (vm)
  (format t "Updating labels for jumps~%")
  (let ((code-start (vm-variable-get vm +SOC+))
        (code-end (vm-variable-get vm +EOC+)))
    (format t "Code range: ~A to ~A~%" code-end code-start)
    ;; First pass: register all labels
    (loop for addr from code-end to code-start do
      (let ((insn (mem-get vm addr)))
        (when (and (listp insn)
                   (eq (first insn) 'LABEL))
          (format t "Registering label: ~A at address ~A~%" (second insn) addr)
          (etiq-set vm (string (second insn)) addr))))
    
    ;; Print all registered labels
    (format t "Registered labels: ~A~%" (attr-get vm +ETIQ+))
    
    ;; Second pass: resolve jump targets
    (loop for addr from code-end to code-start do
      (let ((insn (mem-get vm addr)))
        (when (and (listp insn)
                   (member (first insn)
                           '(JUMP JMP JSR JGT JGE JLT JLE JEQ JNE JTRUE JNIL)))
          ;;(format t "Processing jump instruction at ~A: ~A~%" addr insn)
          (let* ((label (second insn))
                 (target (if (integerp label)
                             label
                             (etiq-get vm (string label)))))
                ;;(print (format nil "Label: ~A, Target: ~A" label target))
            (if target
                (progn
                  ;;(format t "Resolved label ~A to address ~A~%" label target)
                  (setf (second insn) target)
                  (mem-set vm addr insn))
                (format t "Warning: Unresolved label ~A~%" label))))))))

(defun vm-reset (vm &optional (size 1000))
  (let ((size (max size 1000))
        (base-vars 30)
        (zone-size (- (max size 1000) 30)))
    (attr-set vm :R0 0)
    (attr-set vm :R1 0)
    (attr-set vm :R2 0)
    (attr-set vm :FP 30)
    (attr-set vm :MAX_MEM size)
    (attr-array-init vm :MEM size)
    (vm-variable-set vm +SOC+ (- size 1))
    (vm-variable-set vm +ETIQ+ (make-hash-table :test 'equal))  ; Corrected from ETIQ to +ETIQ+
    (pc-set vm (- size 1))
    (bp-set vm base-vars)
    (sp-set vm (bp-get vm))
    (fp-set vm (sp-get vm))
    (ms-set vm (+ base-vars (/ zone-size 2)))
    (set-running vm 1)))

(defun vm-init (vm &optional (size 1000))
  (let ((size (max size 1000))
        (base-vars 30))
    ;; Initialize registers
    (attr-set vm :R0 0)
    (attr-set vm :R1 0)
    (attr-set vm :R2 0)
    (attr-set vm :FP base-vars)
    (attr-set vm :SP base-vars)
    (attr-set vm :BP base-vars)
    (attr-set vm :MAX_MEM size)
    ;; Initialize memory and labels
    (attr-array-init vm :MEM size)
    (attr-set vm +ETIQ+ (make-hash-table :test 'equal))
    ;; Set code boundaries
    (attr-set vm +SOC+ (- size 1))
    (attr-set vm +EOC+ (- size 1))
    (attr-set vm +EOF+ (- size 1))
    ;; Set initial PC and running state
    (attr-set vm :PC (- size 2))
    (attr-set vm :RUNNING 1)
    vm))

(defun make-vm (&optional (size 1000))
  (let ((vm (make-hash-table)))
    (vm-init vm size)
    vm))

(defun vm-load (vm program)
  (format t "Loading program into VM ~A ~%" program)
  (let ((initial-pc (or (- (vm-variable-get vm +EOC+) 1)
                          (+ (pc-get vm) 1))))
    (format t "Initial PC: ~A~%" initial-pc)
    (loop for insn in program do
      (if (is-label insn)
          (progn
            (format t "Registering label: ~A at address ~A~%" (second insn) initial-pc)
            (etiq-set vm (string (second insn)) initial-pc))
          (progn
            (format t "Loading instruction: ~A at address ~A~%" insn initial-pc)
            (mem-set vm initial-pc insn)
            (setq initial-pc (- initial-pc 1)))))
    (vm-variable-set vm +EOC+ (+ initial-pc 1))
    (update-labels-for-jumps vm)
    (format t "Program loaded. EOC: ~A~%" (vm-variable-get vm +EOC+))))

(defun vm-execute (vm)
  (format t "Executing VM~%")
  ;; display full code
  
  (when (is-debug vm) (format t "Code: ~A~%" (subseq (attr-get vm :MEM) (vm-variable-get vm +EOC+) (vm-variable-get vm +SOC+))))
  (loop while (and (>= (pc-get vm) (vm-variable-get vm +EOC+))
                   (is-running vm)) do
    ;; wait for enter to continue
    (when (is-debug vm) 
      (format t "Press enter to continue~%")
      (finish-output)
      (read-line)
    )

    (let ((insn (mem-get vm (pc-get vm))))
      (if (is-debug vm) (format t "Executing instruction: ~A~%" insn ))
      (case (first insn)
        (LABEL (asm-label vm insn))
        (RET (asm-ret vm insn))
        (PUSH (asm-push vm insn))
        (POP (asm-pop vm insn))
        (LOAD (asm-load vm insn))
        (STORE (asm-store vm insn))
        (ADD (asm-add vm insn))
        (SUB (asm-sub vm insn))
        (MUL (asm-mul vm insn))    ;; Use MUL consistently
        (MULT (asm-mul vm insn))   ;; Allow both MUL and MULT
        (JUMP (asm-jmp vm insn))   ;; Map JUMP to JMP
        (JMP (asm-jmp vm insn))    ;; Keep JMP
        (JSR (asm-jsr vm insn))
        (JGT (asm-jgt vm insn))
        (JGE (asm-jge vm insn))
        (JLT (asm-jlt vm insn))
        (JLE (asm-jle vm insn))
        (JEQ (asm-jeq vm insn))
        (JNE (asm-jne vm insn))
        (MOVE (asm-move vm insn))
        (CMP (asm-cmp vm insn))
        (TEST (asm-test vm insn))
        (JTRUE (asm-jtrue vm insn))
        (JNIL (asm-jnil vm insn))
        (HALT (asm-halt vm insn))
        (NOP (asm-nop vm insn))
        (t (format t "Unknown instruction: ~A~%" insn )
        ))
      (pc-decr vm)
      (when (is-debug vm)
        (format t "R0: ~A R1: ~A R2: ~A SP: ~A FP: ~A Stack: ~A~%"
                (attr-get vm :R0)
                (attr-get vm :R1)
                (attr-get vm :R2)
                (attr-get vm :SP)
                (attr-get vm :FP)
                (stack-get vm))))))

