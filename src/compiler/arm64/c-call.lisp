;;;; VOPs and other machine-specific support routines for call-out to C.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; ARM64 follows calling convention described in [AAPCS64].
;;;; Mac OS X is based on the same ABI, but have some modifications [MACABI].
;;;; Both calling convention are summarized below for reference and for the case
;;;; when original document is difficult to find.
;;;; Notice that this documentation is added after the implementation,
;;;; thus specific details such as notation, names, etc. may differ.
;;;;
;;;; AAPCS64 argument passing is performed in the following stages:
;;;;
;;;; Stage A: Initialization, performed exactly once.
;;;; 1. Set Next General Purpose Register Number (NGRN) to zero.
;;;; 2. Set Next SIMD and FP Register Number (NSRN) to zero.
;;;; 3. Set Next Scalable Predicate Register Number (NPRN) to zero (not applicable).
;;;; 4. Set Next Stacked Argument Address (NSAA) set to current SP.
;;;;
;;;; Stage B: Pre-padding and extension.
;;;;    Match first rule for each argument, or pass unmodified if no match.
;;;; 1. If argument is pure scalable type, pass unmodified.
;;;; 2. If Composite Type whose size CANNOT be determined by BOTH caller and callee,
;;;;    copy to memory and replace by pointer (doesn't exist in C)
;;;; 3. If HFA or HVA, pass unmodified.
;;;; 4. If Composite Type with size > 16 bytes, copy to memory and replace by pointer.
;;;; 5. If Composite Type, round up to nearest multiples of 8 bytes.
;;;; 6. If alignment-adjusted type, pass a copy of the value such that:
;;;;    - For Fundamental Type, aligned to natural alignment of that type.
;;;;    - For Composite Type:
;;;;        * If natural alignment <= 8 bytes, align to 8 bytes.
;;;;        * If natural alignment >= 16 bytes, align to 16 bytes
;;;;    - Alignment of copy is used for applying marshaling rules (not applicable)
;;;;
;;;; Stage C: Assignment of arguments to registers and stack.
;;;;    Match each rule for each argument until argument has been allocated.
;;;;    When assigned to register, unused bits have unspecified value.
;;;;    When assigned to stack, padding bytes have unspecified value.
;;;; 1. If argument is half-, single-, double-, quad- float or short vector type and NSRN < 8:
;;;;    - Allocate to LSBs of v[NSRN] and increment NSRN.
;;;; 2. If HFA/HVA and there are enough SIMD and FP registers (NSRN + N members <= 8):
;;;;    - Allocate to SIMD and FP registers, one per member, and increase NSRN by N.
;;;; 3. If HFA/HVA, then set NSRA to 8 and round up size of argument to next multiple of 8 bytes.
;;;; 4. If NFA/HVA, quad-float, or short vector type:
;;;;    - If natural alignment <= 8, round up NSAA to next multiple of 8.
;;;;    - If natural alignment >= 16, round up NSAA to next multiple of 16.
;;;; 5. If half- or single- float, set size of argument to 8 bytes, remaining bytes unspecified.
;;;; 6. If HFA, HVA, half-, single-, double-, quad-float, or short vector type:
;;;;    - Allocate and copy to memory at adjusted NSAA; increment NSAA by size.
;;;; 7. If argument is pure scalable type consist of NV scalable vector types and NP scalable predicate types, if argument is named, if NSRN+NV <= 8, and if NPRN+NP <= 4:
;;;;    - Allocate Scalable Vector Types to z[NSRN]...z[NSRN+NV-1] inclusive; increment NSRN by NV.
;;;;    - Allocate Scalable Predicate Types to p[NPRN]...p[NPRN+NP-1] inclusive; increment NPRN by NP
;;;; 8. If Pure Scalable Type that has not been allocated by rules above:
;;;;    - Copy to memory and replace by pointer, then allocate according to rules below.
;;;; 9. If integral or pointer type, size <= 8 bytes, and NGRN < 8:
;;;;    - Allocate to LSBs in x[NGRN] and increment NGRN by 1.
;;;; 10. If argument has alignment of 16 bytes, round up NGRN to next even number.
;;;; 11. If integral type, size = 16, and NGRN < 7:
;;;;    - Allocate lower-address dword to x[NGRN] and high-address dword x[NGRN+1].
;;;;    - Increment NGRN by 2.
;;;; 12. If Composite Type and size in double words <= 8-NGRN:
;;;;    - Allocate to consecutive GPR starting at x[NGRN], as if loaded from dword-aligned address with LDR instructions.
;;;;    - Increment NGRN by registers used.
;;;; 13. NGRA is set to 8.
;;;; 14. NSAA is rounded up to max(8, Natural Alignment of Type).
;;;; 15. If Composite Type:
;;;;    - Allocate to memory at adjusted NSAA. Increment NSAA by size.
;;;; 16. If size < 8 bytes, set size to 8 bytes.
;;;; 17. Allocate to adjusted NSAA and increment NSAA by size.
;;;;
;;;; AAPCS64 return is performed as following:
;;;;
;;;; 1. If return type T is such that a function `void func(T arg)' requires passing T in a register:
;;;;    - Return result in the same registers as it would use for such argument.
;;;; 2. Otherwise:
;;;;    - Reserve memory of sufficient size and alignment.
;;;;    - Pass the address as additional argument in x8.
;;;;    - Note that x8 is not preserved by callee.
;;;;
;;;; AAPCS64 contains additional specification on variadic arguments:
;;;; - Varargs consist of ``named arguments'' and ``anonymous arguments''
;;;; - Regular function calls are like varargs with only named arguments
;;;; - In unprototyped and variadic C, ``__fp16'' and ``single float'' are converted to double
;;;;
;;;; Prologue of a function accepting varargs and which invokes ``va_start'' is expected to
;;;; save the incoming argument registers to two register save areas in its own stack frame:
;;;; - One area holding GP registers xn-x7
;;;; - One area holding FP/SIMD registers vn-v7
;;;; - Only those beyond named parameters need to be saved.
;;;; - If a function is known to never accept a certain class of registers, that save area may be omitted.
;;;; - In each area, registers are saved in ascending order.
;;;; - For memory format of FP/SIMD save area, registers must be saved as if each is saved using integer STR instruction for the entire register.
;;;;
;;;; The ``va_list'' argument is passed as the following structure:
;;;;
;;;; ```
;;;; typedef struct  va_list {
;;;;    void * stack; // next stack param
;;;;    void * gr_top; // end of GP arg reg save area
;;;;    void * vr_top; // end of FP/SIMD arg reg save area
;;;;    int gr_offs; // offset from  gr_top to next GP register arg
;;;;    int vr_offs; // offset from  vr_top to next FP/SIMD register arg
;;;;} va_list;
;;;; '''
;;;;
;;;; The ``va_start'' macro initializes fields of ``va_list'' argument as:
;;;; - Let ``named_gr'' be the number of GP registers known to hold named incoming args
;;;; - Let ``named_vr'' be the number of FP/SIMD registers known to hold named incoming args
;;;; - Set ``stack'' to:
;;;;    - Addr following last (highest addressed) named argument on stack, rounded up to next multiple of 8 bytes.
;;;;    - Or if no named arguments, value of stack pointer when function is entered.
;;;; - Set ``gr_top'' to:
;;;;    - Addr of the byte immediately following GP register save area, aligned to 16 bytes
;;;; - Set ``vr_top'' to:
;;;;    - Addr of the byte immediately following FP/SIMD register save area, aligned to 16 bytes
;;;; - Set ``gr_offs'' to (0 - ((8 - named_gr)) * 8)
;;;; - Set ``vr_offs'' to (0 - ((8 - named_vr)) * 16)
;;;; - If it's known that ``va_list'' is never used to access FP/SIMD registers:
;;;;    - No FP/SIMD registers need to be saved
;;;;    - ``vr_top'' and ``vr_offs'' are 0.
;;;;    - If GP register save area is immediately below stack pointer on entry:
;;;;        - ``stack'' can be set address of the anonymous argument in GP arg save area
;;;;        - ``gr_top'' and ``gr_offs'' are 0.
;;;; - The optimization above for FP/SIMD register cannot be used for GP registers.
;;;;
;;;; Mac OS X (Darwin) [MACABI] uses the arm64 calling convention, except for a few deviations.
;;;;
;;;; Registers:
;;;; - Register x18 is reserved and should not be used.
;;;; - Frame pointer (x29, FP) must always address a valid frame record.
;;;;    - Some functions (leaf, tail call, etc.) may opt not to create an entries in the list
;;;;
;;;; Data types:
;;;; - wchar_t is signed 32 bit type
;;;; - char is signed
;;;; - long is 64 bit
;;;; - __fp16 uses IEEE754-2008 format
;;;; - long double is IEEE754 double-float, instead of quad-float
;;;;
;;;; Stack red zone:
;;;; - 128 bytes below SP is ``red zone'' which is not touched by the OS
;;;; - If function calls itself, caller must assume callee can modify this zone
;;;;
;;;; Argument passing:
;;;; - Function arguments may consume slots on stack that are not multiple of 8 bytes.
;;;;    - If total number of bytes used for stack arguments is not multiple of 8:
;;;;        - Pad to multiple of 8 bytes.
;;;;    - Affects B.5, B.6 (TODO: TEST BOTH -- Rongcui), C.4, C.14, C.16
;;;; - When passing an argument with 16 byte alignment, it can start in odd-numbered xN register.
;;;; - Caller, instead of callee, perform signed/zero extension of arguments fewer than 32 bits.
;;;;    - NOTE: this means caller must not leave padding bits unspecified
;;;; - Functions may ignore params containing empty struct types.
;;;;    - This is unspecified by AAPCS64
;;;;
;;;; Variadic function:
;;;; - Arguments are initialized as Stage A and Stage B
;;;; - When assigning register and stack slots, follow these rules for each variadic arg:
;;;;    - Round up NSRN to the next multiple of 8 bytes.
;;;;    - Assign each variadic argument to appropriate 8-byte stack slots.
;;;; - In essense, Mac OS's ``va_list'' is represented by a ``char *'' instead of a structure
;;;;    - NOTE: that's paraphrased from Mac documentation. I think it's more like ``uint64_t *'' -- Rongcui
;;;; - TODO: verify whether slotting follows the AAPCS64 rules or the Mac OS rules
;;;;
;;;; Ref:
;;;; [AAPCS64] Procedure Call Standard for the Arm® 64-bit Architecture (AArch64), 2023Q3, ARM
;;;; [MACABI] Writing ARM64 code for Apple platforms, https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms


(in-package "SB-VM")

(defconstant +number-stack-alignment-mask+ (1- (* n-word-bytes 2)))

(defconstant +max-register-args+ 8)

(defstruct arg-state
  ;; NGRN
  (num-register-args 0)
  ;; NSRN
  (fp-registers 0)
  ;; NSAA = SP + stack-frame-size
  (stack-frame-size 0)
  (arg-ops nil))

(defstruct (result-state (:copier nil))
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 nl0-offset)
    (1 nl1-offset)))

(defun register-args-offset (index)
  (elt '#.(list nl0-offset nl1-offset nl2-offset nl3-offset nl4-offset nl5-offset nl6-offset
                      nl7-offset)
       index))

(define-vop (move-word-arg-stack)
  (:args (x :scs (signed-reg unsigned-reg single-reg))
         (fp :scs (any-reg)))
  (:info size offset)
  (:generator 0
    (let ((addr (@ fp (load-store-offset offset))))
      (ecase size
        (1
         (inst strb x addr))
        (2
         (inst strh x addr))
        (4
         (inst str (if (sc-is x single-reg)
                       x
                       (32-bit-reg x))
               addr))))))

(defun move-to-stack-location (value size offset prim-type sc node block nsp)
  (let ((temp-tn (sb-c:make-representation-tn
                  (primitive-type-or-lose prim-type)
                  sc)))
    (sb-c::emit-move node
                     block
                     (sb-c::lvar-tn node block value)
                     temp-tn)
    (sb-c::vop move-word-arg-stack node block temp-tn nsp size offset)))

;; Moving a struct of size 1 to 8 to register
(define-vop (move-struct-8-to-register)
  (:args (from-ptr :scs (sap-reg)))
  (:results (to-reg))
  (:generator 0
   (inst ldr to-reg (@ from-ptr))))

;; Moving a struct of size 9 to 16 to register
(define-vop (move-struct-16-to-registers)
  (:args (from-ptr :scs (sap-reg)))
  (:results (to-reg-l) (to-reg-h))
  (:generator 0
              (let ((addr-l (@ from-ptr))
                    (addr-h (@ from-ptr (load-store-offset 8))))
                (inst ldr to-reg-l addr-l)
                (inst ldr to-reg-h addr-h))))

(defun move-struct-to-registers (value size-dwords node block next-reg)
  (let ((temp-tn (sb-c:make-representation-tn
                  (primitive-type-or-lose 'system-area-pointer)
                  sap-reg-sc-number)))
    (sb-c::emit-move node block (sb-c::lvar-tn node block value) temp-tn)
    (ecase size-dwords
      (1
       (let ((reg-tn (make-wired-tn* 'unsigned-byte-64 unsigned-reg-sc-number next-reg)))
           (sb-c::vop move-struct-8-to-register node block temp-tn reg-tn)))
      (2
       (let ((reg-l-tn (make-wired-tn* 'unsigned-byte-64 unsigned-reg-sc-number next-reg))
             (reg-h-tn (make-wired-tn* 'unsigned-byte-64 unsigned-reg-sc-number (1+ next-reg))))
         (sb-c::vop move-struct-16-to-registers node block temp-tn reg-l-tn reg-h-tn))))))

(defun int-arg (state prim-type reg-sc stack-sc &optional (size 8))
  "Pass ints by AAPCS64: GP register (C.9) or stack (C.13, C.14, C.16, C.17)"
  (let ((reg-args (arg-state-num-register-args state)))
    (cond ((< reg-args +max-register-args+) ; C.9
           (setf (arg-state-num-register-args state) (1+ reg-args))
           (make-wired-tn* prim-type reg-sc (register-args-offset reg-args)))
          (t ; C.13 and below
           (let ((frame-size (align-up (arg-state-stack-frame-size state) size))) ; C.14
             (setf (arg-state-stack-frame-size state) (+ frame-size size))
             (cond #+darwin ; On Darwin, integer can consume non-8-byte slots; C.17
                   ((/= size n-word-bytes)
                    (lambda (value node block nsp)
                      (move-to-stack-location value size frame-size
                                              prim-type reg-sc node block nsp)))
                   (t ; C.16, C.17
                    (make-wired-tn* prim-type stack-sc (truncate frame-size size)))))))))

(defun float-arg (state prim-type reg-sc stack-sc &optional (size 8))
  "Pass floats by AAPCS64: FP register (C.1) or stack (C.5, C.6)"
  (let ((reg-args (arg-state-fp-registers state)))
    (cond ((< reg-args +max-register-args+) ; C.1
           (setf (arg-state-fp-registers state) (1+ reg-args))
           (make-wired-tn* prim-type reg-sc reg-args))
          (t ; C.5 and below
           (let ((frame-size (align-up (arg-state-stack-frame-size state) size)))
             (setf (arg-state-stack-frame-size state) (+ frame-size size))
             (cond #+darwin ; On Darwin, floats can consume non-8-byte slots; C.6
                   ((/= size n-word-bytes)
                    (lambda (value node block nsp)
                      (move-to-stack-location value size frame-size
                                              prim-type reg-sc node block nsp)))
                   (t ; C.5, C.6
                    (make-wired-tn* prim-type stack-sc (truncate frame-size size)))))))))

(defun record-arg-small (type state)
  "Records <= 16 bytes: B.4, C.12. Pass by registers if possible."
  (let ((reg-args (arg-state-num-register-args state))
        (size-dwords (ceiling (alien-type-bits type) n-word-bits)))
    (cond
      ;; C.12, if we can fit the argument in register, copy to consecutive registers
      ((< size-dwords (- +max-register-args+ reg-args))
       (setf (arg-state-num-register-args state) (+ size-dwords reg-args))
       (lambda (value node block nsp)
         (declare (ignore nsp))
         (move-struct-to-registers value size-dwords node block reg-args)))
      ;; Otherwise, pass by stack
      (t
       (error "WIP")
       ;; C.13, set NGRA to 8
       (setf (arg-state-num-register-args state) +max-register-args+)
       ;; C.14 Align NSAA
       (let* ((frame-size (align-up (arg-state-stack-frame-size state) 8))
              (offset (truncate frame-size 8)))
         (setf (arg-state-stack-frame-size state) (+ frame-size size-dwords))
         (loop for off from offset upto (+ offset size-dwords)
               collect (make-wired-tn* 'unsigned-byte-64 unsigned-stack-sc-number offset)))))))

(defun record-arg-large (type state)
  (declare (ignore type))
  ;; FIXME: we need a copy, not the original. Will fix later.
  ;; Structs >16B are passed by pointer. SBCL already holds struct as SAP.
  (warn "FIXME: Large structs pass by value does not not copy yet.")
  (int-arg state 'system-area-pointer sap-reg-sc-number sap-stack-sc-number))

(define-alien-type-method (integer :arg-tn) (type state)
 (let ((size #+darwin (truncate (alien-type-bits type) n-byte-bits)
             #-darwin n-word-bytes))
   (if (alien-integer-type-signed type)
       (int-arg state 'signed-byte-64 signed-reg-sc-number signed-stack-sc-number
                size)
       (int-arg state 'unsigned-byte-64 unsigned-reg-sc-number unsigned-stack-sc-number
                size))))

(define-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer sap-reg-sc-number sap-stack-sc-number))

(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg state 'single-float single-reg-sc-number single-stack-sc-number #+darwin 4))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg state 'double-float double-reg-sc-number double-stack-sc-number))

(define-alien-type-method (sb-alien::record :arg-tn) (type state)
  (let* ((bits (alien-type-bits type))
         (size (truncate (or bits 0) n-byte-bits)))
    (cond
      ;; Darwin specifies that empty records are ignored.
      ;; However, notice that empty structs are not portable.
      #+darwin
      ((zerop size)
       (lambda (value node block nsp) (declare (ignore value node block nsp))))
      ((<= size 16) (record-arg-small type state))
      (t (record-arg-large type state)))))

(define-alien-type-method (sb-alien::record :result-tn) (type state)
  (declare (ignore type state))
  (error "WIP arm64 struct value return."))
;;

(defknown sign-extend ((signed-byte 64) t) fixnum
    (foldable flushable movable))

(defoptimizer (sign-extend derive-type) ((x size))
  (when (sb-c:constant-lvar-p size)
    (specifier-type `(signed-byte ,(sb-c:lvar-value size)))))

(define-vop (sign-extend)
  (:translate sign-extend)
  (:policy :fast-safe)
  (:args (val :scs (signed-reg)))
  (:arg-types signed-num (:constant fixnum))
  (:info size)
  (:results (res :scs (signed-reg)))
  (:result-types fixnum)
  (:generator 1
    (check-type size (member 8 16 32))
    (inst sbfm res val 0 (1- size))))

#-sb-xc-host
(defun sign-extend (x size)
  (declare (type (signed-byte 64) x))
  (ecase size
    (8 (sign-extend x size))
    (16 (sign-extend x size))
    (32 (sign-extend x size))))

(define-alien-type-method (integer :naturalize-gen) (type alien)
  (if (<= (alien-type-bits type) 32)
      (if (alien-integer-type-signed type)
          `(sign-extend ,alien ,(alien-type-bits type))
          `(logand ,alien ,(1- (ash 1 (alien-type-bits type)))))
      alien))

(define-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
        (if (alien-integer-type-signed type)
            (values 'signed-byte-64 signed-reg-sc-number)
            (values 'unsigned-byte-64 unsigned-reg-sc-number))
      (make-wired-tn* ptype reg-sc
                        (result-reg-offset num-results)))))

(define-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type state))
  (make-wired-tn* 'system-area-pointer sap-reg-sc-number (result-reg-offset 0)))


(define-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  (make-wired-tn* 'single-float single-reg-sc-number 0))

(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (make-wired-tn* 'double-float double-reg-sc-number 0))

(define-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error "Too many result values from c-call."))
    (mapcar (lambda (type)
              (invoke-alien-type-method :result-tn type state))
            values)))

(defun natural-alignment (type)
  "Find the natural alignment of a type."
  (declare (ignore type))
  (error "TODO"))

(defun make-call-out-tns (type)
  (let* ((arg-state (make-arg-state)))
    ;; Create TNs
    (collect ((arg-tns))
      (let (#+darwin (variadic (sb-alien::alien-fun-type-varargs type)))
        (loop for i from 0
              for arg-type in (alien-fun-type-arg-types type)
              do
              #+darwin ; In Darwin, variadic args is passed on stack slots
              (when (eql i variadic)
                (setf (arg-state-num-register-args arg-state) +max-register-args+
                      (arg-state-fp-registers arg-state) +max-register-args+))
              (arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state))))
      (values (make-normal-tn *fixnum-primitive-type*)
              (arg-state-stack-frame-size arg-state)
              (arg-tns)
              (invoke-alien-type-method :result-tn
                                        (alien-fun-type-result-type type)
                                        (make-result-state))))))

(define-vop (foreign-symbol-sap)
  (:translate foreign-symbol-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (load-foreign-symbol res foreign-symbol)))

(define-vop (foreign-symbol-dataref-sap)
  (:translate foreign-symbol-dataref-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (load-foreign-symbol res foreign-symbol :dataref t)))

#+sb-safepoint
(defconstant thread-saved-csp-slot (- (1+ sb-vm::thread-header-slots)))

(defun emit-c-call (vop nfp-save temp temp2 cfunc function)
  (let ((cur-nfp (current-nfp-tn vop)))
    (when cur-nfp
      (store-stack-tn nfp-save cur-nfp))
    (assemble ()
      #+sb-thread
      (progn
        (inst add temp csp-tn (* 2 n-word-bytes))
        ;; Build a new frame to stash a pointer to the current code object
        ;; for the GC to see.
        (inst adr temp2 return)
        (inst stp cfp-tn temp2 (@ csp-tn))
        (storew-pair csp-tn thread-control-frame-pointer-slot temp thread-control-stack-pointer-slot thread-tn)
        ;; OK to run GC without stopping this thread from this point
        ;; on.
        #+sb-safepoint
        (storew csp-tn thread-tn thread-saved-csp-slot)
        (cond ((stringp function)
               (invoke-foreign-routine function cfunc))
              (t
               (sc-case function
                 (sap-reg (move cfunc function))
                 (sap-stack
                  (load-stack-offset cfunc cur-nfp function)))
               (inst blr cfunc)))
        (loop for reg in (list r0-offset r1-offset r2-offset r3-offset
                               r4-offset r5-offset r6-offset r7-offset
                               #-darwin r8-offset)
              do
              (inst mov
                    (make-random-tn
                     :kind :normal
                     :sc (sc-or-lose 'descriptor-reg)
                     :offset reg)
                    0))
        ;; No longer OK to run GC except at safepoints.
        #+sb-safepoint
        (storew zr-tn thread-tn thread-saved-csp-slot)
        (storew zr-tn thread-tn thread-control-stack-pointer-slot))
      return
      #-sb-thread
      (progn
        temp2
        (if (stringp function)
            (load-foreign-symbol cfunc function)
            (sc-case function
              (sap-reg (move cfunc function))
              (sap-stack
              (load-stack-offset cfunc cur-nfp function))))
        (invoke-foreign-routine "call_into_c" temp))
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun destroyed-c-registers ()
    (let ((gprs (list nl0-offset nl1-offset nl2-offset nl3-offset
                      nl4-offset nl5-offset nl6-offset nl7-offset nl8-offset #| tmp-offset |#
                      r0-offset r1-offset r2-offset r3-offset
                      r4-offset r5-offset r6-offset r7-offset
                      #-darwin r8-offset
                      #-sb-thread r11-offset))
          (vars))
      (append
       (loop for gpr in gprs
             collect `(:temporary (:sc any-reg :offset ,gpr :from :eval :to :result)
                                  ,(car (push (gensym) vars))))
       (loop for float to 31
             collect `(:temporary (:sc single-reg :offset ,float :from :eval :to :result)
                                  ,(car (push (gensym) vars))))
       `((:ignore ,@vars))))))

(define-vop (call-out)
  (:args (function :scs (sap-reg sap-stack))
         (args :more t))
  (:results (results :more t))
  (:ignore args results lr)
  (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)
  (:temporary (:sc any-reg :offset r9-offset
               :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:sc any-reg :offset #+darwin r8-offset #-darwin r10-offset) temp)
  (:temporary (:sc any-reg :offset lexenv-offset) temp2)
  (:vop-var vop)
  (:generator 0
    (emit-c-call vop nfp-save temp temp2 cfunc function))
  .
  #. (destroyed-c-registers))

;;; Manually load the fixup instead of using foreign-symbol-sap,
;;; because it wants to go to r9, which is not compatible with sap-reg.
(define-vop (call-out-named call-out)
  (:args (args :more t))
  (:info function variadic)
  (:ignore args results variadic lr))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:result-types system-area-pointer)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount +number-stack-alignment-mask+)
                             +number-stack-alignment-mask+)))
        (inst sub nsp-tn nsp-tn (add-sub-immediate delta))
        (inst mov-sp result nsp-tn)))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount +number-stack-alignment-mask+)
                             +number-stack-alignment-mask+)))
        (inst add nsp-tn nsp-tn (add-sub-immediate delta))))))

;;; Callback
#-sb-xc-host
(defun alien-callback-accessor-form (type sap offset)
  (let ((parsed-type type))
    (if (alien-integer-type-p parsed-type)
        (let ((bits (sb-alien::alien-integer-type-bits parsed-type)))
               (let ((byte-offset
                      (cond ((< bits n-word-bits)
                             (- n-word-bytes
                                (ceiling bits n-byte-bits)))
                            (t 0))))
                 `(deref (sap-alien (sap+ ,sap
                                          ,(+ byte-offset offset))
                                    (* ,type)))))
        `(deref (sap-alien (sap+ ,sap ,offset) (* ,type))))))

#-sb-xc-host
(defun alien-callback-assembler-wrapper (index result-type argument-types)
  (flet ((make-tn (offset &optional (sc-name 'any-reg))
           (make-random-tn :kind :normal
                           :sc (sc-or-lose sc-name)
                           :offset offset)))
    (let* ((segment (make-segment))
           ;; How many arguments have been copied
           (arg-count 0)
           ;; How many arguments have been copied from the stack
           (stack-argument-bytes 0)
           (r0-tn (make-tn 0))
           (r1-tn (make-tn 1))
           (r2-tn (make-tn 2))
           (r3-tn (make-tn 3))
           (r4-tn (make-tn 4))
           (temp-tn (make-tn 9))
           (nsp-save-tn (make-tn 10))
           (gprs (loop for i below 8
                       collect (make-tn i)))
           (fp-registers 0)
           (frame-size (* (length argument-types) n-word-bytes)))
      (setf frame-size (logandc2 (+ frame-size +number-stack-alignment-mask+)
                                 +number-stack-alignment-mask+))
      (assemble (segment 'nil)
        (inst mov-sp nsp-save-tn nsp-tn)
        (inst str lr-tn (@ nsp-tn -16 :pre-index))
        ;; Make room on the stack for arguments.
        (when (plusp frame-size)
          (inst sub nsp-tn nsp-tn frame-size))
        ;; Copy arguments
        (dolist (type argument-types)
          (let ((target-tn (@ nsp-tn (* arg-count n-word-bytes)))
                (size #+darwin (truncate (alien-type-bits type) n-byte-bits)
                      #-darwin n-word-bytes))
            (cond ((or (alien-integer-type-p type)
                       (alien-pointer-type-p type)
                       (alien-type-= #.(parse-alien-type 'system-area-pointer nil)
                                     type))
                   (let ((gpr (pop gprs)))
                     (cond (gpr
                            (inst str gpr target-tn))
                           (t
                            (setf stack-argument-bytes
                                  (align-up stack-argument-bytes size))
                            (let ((addr (@ nsp-save-tn stack-argument-bytes)))
                              (cond #+darwin
                                    ((/= size 8)
                                     (let ((signed (and (alien-integer-type-p type)
                                                        (alien-integer-type-signed type))))
                                       (ecase size
                                         (1
                                          (if signed
                                              (inst ldrsb temp-tn addr)
                                              (inst ldrb temp-tn addr)))
                                         (2
                                          (if signed
                                              (inst ldrsh temp-tn addr)
                                              (inst ldrh temp-tn addr)))
                                         (4
                                          (if signed
                                              (inst ldrsw (32-bit-reg temp-tn) addr)
                                              (inst ldr (32-bit-reg temp-tn) addr))))))
                                    (t
                                     (inst ldr temp-tn addr)))
                              (inst str temp-tn target-tn))
                            (incf stack-argument-bytes size))))
                   (incf arg-count))
                  ((alien-float-type-p type)
                   (cond ((< fp-registers 8)
                          (inst str (make-tn fp-registers
                                             (if (alien-single-float-type-p type)
                                                 'single-reg
                                                 'double-reg))
                                target-tn))
                         (t
                          (setf stack-argument-bytes
                                  (align-up stack-argument-bytes size))
                          (case size
                            #+darwin
                            (4
                             (let ((reg (32-bit-reg temp-tn)))
                              (inst ldr reg (@ nsp-save-tn stack-argument-bytes))
                              (inst str reg target-tn)))
                            (t
                             (inst ldr temp-tn (@ nsp-save-tn stack-argument-bytes))
                             (inst str temp-tn target-tn)))
                          (incf stack-argument-bytes size)))
                   (incf fp-registers)
                   (incf arg-count))
                  (t
                   (bug "Unknown alien type: ~S" type)))))
        ;; arg0 to FUNCALL3 (function)
        (load-immediate-word r0-tn (static-fdefn-fun-addr 'enter-alien-callback))
        (loadw r0-tn r0-tn)
        ;; arg0 to ENTER-ALIEN-CALLBACK (trampoline index)
        (inst mov r1-tn (fixnumize index))
        ;; arg1 to ENTER-ALIEN-CALLBACK (pointer to argument vector)
        (inst mov-sp r2-tn nsp-tn)
        ;; add room on stack for return value
        (inst sub nsp-tn nsp-tn (* n-word-bytes 2))
        ;; arg2 to ENTER-ALIEN-CALLBACK (pointer to return value)
        (inst mov-sp r3-tn nsp-tn)

        ;; Call
        (load-immediate-word r4-tn (foreign-symbol-address
                                    #-sb-thread "funcall3"
                                    #+sb-thread "callback_wrapper_trampoline"))
        (inst blr r4-tn)

        ;; Result now on top of stack, put it in the right register
        (cond
          ((or (alien-integer-type-p result-type)
               (alien-pointer-type-p result-type)
               (alien-type-= #.(parse-alien-type 'system-area-pointer nil)
                             result-type))
           (loadw r0-tn nsp-tn))
          ((alien-float-type-p result-type)
           (loadw (make-tn fp-registers
                              (if (alien-single-float-type-p result-type)
                                  'single-reg
                                  'double-reg))
                 nsp-tn))
          ((alien-void-type-p result-type))
          (t
           (error "Unrecognized alien type: ~A" result-type)))
        (inst add nsp-tn nsp-tn (+ frame-size (* n-word-bytes 2)))
        (inst ldr lr-tn (@ nsp-tn 16 :post-index))
        (inst ret))
      (finalize-segment segment)
      ;; Now that the segment is done, convert it to a static
      ;; vector we can point foreign code to.
      (let* ((buffer (sb-assem:segment-buffer segment))
             (vector #-darwin-jit
                     (make-static-vector (length buffer)
                                         :element-type '(unsigned-byte 8)
                                         :initial-contents buffer)
                     #+darwin-jit
                     (make-static-code-vector (length buffer)
                                              buffer))
             (sap (vector-sap vector)))
        (alien-funcall
         (extern-alien "os_flush_icache"
                       (function void
                                 system-area-pointer
                                 unsigned-long))
         sap (length buffer))
        vector))))
