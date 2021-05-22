;;;; This is a testing program to load and test making bindings for c functions

;; This isn't loading for whatever reason
(defpackage :cl-ptrace
    (:use :common-lisp :sb-ext :cffi))

(in-package :cl-ptrace)

;; Telling lisp how to load libraries into the lisp image
(define-foreign-library libc
    (:unix (:or
            "/usr/lib/libc.so"
            "/usr/lib/libc-2.33.so"
	    "/usr/lib/x86_64-linux-gnu/libc-2.31.so")))

(use-foreign-library libc)

;;; Constants

(defconstant +ERROR-HEX+ #xffffffffffffffff)

(defconstant +PTRACE-TRACEME+ 0)

(defconstant +PTRACE-PEEKTEXT+ 1)
(defconstant +PTRACE-PEEKDATA+ 2)
(defconstant +PTRACE-PEEKUSER+ 3)

(defconstant +PTRACE-POKETEXT+ 4)
(defconstant +PTRACE-POKEDATA+ 5)
(defconstant +PTRACE-POKEUSER+ 6)

(defconstant +PTRACE-CONTINUE+ 7)
(defconstant +PTRACE-KILL+ 8)
(defconstant +PTRACE-STEP+ 9)

(defconstant +PTRACE-GET-REG+ 12)
(defconstant +PTRACE-SET-REG+ 13)

(defconstant +PTRACE-ATTACH+ 16)
(defconstant +PTRACE-DETACH+ 17)

(defconstant +PTRACE-SYSCALL+ 24)

(defconstant +PTRACE-GET-REG-CONTENT+ #x4204)
(defconstant +PTRACE-SET-REG-CONTENT+ #x4205)

(defconstant +PTRACE-SIEZE+ #x4206)

(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (defvar null-value (null-pointer)))
(defconstant +NULL+ null-value)

;;; Defining C types

(defcstruct register-contents
    (r15    :unsigned-long-long)
    (r14    :unsigned-long-long)
    (r13    :unsigned-long-long)
    (r12    :unsigned-long-long)
    (rbp    :unsigned-long-long)
    (rbx    :unsigned-long-long)
    (r11    :unsigned-long-long)
    (r10    :unsigned-long-long)
    (r9     :unsigned-long-long)
    (r8     :unsigned-long-long)
    (rax    :unsigned-long-long)
    (rcx    :unsigned-long-long)
    (rdx    :unsigned-long-long)
    (rsi    :unsigned-long-long)
    (rdi    :unsigned-long-long)
    (orig_rax :unsigned-long-long)
    (rip    :unsigned-long-long)
    (cs     :unsigned-long-long)
    (eflags :unsigned-long-long)
    (rsp    :unsigned-long-long)
    (ss     :unsigned-long-long)
    (fs_base :unsigned-long-long)
    (gs_base :unsigned-long-long)
    (ds     :unsigned-long-long)
    (es     :unsigned-long-long)
    (fs     :unsigned-long-long)
    (gs     :unsigned-long-long))

;;; Make function to fill the struct
;;; Make function to print the struct

(defctype pid-t :long)

(defcvar "errno" :int)
(declaim ((signed-byte 32) *errno*))
(get-var-pointer '*errno*)

;;; Binding to C functions

;; This is a bound function ot the C function waitpid in sys/wait.h. 
;; If -1 is returned; error, if the PID is returned; success.
(defcfun "waitpid" pid-t
    (pid pid-t) (status :pointer) (options :int))

;; This is a bound function to the C function ptrace in bits/ptrace-shared.h
;; If -1 is returned; error. *errno* will be '0' on success
(defcfun "ptrace" :unsigned-long
    (ptrace-request :unsigned-int) (pid pid-t) (addr :pointer) (data :pointer))

;;; Lisp variables

(defvar *pid* 0 "Default pid for functions")
(defvar *lisp-form* t "Determines whether functions simplify the output and return t or nil, if false, it will return regular C function output")
(defvar *verbose* t "Sets whether functions output text or not")

;;; Lisp macros

(defmacro read-register-content (register-symbol &optional (pid *pid*))
    "This function reads the data of the register specified"
    `(with-foreign-object (regs '(:struct register-contents))
        (%get-registers ,pid regs)
        (foreign-slot-value regs '(:struct register-contents) ,register-symbol)))

(defmacro rip-address () 
    "This macro returns the address to which the Instruction Pointer Register contains. The address is a number, unsigned long long to be exact."
    '(read-register-content 'rip))

(defmacro peek-rip ()
    "This macro is a shorthand to peek the data at the address that RIP contains"
    '(peekdata (rip-address)))

(defmacro load-short ()
    "This loads the CL-PTRACE system and switches to inside the package"
    (asdf:load-system :cl-ptrace)
    (in-package :cl-ptrace))

(defmacro register-short ()
    "Shorthand macro to set the global register variable to the process' register contents and then prints those registers"
    (setq *registers* (get-register-struct))
    (print-register-contents *registers*))

(defmacro pokedata (byte-offset data &key (pid *pid*) (bit-length (integer-length data)))
    `(let ((peeked-data (peekdata ,byte-offset ,pid nil nil)))
        (setf (ldb (byte ,bit-length 0) peeked-data) ,data)
        (pokedata-full-address ,byte-offset peeked-data)))

;;; Lisp functions

(defun pokedata-full-address (byte-offset data &optional (pid *pid*))
    (let ((ptrace-return-value (ptrace +PTRACE-POKEDATA+ pid (make-pointer byte-offset) (make-pointer data))))
        (ptrace-success-p ptrace-return-value)))

(defun hex-print (n)
    "This function prints hexadecimal equivalent numbers padded to 16 spots"
    (declare ((unsigned-byte 64) n))
    (format t "0x~(~16,'0x~)~%" n)
    t)

(defun peekdata (address &optional (pid *pid*) (hex-print-p t) (verbose *verbose*))
    (let ((data (ptrace +PTRACE-PEEKDATA+ pid (make-pointer address) +NULL+)))
        (if verbose (if hex-print-p (hex-print data) (format t "~D~%" data)))
        data))

(defun alloc-register-contents ()
    (foreign-alloc '(:struct register-contents)))

(defvar *registers* (alloc-register-contents) "Global variable to hold registers")

(defun free-register-contents (reg-struct)
    "A function that wraps around the foreign-free function from CFFI"
    (foreign-free reg-struct))

(defun inject-repl ()
    "This function acts as a REPL to step the process and watch the register contents"
    (format t "UNFINISHED~%")
    (let ((registers) (print-regs t))
        (setf registers (get-register-struct))
        (loop as user-choice = (progn
                                    (if print-regs (print-register-contents registers))
                                    (format t "(S): Step process | (Q): Quit Process~% | (I): Inject Data to address | (G): Get register content")
                                    (finish-output)
                                    (char-downcase (aref (string (read)) 0)))
            while (and (characterp user-choice) (char/= user-choice #\q))
            do (case user-choice
                ((#\i) (format t "Enter "))
                ((#\s) (step-process)
                    (setf registers (get-register-struct))
                    (setf print-regs t))
                (otherwise (format t "enter a valid character~%")
                    (setf print-regs nil))))
        (setq *registers* registers))
    t)

(defun step-repl ()
    "This function acts as a REPL to step the process and watch the register contents"
    (let ((registers) (print-regs t))
        (setf registers (get-register-struct))
        (loop as user-choice = (progn
                                    (if print-regs (print-register-contents registers))
                                    (format t "(S): Step process | (Q): Quit Process~%")
                                    (finish-output)
                                    (char-downcase (aref (string (read)) 0)))
            while (and (characterp user-choice) (char/= user-choice #\q))
            do (case user-choice
                ((#\s) (step-process)
                    (setf registers (get-register-struct))
                    (setf print-regs t))
                (otherwise (format t "enter a valid character~%")
                    (setf print-regs nil))))
        (setq *registers* registers))
    t)

(defun step-process (&optional (pid *pid*) (lisp-form *lisp-form*))
    "This function steps the process"
    (let ((ptrace-return-value (ptrace +PTRACE-STEP+ pid +NULL+ +NULL+)))
        (ptrace-success-p ptrace-return-value)
        (waitpid pid +NULL+ 0)
        (prog1 
            (if lisp-form (ptrace-success-p ptrace-return-value) ptrace-return-value))))

(defun %get-registers (pid reg-struct)
    "This function executes a PTRACE GET REGISTERS call and returns a C struct with those values"
    (progn (ptrace +PTRACE-GET-REG+ pid +NULL+ reg-struct)
        reg-struct))

(defun %c-reg-struct->lisp-list (reg-struct)
    "This function maps the C struct values to a Lisp list of register value pairs"
    (let ((register-list '(r15 r14 r13 r12 rbp rbx r11 r10 r9 r8 rax rcx rdx
                            rsi rdi orig_rax rip cs eflags rsp ss fs_base gs_base ds es fs gs)))
            (loop for register in register-list collect
                (cons register
                    (foreign-slot-value reg-struct '(:struct register-contents) register)))))

(defun get-register-struct (&optional (pid *pid*))
    "This function retrieves all the contents from each register and returns a struct of those registers"
    (with-foreign-object (regs '(:struct register-contents))
        (%get-registers pid regs)
        (%c-reg-struct->lisp-list regs)
        regs))

(defun print-register-contents (&optional (reg-struct *registers*))
    "This function is to print out the contents of all the registers belonging to the current process"
    (loop for register in '((r15 "general purpose registers")
                            (r14)
                            (r13)
                            (r12)
                            (rbp)
                            (rbx)
                            (r11)
                            (r10)
                            (r9 "6.")
                            (r8 "5.")
                            (rax)
                            (rcx "4. used for LOOPing in assembly..")
                            (rdx "3.")
                            (rsi "2.")
                            (rdi "1. function/syscall argument")
                            (orig_rax)
                            (rip "instruction pointer")
                            (cs)
                            (eflags "flags used for results of operations and CPU control")
                            (rsp "Stack Pointer to last item pushed on stack; grows to lower addresses")
                            (ss)
                            (fs_base)
                            (gs_base)
                            (ds)
                            (es)
                            (fs)     
                            (gs)) 
        :do
            (format t "~8a:~(~20x~) ~a~%" (car register) (foreign-slot-value reg-struct '(:struct register-contents) (car register)) (if (null (cadr register)) "" (cadr register))))
    t)

(defun ptrace-success-p (return-code &optional (verbose *verbose*))
    "This function tests whether the executed ptrace call was successful or not"
    (declare ((unsigned-byte 64) return-code))
    (if (and (= return-code +ERROR-HEX+) (/= *errno* 0))
        (progn 
            (if verbose (format t "Error: ~s" *errno*))
            (values nil return-code))
        t))

(defun attach-to (&optional (pid *pid*) (lisp-form *lisp-form*) (verbose *verbose*))
    "This function attaches the REPL to the specified process"
  (let ((ptrace-return-value (ptrace +PTRACE-ATTACH+ pid +NULL+ +NULL+)))
    (if (ptrace-success-p ptrace-return-value)
            (with-foreign-object (status :int)
                (waitpid pid status 0)
                (if verbose (progn
                    (format t "waitpid status: ~a~%" (mem-ref status :int))
                    (format t "Successfully attached to process ~a~%" pid)
                    t))
                (if lisp-form t pid))
            (if lisp-form nil ptrace-return-value))))

(defun detach-from (&optional (pid *pid*) (lisp-form *lisp-form*))
    "This function detaches teh REPL from the specified process"
    (let ((ptrace-return-value (ptrace +PTRACE-DETACH+ pid +NULL+ +NULL+)))
        (ptrace-success-p ptrace-return-value)
        (prog1 
            (if lisp-form (ptrace-success-p ptrace-return-value) ptrace-return-value))))
