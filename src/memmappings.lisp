;;;; This file is to organize all the functions which access the memory mappings of the current process

;; Make a standardized format to remember functions by:
;; -- a range of addresses is called a region. e.g., (- 111222333 777888999) is a region
;; -- search functions will return a list of addresses (1111 2222 3333 4444 5555 6666)
;; -- get functions return region(s)
;; -- snapshot macros return an address value pair

;; [X] removed comparison and parsing function from get-memory-regions function
;; [X] made a macro to specifically target the heap from a process
;; [X] make a function to search memory regions for a specific value
;; [ ] make a function to COMPLETELY stop multi-threaded processes

;; -- Aside/Rants --

;; I was erroring because my guess is that I was reading too small of address ranges
;; and when the REPL was trying to convert it to an unsigned bytes it was crashing. Why? great question.
;; The fix was fixing the value of +CHAR+ from 4 bits to 8 bits. Which it what it was supposed to be but
;; I can't understand how many bits are in a byte.

;; -- Future --

;; Actually use this library on a simple program like an emulator

;; Make a function to calculate the displacement of a certain address from the start of the program
;;   and save it to a file
;; e.g., calculate the displacement for a health variable

(in-package :cl-ptrace)

;; We are counting in bits, not bytes
(defconstant +BYTE+       (* 8 1)) ; 1 byte
(defconstant +WORD+       (* 8 2)) ; 2 bytes 
(defconstant +DOUBLEWORD+ (* 8 4)) ; 4 bytes
(defconstant +QUADWORD+   (* 8 8)) ; 8 bytes

;; defining variables for mem-mapping purposes
(defvar *sieve* '() "this is the place where the results of searching functions will output to")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snapshot Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro heap-snapshot (&optional (pid *pid*) (read-size +BYTE+))
  "This function is a short-hand to read and take the snapshot of the heap."
  `(let ((heap-range (get-heap-region ,pid)))
     (if (null heap-range)
	 nil
	 (collect-address-value-pairs heap-range ,read-size ,pid))))

(defmacro memory-snapshot (&optional (pid *pid*) (read-size +BYTE+))
  "This function takes a list of address ranges and then calls collect-address-value-pairs on them."
  `(apply #'append (loop for region in (get-memory-regions ,pid)
	collect (collect-address-value-pairs region ,pid ,read-size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro get-heap-region (&optional (pid *pid*))
  "Parses the memory regions of the process and returns the memory region of the heap"
  `(car (get-memory-regions ,pid #'parse-memory-regions "(heap)")))

(defun get-memory-regions (&optional (pid *pid*) (parser #'parse-memory-regions) (match-query "([r\-][w\-][x\-]p)"))
  "Reads the /proc/id/maps file and returns all the memory addresses associated with the process"
  (with-open-file (stream (concatenate 'string "/proc/" (write-to-string pid) "/maps"))
    (loop for line = (read-line stream nil)
	  while line
	  if (cl-ppcre:scan match-query line) ; this is to make sure I'm reading the memory regions that are private and not shared
	  collect (funcall parser line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro search-heap (target-value &optional (pid *pid*) (byte-padding t))
  "shorthand to search the heap for a value"
  `(setq *sieve* (search-region ,target-value :address-range (get-heap-region ,pid) :pid ,pid :byte-padding ,byte-padding)))

(defmacro search-sieve (target-value &optional (sieve '*sieve*) (pid *pid*) (byte-padding t))
  "Used to take a list of memory addresses and compare the values at those addresses to the target-value"
  `(search-region ,target-value :address-list ,sieve :pid ,pid :byte-padding ,byte-padding))

(defmacro search-all-regions (target-value &optional (pid *pid*) (byte-padding t))
  "This macro is a short-hands to search every memory region for the target-value"
  `(apply #'append (loop for range in (get-memory-regions ,pid)
	      collect (search-region ,target-value :address-range range :pid ,pid :byte-padding ,byte-padding))))

(defun search-region (target-value &key (pid *pid*)
				     (byte-padding t)
				     (address-range nil)
				     (address-list nil))
  "this function takes a target-value and a RANGE of addresses and searches all the addresses within the range to find the value. thanks k-stz"
  (let ((bits (if byte-padding
		  (* 8 (integer-byte-length target-value))
		  (integer-length target-value))))
    (cond
      ((not (null address-range))
       (loop for address from (car address-range) to (cadr address-range)
	     when (ends-with-bits? (peekdata address pid nil nil) target-value bits)
	       collect address))
      ((not (null address-list))
       (loop for address in address-list
	     when (ends-with-bits? (peekdata address pid nil nil) target-value bits)
	       collect address))
      (t ;; this is the fall through case where nothing is given
       nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun integer-byte-length (integer)
  "this function returns the amount of bytes it takes to represent the integer given. thanks k-stz"
  (let ((bytes (ceiling (integer-length integer)
			8)))
    (if (= bytes 0)
	1
	;; if the number is 0, return 1
	bytes)))

(defun ends-with-bits? (target-number match-number &optional (bits (integer-length target-number)))
    "Returns true if the bit representation of target-number ends with the bit representation of match-number. thanks k-stz"
    (= (ldb (byte bits 0) match-number)
       (ldb (byte bits 0) target-number)))
  
(defun collect-address-value-pairs (region &optional (pid *pid*) (read-size +BYTE+))
  "This function searches each memory address and returns it and its corresponding value."
  (loop for n from 0 to (/ (- (cadr region) (car region)) read-size)
	collect (list (+ (car region) (* n read-size)) (peekdata (+ (car region) (* n read-size)) pid nil nil))))

(defmacro print-memory-regions (&optional (pid *pid*) (read-size +BYTE+))
  "This function prints the number of addresses in each address range: [a, b]. Including a proper (- a b) form"
  `(let ((list-regions (get-memory-regions ,pid)))
     (format t "Read size: ~D byte~:*~[~;~:;s~]~%" (/ ,read-size 8))
     (loop for region in list-regions
	   do (format t "(- ~D ~D) >> ~D Addrs~%" (car region) (cadr region) (/ (- (cadr region) (car region)) ,read-size)))
     t))

(defun num-addresses (&optional (pid *pid*) (read-size +BYTE+))
  "Sums the amount of addresses used by the process"
  (let ((mappings (get-memory-regions pid)))
    (apply #'+ (loop for region in mappings
		     collect (/ (- (cadr region) (car region)) read-size)))))

(defun parse-memory-regions (string)
  "Function built to parse the /proc/id/maps file"
  (mapcar #'(lambda (x) (parse-integer x :radix 16)) (cl-ppcre:split "\-" (car (cl-ppcre:split "\\s+" string)))))
