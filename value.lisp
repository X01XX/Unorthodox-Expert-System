;;;; Implement the value struct and functions.

;;; The value struct.
;;; It holds a given number of bits.
(defstruct value
  num-bits  ; Number of bits used.
  bits      ; Bits value, zero to 2^num-bits - 1.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (value-<field name> <instance>) -> struct field.
;   (value-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> value
;   (typep <instance> 'value) -> bool
;
; Probably shouldn't use:
;   (make-value [:<field-name> <field-value>]*), use value-new instead.
;   (copy-value <instance>) copies a value instance.

;;; Return a new value instasnce.
(defun value-new (&key num-bits bits) ; -> value.
  (assert (integerp num-bits))
  (assert (plusp num-bits))
  (assert (integerp bits))
  (assert (>= bits 0))
  (assert (< bits (expt 2 num-bits)))

  (make-value :num-bits num-bits :bits bits)
)

;;; Given a symbol, like "v10", "v11_1100", 'v101, return a valid value.
;;; Underscore characters, which can be used as spacers, are ignored.
;;; All bits must be specified, since the number of bits is kept in the num-bits field in the struct.
(defun value-from (symx) ; -> value.
  ;(format t "~&value-from ~A" (type-of symbol))
  (assert (symbolp symx))

  (let ((strx (symbol-name symx)))
    (assert (> (length strx) 1))

    ;; Check for v prefix.
    (if (not (string-equal (subseq strx 0 1) "v"))
      (return-from value-from (err-new (format nil "Value ~A Should begin with an v character" strx))))

    (let ((ret (value-from-str strx)))
      (cond ((err-p ret) (error (err-str ret)))
            ((value-p ret) ret)
             (t (error "Value is invalid"))))
  )
)

;;; Get a value from a string, no-abort.
(defun value-from-str (strx) ; -> value, or err.
  ;(format t "~&value-from-str: ~A" (type-of strx))
  (assert (stringp strx))

  ;; Check for v prefix.
  (if (not (string-equal (subseq strx 0 1) "v"))
     (return-from value-from-str (err-new (format nil "value-from-str: Value ~A Should begin with a v character" strx))))

  (setf strx (subseq strx 1))

  (let (str2 num-bits valx)

    ;; Count digits, count digits, accumulate digits, skip underscores.
    (setf str2 "#b")
    (setf num-bits 0)
    (loop for chr across strx do
      (when (char/= chr #\_)
        (incf num-bits)
	    (if (or (char= chr #\0) (char= chr #\1))
	      (setf str2 (concatenate 'string str2 (princ-to-string chr)))
		  (return-from value-from-str (err-new (format nil "value-from-str: Invalid binary digit ~A" chr))))
      )
    ) ; end loop

    (if (zerop num-bits)
	  (return-from value-from-str (err-new (format nil "value-from-str: At least one bit must be given ~A" strx))))

    ;; Translate string to integer.
    (setf valx (read-from-string str2))

    ;; Create value to return.
    (value-new :num-bits num-bits :bits valx)
  )
)

;;; Add underscores for each 4 characters of a string, from right to left.
(defun string-add-underscores (str) ; -> string.
  (assert (stringp str))

  (let ((ret (string-add-underscores-na str)))
    (cond ((err-p ret) (error (err-str ret)))
	  ((stringp ret) ret)
	  (t (error "Result is not a string"))))
)
;;; Add underscores no-abort (na).
(defun string-add-underscores-na (str) ; -> string, or err.

  (let ((str2 "") cnt (str-len (length str)))
     (setf cnt str-len)
     (loop for chr across str do
       (if (char= chr #\_)
           (return-from string-add-underscores-na (err-new "Argument contains underscores")))

       (if (and (/= cnt str-len) (zerop (mod cnt 4)))
           (setf str2 (concatenate 'string str2 "_"))
       )
       (setf str2 (concatenate 'string str2 (princ-to-string chr)))
       (decf cnt)
     )
     str2
  )
)

;;; Return a string representation of a value.
;;; Use hexadecimal in preference to binary, if possible.
(defun value-str (val) ; -> string.
  (assert (value-p val))

  (let (str str-len val-len)

    (setf str (format nil (write-to-string (value-bits val) :base 2)))
    (setf str-len (length str))
    (setf val-len (value-num-bits val))

    (while (< str-len val-len)
      (setf str (concatenate 'string "0" str))
      (incf str-len)
    )
    (concatenate 'string "v" (string-add-underscores str))
  )
)

;;; Return true if a given value is zero.
(defun value-zerop (val) ; -> bool.
  (assert (value-p val))

  (zerop (value-bits val))
)

;;; Return the number of ones in a given value.
(defun value-num-ones (val) ; -> integer.
  (assert (value-p val))

  (logcount (value-bits val))
)

;;; Return the "not" bit value of a given value.
(defun value-not (val) ; -> value.
  (assert (value-p val))

  ; Create value to return.
  (value-new :num-bits (value-num-bits val) :bits (logxor (- (expt 2 (value-num-bits val)) 1) (value-bits val)))
)

;;; Return true if two given values are adjacent.
(defun value-is-adjacent (val1 val2) ; -> bool.
  (let ((ret (value-is-adjacent-na val1 val2)))
    (cond ((err-p ret) (error (err-str ret)))
	  ((bool-p ret) ret)
	  (t (error "Result is not a bool"))))
)
;;; value-is-adjacent no-abort (na).
(defun value-is-adjacent-na (val1 val2) ; -> bool, or err.
  (if (not (value-p val1))
    (return-from value-is-adjacent-na (err-new "Argument 1 is not a value")))

  (if (not (value-p val2))
    (return-from value-is-adjacent-na (err-new "Argument 2 is not a value")))

  (if (not (= (value-num-bits val1) (value-num-bits val2)))
    (return-from value-is-adjacent-na
		 (err-new "Argument 1 and 2 use a different number of bits")))

  (= 1 (value-num-ones (value-xor val1 val2)))
)

;;; Return true if two given values are equal.
(defun value-eq (val1 val2) ; -> bool.
  (assert (value-p val1))
  (assert (value-p val2))
  (assert (= (value-num-bits val1) (value-num-bits val2)))

  (= (value-bits val1) (value-bits val2))
)

;;; Return the "or" bit value of two, or more, values.
(defun value-or (&rest vals) ; -> value.
  (assert (> (length vals) 1))
  (assert (value-p (car vals)))

  (let* ((first-val (car vals))
	 (num-bits (value-num-bits first-val))
	 (ret-bits (value-bits first-val)))

    (loop for valx in (cdr vals) do
        (assert (value-p valx))
	(assert (= (value-num-bits valx) num-bits))

	(setf ret-bits (logior ret-bits (value-bits valx)))
    )
    (value-new :num-bits num-bits :bits ret-bits)
  )
)

;;; Return the "and" bit value of two, or more, given values.
(defun value-and (&rest vals) ; -> value.
  (assert (> (length vals) 1))
  (assert (value-p (car vals)))

  (let* ((first-val (car vals))
	 (num-bits (value-num-bits first-val))
	 (ret-bits (value-bits first-val)))

    (loop for valx in (cdr vals) do
        (assert (value-p valx))
	(assert (= (value-num-bits valx) num-bits))

	(setf ret-bits (logand ret-bits (value-bits valx)))
    )
    (value-new :num-bits num-bits :bits ret-bits)
  )
)

;;; Return the "xor" bit value of two given values.
(defun value-xor (val1 val2) ; -> value.
  (assert (value-p val1))
  (assert (value-p val2))
  (assert (= (value-num-bits val1) (value-num-bits val2)))

  ; Create value to return.
  (value-new :num-bits (value-num-bits val1) :bits (logxor (value-bits val1) (value-bits val2)))
)

;;; Return the "not xor" bit value of two given values.
(defun value-eqv (val1 val2) ; -> value.
  (assert (value-p val1))
  (assert (value-p val2))
  (assert (= (value-num-bits val1) (value-num-bits val2)))

  ; Create value to return.
  (value-not (value-xor val1 val2))
)

;;; Return a list of single-bit 1 values from a given value.
(defun value-split (val) ; -> list of values with only one bit, from input, set each
  (assert (value-p val))

  (let (ret (val2 (value-bits val)) val3 tmp (all-bits (1- (expt 2 (value-num-bits val)))))

    (loop while (> val2 0) do

       (setf val3 (1- val2))

       (setf tmp (logand (logxor val3 all-bits) val2))

       (push (value-new :num-bits (value-num-bits val) :bits tmp) ret)

       (setf val2 (logxor val2 tmp))

    ) ; end-loop
    ret
  )
)

;;; Given a value, and two others, return true if the first value is between the other two.
(defun value-between (&key target from to) ; -> bool
  (assert (value-p target))
  (assert (value-p from))
  (assert (value-p to))
  (assert (= (value-num-bits target) (value-num-bits from)))
  (assert (= (value-num-bits target) (value-num-bits to)))

  (value-zerop (value-and (value-xor target from) (value-xor target to)))
)

;;; Return a value with the most significant bit set to one.
(defun value-msb (val) ; -> value.
  (assert (value-p val))

  (value-new :num-bits (value-num-bits val) :bits (expt 2 (1- (value-num-bits val))))
)

;;; Return a value with bits shifted by a given value.
;;; A positive integer shifts left.
;;; A negative integer shifts right.
(defun value-shift (val num) ; -> value.
  (assert (value-p val))
  (assert (integerp num))
  (assert (<= (abs num) (value-num-bits val)))

  (let ((new-bits (ash (value-bits val) num)))
      (if (plusp new-bits)
	(setf new-bits (logand new-bits (1- (expt 2 (value-num-bits val))))) ; Some zero bits may have shifted too far left.
      )
      (value-new :num-bits (value-num-bits val) :bits new-bits)
  )
)

;;; Return true if a value is low.
(defun value-is-low (val) ; -> bool.
  (assert (value-p val))

  (zerop (value-bits val))
)

;;; Return true if a value is not low.
(defun value-is-not-low (val) ; -> bool.
  (assert (value-p val))

  (not (zerop (value-bits val)))
)

;;; Return true if a value is high.
(defun value-is-high (val) ; -> bool.
  (assert (value-p val))

  (= (1- (expt 2 (value-num-bits val))) (value-bits val))
)

;;; Return a value of the same number bits, with a high value.
(defun value-new-high (valx) ; -> value instance.
  (assert (value-p valx))

  (value-new :num-bits (value-num-bits valx) :bits (1- (expt 2 (value-num-bits valx))))
)

;;; Return a value of the same number bits, with a low value.
(defun value-new-low (valx) ; -> value instance.
  (assert (value-p valx))

  (value-new :num-bits (value-num-bits valx) :bits 0)
)

;;; Return a string for a value list;
(defun value-list-str (vlist) ; -> string
  (assert (listp vlist))

  (let ((ret "(") (first t))
    (loop for valx in vlist do
      (assert (value-p valx))
      (if first
        (setf first nil)
        (setf ret (concatenate 'string ret " ")))
      
      (setf ret (concatenate 'string ret (value-str valx)))
    )
    (setf ret (concatenate 'string ret ")"))
    ret
  )
)
