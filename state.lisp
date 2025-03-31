;;;; Implement the state struct and functions.
;;;; It reresents a square on a K-Map.




;;; The state struct.
(defstruct state
  value  ; A value.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (state-<field name> <instance>) -> struct field.
;   (state-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> state
;   (typep <instance> 'state) -> bool
;
; Probably shouldn't use:
;   (make-state [:<field-name> <field-state>]*), use state-new instead.
;   (copy-state <instance>) copies a state instance.

;;; Return a new state.
(defun state-new (value) ; -> state.
  (assert (value-p value))

  (make-state :value value)
)

;;; Return a state of the same number bits, with a high value.
(defun state-new-high (stax) ; -> state instance.
  (assert (state-p stax))

  (make-state :value (value-new-high (state-value stax)))
)

;;; Return a state of the same number bits, with a low value.
(defun state-new-low (stax) ; -> state instance.
  (assert (state-p stax))

  (make-state :value (value-new-low (state-value stax)))
)

;;; Return a state instance from a symbol.
(defun state-from (symx) ; -> state.
    ;(format t "~&state-from ~A" (type-of symx))
    (assert (symbolp symx))

    (let ((stax (symbol-name symx)))

        (if (not (string-equal (subseq stax 0 1) "s"))
           (return-from state-from (err "State ~A should begin with an s character")))

        (state-from-str stax) 
   )
)

;;; Return a state instance from a string.
(defun state-from-str (strx) ; -> state.
    ;(format t "~&state-from ~A" (type-of strx))
    (assert (stringp strx))

    (if (not (string-equal (subseq strx 0 1) "s"))
       (return-from state-from-str (err "State ~A should begin with an s character")))

    (state-new (value-from-str (concatenate 'string "v" (subseq strx 1)))) 
)

;;; Return a string for a state.
(defun state-str (sta)  ; -> string.
  (assert (state-p sta))

  (format nil "s~A" (subseq (value-str (state-value sta)) 1))
)

;;; Return the number of bits used by a state.
(defun state-num-bits (sta) ; -> number
  (assert (state-p sta))

  (value-num-bits (state-value sta))
)

;;; Return t if two states are equal.
(defun state-eq (sta1 sta2) ; -> bool
  (assert (state-p sta1))
  (assert (state-p sta2))
  (assert (= (state-num-bits sta1) (state-num-bits sta2)))

  (value-eq (state-value sta1) (state-value sta2))
)

;;; Return the value of a state xor another state, mask, or value.
(defun state-xor (sta other) ; -> value inst.
  (assert (state-p sta))

  (cond ((state-p other)
         (assert (= (state-num-bits sta) (state-num-bits other)))
         (value-xor (state-value sta) (state-value other))
        )
        ((mask-p other)
         (assert (= (state-num-bits sta) (mask-num-bits other)))
         (value-xor (state-value sta) (mask-value other))
        )
        ((value-p other)
         (assert (= (state-num-bits sta) (value-num-bits other)))
         (value-xor (state-value sta) other)
        )
        (t (error "~&other type not expected ~A" (type-of other)))
  )
)

;;; Return true if a satate is between two others.
(defun state-between (sta1 sta2 sta3) ; -> bool
  (assert (state-p sta1))
  (assert (state-p sta2))
  (assert (state-p sta3))
  (assert (= (state-num-bits sta1) (state-num-bits sta2)))
  (assert (= (state-num-bits sta1) (state-num-bits sta3)))
  (assert (not (state-eq sta1 sta2)))
  (assert (not (state-eq sta1 sta3)))
  (assert (not (state-eq sta2 sta3)))

  (value-zerop (value-and (state-xor sta1 sta2) (state-xor sta1 sta3)))
)

;;; Return true if two states are not equal.
(defun state-ne (sta1 sta2) ; -> bool
  (assert (state-p sta1))
  (assert (state-p sta2))
  (assert (= (state-num-bits sta1) (state-num-bits sta2)))

  (not (state-eq sta1 sta2))
)

;;; Return the value of a state and another state, mask, or value.
(defun state-and (sta other) ; -> value inst.
  (assert (state-p sta))

  (cond ((state-p other)
         (assert (= (state-num-bits sta) (state-num-bits other)))
         (value-and (state-value sta) (state-value other))
        )
        ((mask-p other)
         (assert (= (state-num-bits sta) (mask-num-bits other)))
         (value-and (state-value sta) (mask-value other))
        )
        ((value-p other)
         (assert (= (state-num-bits sta) (value-num-bits other)))
         (value-and (state-value sta) other)
        )
        (t (error "~&other type not expected ~A" (type-of other)))
  )
)

;;; Return the value of a state or another state, mask, or value.
(defun state-or (sta other) ; -> value inst.
  (assert (state-p sta))

  (cond ((state-p other)
         (assert (= (state-num-bits sta) (state-num-bits other)))
         (value-or (state-value sta) (state-value other))
        )
        ((mask-p other)
         (assert (= (state-num-bits sta) (mask-num-bits other)))
         (value-or (state-value sta) (mask-value other))
        )
        ((value-p other)
         (assert (= (state-num-bits sta) (value-num-bits other)))
         (value-or (state-value sta) other)
        )
        (t (error "~&other type not expected ~A" (type-of other)))
  )
)

;;; Return the inverted, "not", value of a state.
(defun state-not (stax) ; -> value.
  (assert (state-p stax))

  (value-not (state-value stax))
)

;;; Return true if a list is a list of states.
;;; An empty list will return true.
(defun state-list-p (stelst) ; -> bool
  (if (not (listp stelst))
    (return-from state-list-p false))

  (loop for stax in stelst do
    (if (not (state-p stax))
      (return-from state-list-p false))
  )
  true
)

;;; Return a random state of a given number of bits.
(defun state-random (num-bits) ; -> state
    (assert (integerp num-bits))
    (assert (> num-bits 0))

    (let ((max (expt 2 num-bits)) rand-num)
        (setf rand-num (random max))
        (state-new (value-new :num-bits num-bits :bits rand-num))
    )
)

;;; Return true if two states are adjacent.
(defun state-is-adjacent (sta1 sta2) ; -> bool
  (value-is-adjacent (state-value sta1) (state-value sta2))
)

;;; Return the distance between two states.
(defun state-distance (sta1 sta2) ; -> integer
  (value-num-ones (state-xor sta1 sta2))
)

