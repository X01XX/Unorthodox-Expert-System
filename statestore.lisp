;;;; Implement a statestore struct and functions.




;;; The statestore struct.
(defstruct statestore
  states  ; A list of zero, or more, non-duplicate, same number bits, states.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (statestore-<field name> <instance>) -> struct field.
;   (statestore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> statestore
;   (typep <instance> 'statestore) -> bool
;
; Probably shouldn't use:
;   (make-statestore [:<field-name> <field-statestore>]*), use statestore-new instead.
;   (copy-statestore <instance>) copies a statestore instance.

;;; Return a new statestore, given a list of states.
(defun statestore-new (states) ; -> statestore.
  ;(format t "~&states ~A" states)
  (assert (state-list-p states))

  (make-statestore :states states)
)

;;; Push a new state into a statestore, suppress dups.
(defun statestore-push (store state) ; -> nothing, side-effect statestore is changed.
  (assert (statestore-p store))
  (assert (state-p state))

  (if (not (statestore-member store state))
    (push state (statestore-states store)))
)

;;; Return the number of states in a statestore.
(defun statestore-length (storex) ; -> number.
  (assert (statestore-p storex))

  (length (statestore-states storex)))

;;; Return true if a statestore is empty.
(defun statestore-is-empty (storex) ; -> bool
  (assert (statestore-p storex))

  (zerop (statestore-length storex))
)

;;; Return true if a statestore is not empty.
(defun statestore-is-not-empty (storex) ; -> bool
  (assert (statestore-p storex))

  (plusp (statestore-length storex))
)

;;; Return a string representing a statestore.
(defun statestore-str (storex) ; -> string.
  ;(format t "~&statestore-str")
  (assert (statestore-p storex))

  (let ((ret "(") (start t))

    (loop for stax in (statestore-states storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))

      (setf ret (concatenate 'string ret (state-str stax)))
    )
    (setf ret (concatenate 'string ret ")"))

    ret)
)

;;; Return true if a statestore contains a given state.
(defun statestore-member (storex stax) ; -> bool
  (assert (statestore-p storex))
  (assert (state-p stax))

  (if (member stax (statestore-states storex) :test #'state-eq) true false)
)

;;; Return the first state of a non-empty statestore.
(defun statestore-first-state (storex) ; -> state
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (car (statestore-states storex))
)

;;; Return the last state of a non-empty statestore.
(defun statestore-last-state (storex) ; -> state
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (car (last (statestore-states storex)))
)

;;; Return the number of bits used by states in a non-empty statestore.
(defun statestore-num-bits (storex) ; -> number
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (state-num-bits (statestore-first-state storex))
)

;;; Return an x-mask for states in a statestore.
(defun statestore-x-mask (storex) ; -> mask
  ;(format t "~&statestore-x-mask ~A" storex)
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (let (ret (first-state (statestore-first-state storex)))

    (setf ret (value-new :num-bits (state-num-bits first-state) :bits 0))

    (loop for stax in (cdr (statestore-states storex)) do
       (setf ret (value-or ret (state-xor stax first-state)))
    )
    (mask-new ret)
  )
)

;;; Return the Boolean "or" of all states.
(defun statestore-or-all (storex) ; -> state
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (let ((ret (statestore-first-state storex)))
    (loop for stax in (cdr (statestore-states storex)) do
      (state-or ret stax)
    )
    ret
  )
)

;;; Return the Boolean "and" of all states.
(defun statestore-and-all (storex) ; -> state
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))

  (let ((ret (statestore-first-state storex)))
    (loop for stax in (cdr (statestore-states storex)) do
      (state-and ret stax)
    )
    ret
  )
)

;;; Return true if all states in a statestore use the same number of bits.
(defun statestore-same-num-bits (storex) ; -> bool
  (assert (statestore-p storex))
  (assert (statestore-is-not-empty storex))
  
  (if (< (statestore-length storex) 2)
    (return-from statestore-same-num-bits true))

  (let ((num-bits (state-num-bits (car (statestore-states storex)))))
    (loop for stax in (cdr (statestore-states storex)) do
      (if (/= (state-num-bits stax) num-bits)
        (return-from statestore-same-num-bits false))
    )
    true
  )
)

;;; Return a statestore with only states required to make a region.
(defun statestore-remove-unneeded (storex) ; -> statestore.
  (assert (statestore-p storex))
  
  (if (< (statestore-length storex) 3)
    (return-from statestore-remove-unneeded storex))

  (let (options (targ-x (statestore-x-mask storex)) opt-x storey)

    ;; Try combinations of successively more states.
    ;; Return first successful combination.
    (loop for num from 2 below (statestore-length storex) do

      (setf options (any-x-of-n num (statestore-states storex)))
      (loop for optx in options do

	    (setf storey (statestore-new optx))
        (setf opt-x (statestore-x-mask storey))

	    (if (mask-eq opt-x targ-x)
	      (return-from statestore-remove-unneeded storey))
      )
    )
  )
  ;; Store already at the minimum needed.
  storex
)

;;; Ruturn a statestore instance from a list of symbols.
(defun statestore-from (symbols) ; -> statestore
    ;(format t "~&statestore-from ~A" symbols)
    (assert (listp symbols))                                                                                                                    

    ;(assert (eq (car symbols) 'QUOTE))
    ;(setf symbols (second symbols))

    (let (states)
        (loop for tokx in symbols do
            ;(format t "~&statestore-from ~A ~A" (type-of tokx) tokx)
            (push (state-from tokx) states)
        ) 
        (statestore-new (reverse states))
    )
)

;;; Return two statestores combined, no dups.
(defun statestore-append (storex storey) ; -> statestore
  (let ((ret (statestore-new nil)))
    (loop for stax in (statestore-states storex) do
      (statestore-push ret stax)
    )
    (loop for stax in (statestore-states storey) do
      (statestore-push ret stax)
    )
    ret
  )
)

;;; Return a list of states.
(defun statestore-state-list (storex) ; -> list of states.
  (statestore-states storex)
)

;;; Add state to the end of a statestore.
(defun statestore-add-end (storex stax) ; -> nothing, side-effect statestore changed.
  (assert (statestore-p storex))
  (assert (state-p stax))

  (setf (statestore-states storex) (append (statestore-states storex) (list stax)))
)

