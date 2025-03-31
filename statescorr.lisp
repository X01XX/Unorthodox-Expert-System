;;;; Implement a series of states, with bit-number values corresponding to a list of domains.




; Implement a store of corresponding states.
(defstruct statescorr
  states  ; A statestore of zero, or more, states.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (statescorr-<field name> <instance>) -> struct field.
;   (statescorr-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> statescorr
;   (typep <instance> 'statescorr) -> bool
;
; Probably shouldn't use:
;   (make-statescorr [:<field-name> <field-statescorr>]*), use statescorr-new instead.
;   (copy-statescorr <instance>) copies a statescorr instance.

;;; Return a new statescorr instance, from a list of states.
(defun statescorr-new (store) ; -> statescorr, or nil.
  ;(format t "~&statescorr-new: states ~A" store)
  (assert (statestore-p store))

  (make-statescorr :states store)
)

;;; Return a list of states for the StatesCorr.
(defun statescorr-state-list (scx) ; -> list of states.
  (statestore-states (statescorr-states scx))
)

;;; Add state to the end of a statescorr.
(defun statescorr-add-end (scx stax) ; -> nothing, side-effect statescorr changed.
  (assert (statescorr-p scx))
  (assert (state-p stax))

  (statestore-add-end (statescorr-states scx) stax)
)

;;; Return the number of states in a statescorr.
(defun statescorr-length (scx) ; -> number.
  ;(format t "~&statescorr-length: ~A" (type-of scx))
  (assert (statescorr-p scx))

  (statestore-length (statescorr-states scx))
)

;;; Return true if a statescorr is empty.
(defun statescorr-is-empty (scx) ; -> bool
  ;(format t "~&statescorr-is-empty: arg ~A" (type-of scx))
  (assert (statescorr-p scx))

  (statestore-is-empty (statescorr-states scx))
)

;;; Return true if a statescorr is not empty.
(defun statescorr-is-not-empty (scx) ; -> bool
  (assert (statescorr-p scx))

  (statestore-is-not-empty (statescorr-states scx))
)

;;; Return a string representing a statescorr.
(defun statescorr-str (scx) ; -> string.
  (assert (statescorr-p scx))

  (format nil "(SC ~A)" (statestore-str (statescorr-states scx)))
)

;;; Return true if two statescorr are equal.
(defun statescorr-eq (scx1 scx2) ; -> bool
  ;(format t "~&statescorr-eq: ~A ~A" scx1 scx2)
  (assert (statescorr-p scx1))
  (assert (statescorr-p scx2))
  (assert (statescorr-congruent scx1 scx2))

  (loop for sta1 in (statescorr-state-list scx1)
        for sta2 in (statescorr-state-list scx2) do
    (if (not (state-eq sta1 sta2))
      (return-from statescorr-eq false))
  )
  true
)

;;; Return true if two statescorr are not equal.
(defun statescorr-ne (stascorr1 stascorr2) ; -> bool
  (assert (statescorr-p stascorr1))
  (assert (statescorr-p stascorr2))
  (assert (statescorr-congruent stascorr1 stascorr2))

  (not (statescorr-eq stascorr1 stascorr2))
)

;;; Return true if a list is a list of statescorr.
;;; An empty list will return true.
(defun statescorr-list-p (state-list) ; -> bool
  ;; Check arg type.
  (if (not (listp state-list))
    (return-from statescorr-list-p false))

  (let (first-item)
    (loop for stax in state-list do

      ;; Check item type.
      (if (not (statescorr-p stax))
        (return-from statescorr-list-p false))

      ;; Check item characteristics.
      (if first-item
	(if (not (statescorr-congruent stax first-item))
          (return-from statescorr-list-p false))
	(setf first-item stax))
    )
    true
  )
)

;;; Return true if two statescorr have the same length and corresponding state-num-bits values.
(defun statescorr-congruent (statescorr1 statescorr2) ; -> bool
  ;(format t "~&statescorr-congruent: ~A ~A" statescorr1 statescorr2)
  (assert (statescorr-p statescorr1))
  (assert (statescorr-p statescorr2))

  (if (/= (statescorr-length statescorr1) (statescorr-length statescorr2))
    (return-from statescorr-congruent false))

  (loop for sta1 in (statescorr-state-list statescorr1)
	for sta2 in (statescorr-state-list statescorr2) do

    (if (/= (state-num-bits sta1) (state-num-bits sta2))
      (return-from statescorr-congruent false))
  )
  true
)

;;; Translate a list of symbols into a statescorr instance.
;;; Like (SC ()), (SC (1010)), or (SC (101, 1000)).
(defun statescorr-from (symbols) ; -> statescorr or error.
    ;(format t "~&statescorr-from ~A" (type-of symbols))
    (assert (listp symbols))
    (assert (not (null symbols)))
    (assert (symbolp (car symbols)))
    (assert (eq (car symbols) 'SC))     

    (setf symbols (second symbols))

    (statescorr-new (statestore-from symbols))
)
