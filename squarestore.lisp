;;; Implement a squarestore struct and functions.                                                                

;;; The squarestore struct.
(defstruct squarestore
  squares        ; A list of squares.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (squarestore-<field name> <instance>) -> struct field.
;   (squarestore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> squarestore
;   (typep <instance> 'squarestore) -> bool
;
; Probably shouldn't use:
;   (make-squarestore [:<field-name> <field-squarestore>]*), use squarestore-new instead.
;   (copy-squarestore <instance>) copies a squarestore instance.

;;; Return a new squarestore, given a list of squares.
(defun squarestore-new () ; -> squarestore.
  ;(format t "~&squarestore-new")

  (make-squarestore :squares nil) ; equalp can detect state struct equality.
)

;;; Add a square.
(defun squarestore-add(storex sqrx) ; -> side-effect, squarestore changed.
  ;(format t "~&squarestore-add ~A" (square-str sqrx))
  (assert (squarestore-p storex))
  (assert (square-p sqrx))

  (push sqrx (squarestore-squares storex)) 

  ;(format t "~&squarestore-add: find after ~A" (type-of (squarestore-find storex (square-state sqrx))))
)

;;; Find a square, given a state.
(defun squarestore-find (storex key) ; -> square, or nil.
  ;(format t "~&squarestore-find ~A" (state-str key))
  (assert (squarestore-p storex))
  (assert (state-p key))

  (loop for sqrx in (squarestore-squares storex) do
    (if (state-eq key (square-state sqrx))
      (return-from squarestore-find sqrx))
  )
  nil
)

;;; Return true if there is any square in a given region.
(defun squarestore-any-in (storex regx) ; -> bool
  (assert (squarestore-p storex))
  (assert (region-p regx))

  (loop for stax in (squarestore-squares storex) do
    (if (region-superset-of-state regx stax)
      (return-from squarestore-any-in true))
  )
  false
)

;;; Return square states in a given region.
(defun squarestore-states-in-region (storex regx) ; -> statestore instance.
  (assert (squarestore-p storex))
  (assert (region-p regx))

  (let ((ret (statestore-new nil)))
    (loop for stax in (squarestore-squares storex) do
      (if (region-superset-of-state regx stax)
        (statestore-push ret stax))
    )
    ret
  )
)

;;; Return squares in a given region.
(defun squarestore-squares-in-region (storex regx) ; -> square list.
  (assert (squarestore-p storex))
  (assert (region-p regx))

  (let (ret)
    (loop for sqrx in (squarestore-squares storex) do
      (if (region-superset-of-state regx (square-state sqrx))
        (push sqrx ret))
    )
    ret
  )
)

;;; Return true if there in a pnc square in a region.
(defun squarestore-pnc-square-in-region (storex regx) ; -> bool.
  (assert (squarestore-p storex))
  (assert (region-p regx))

  (loop for sqrx in (squarestore-squares storex) do
    (if (and (region-superset-of-state regx (square-state sqrx)) (square-pnc sqrx))
        (return-from squarestore-pnc-square-in-region true))
  )
  false
)

;;; Return a statestore of square keys.
(defun squarestore-keys (storex) ; -> statestore instance.
  (assert (squarestore-p storex))

  (let ((ret (statestore-new nil)))
    (loop for sqrx in (squarestore-squares storex) do
        (statestore-push ret (square-state sqrx))
    )
    ret
  )
)

;;; Return ntrue it a rulestore is valid, that is, not invalidated by ony square within its initial region.
(defun squarestore-rulestore-is-valid (storex regx rulstrx) ; -> bool
  ;(format t "~&squarestore-rulestore-is-valid")
  (assert (squarestore-p storex))
  (assert (region-p regx))
  (assert (rulestore-p rulstrx))

  (let ((sqrs (squarestore-squares-in-region storex regx)))

    (loop for sqrx in sqrs do
      (if (rulestore-invalidated-by-square rulstrx sqrx)
         (return-from squarestore-rulestore-is-valid false))
    )
    true
  )
)

;; Check if a region of states, representing squares, is valid.
(defun squarestore-region-is-valid (storex regx) ; -> bool
  ;(format t "~&squarestore-region-is-valid: ~A ~A" (region-str regx) (statestore-str (region-states regx)))
  (assert (squarestore-p storex))
  (assert (region-p regx))

  (let (rules sqrs sqrx region-pn region-defining-squares)

    ;; Find/verify region defining state(s) pn value.
    (loop for stax in (region-state-list regx) do
       ;; Get square represented by state.
       (setf sqrx (squarestore-find storex stax))
       (if (not sqrx)
           (error "Square not found?"))

       (if (null region-pn)
         (setf region-pn (square-pn sqrx))
         (if (pn-ne region-pn (square-pn sqrx))
           (error "Region states with different pn values?")))

       (push sqrx region-defining-squares)
    )

    ;; Get other squares in region.
    (setf sqrs (squarestore-squares-in-region storex regx))
    (loop for sqrx in region-defining-squares do
       (setf sqrs (remove sqrx sqrs))
    )
    
    ;; Check pn/pnc values of all squares that are in the region.
    (loop for sqrx in sqrs do
      (if (pn-gt (square-pn sqrx) region-pn)
        (return-from squarestore-region-is-valid false))

      (if (and (pn-lt (square-pn sqrx) region-pn) (square-pnc sqrx))
        (return-from squarestore-region-is-valid false))
    )

    ;; If region pn is unpredictable, done checking.
    (if (pn-eq region-pn *pn-none*)
      (return-from squarestore-region-is-valid true))

    ;; Calc the union of rules of squares represented by states in the region.
    (loop for sqrx in region-defining-squares do
      (if (null rules)
        (setf rules (square-rules sqrx))
        (setf rules (rulestore-union rules (square-rules sqrx))))

      (if (null rules)
        (return-from squarestore-region-is-valid false))
    )

    ;; Check other square rules in region.
    (loop for sqrx in sqrs do
      (if (not (rulestore-subset-of :sup rules :sub (square-rules sqrx)))
        (return-from squarestore-region-is-valid false))
    )

    true
  )
)

;;; Return a string of squares contained in a squarestore.
(defun squarestore-str (storex) ; -> string
  (assert (squarestore-p storex))

  (let ((ret ""))
    (loop for sqrx in (squarestore-squares storex) do
        (setf ret (concatenate 'string ret (format nil "~& ~A" (square-str sqrx))))
    )
    ret
  )
)
