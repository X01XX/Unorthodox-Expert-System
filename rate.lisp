;;; Implement a rate struct.
;;; It represents the value a certain set of domain regions.
(defstruct rate
  positive  ; A positive value, and its amaunt.
  negative  ; A negative value, and its amount.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (rate-<field name> <instance>) -> struct field.
;   (rate-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> rate
;   (typep <instance> 'rate) -> bool
;
; Probably shouldn't use:
;   (make-rate [:<field-name> <field-rate>]*), use rate-new instead.
;   (copy-rate <instance>) copies a rate instance.
(defun rate-new (&key positive negative) ; -> rate.
  (assert (and (integerp positive) (>= positive 0)))
  (assert (and (integerp negative) (<= negative 0)))

  (make-rate :positive positive :negative negative)
)

;;; Return the effect of a rate.
(defun rate-effect (ratex) ; -> String.
  (assert (rate-p ratex))

  (cond ((and (= (rate-positive ratex) 0) (= (rate-negative ratex) 0)) "Neutral")
        ((and (> (rate-positive ratex) 0) (= (rate-negative ratex) 0)) "Positive")
        ((and (= (rate-positive ratex) 0) (< (rate-negative ratex) 0)) "Negative")
        ((and (> (rate-positive ratex) 0) (< (rate-negative ratex) 0)) "Conflicted")
  )
)

;;; Return a string representation of a rate.
(defun rate-str (ratex) ; -> String.
  (assert (rate-p ratex))

  (format nil "(~D, ~D)" (rate-positive ratex) (rate-negative ratex))
)

;;; Return the rate net value.
(defun rate-net-value (ratex) ; -> integer.
  (assert (rate-p ratex))

  (+ (rate-positive ratex) (rate-negative ratex))
)

;;;; Return true if two rates are equal.
(defun rate-eq (ratex ratey) ; -> bool
  (assert (rate-p ratex))
  (assert (rate-p ratey))

  (and (= (rate-positive ratex) (rate-positive ratey))
       (= (rate-negative ratex) (rate-negative ratey)))
)

;;; Translate a list of symbols into a rate instance.
;;; Like (RT 0 -1)
(defun rate-from (symbols) ; -> rate
  ;(format t "~&rate-from ~A" (type-of symbols))
  (assert (listp symbols))                                                                                                         
  (assert (not (null symbols)))
  (assert (symbolp (car symbols)))
  (assert (eq (car symbols) 'RT))     

  (setf symbols (cdr symbols))

    ;(format t "~&rate-from2 ~A ~A" (type-of symbols) symbols)

  (let (pos neg)
    (if (/= 2 (length symbols))
      (error "The token list should have two parts"))

    (setf pos (car symbols))
    ;(format t "~&rate-from3 pos: ~A ~A" (type-of pos) pos)
           
    (setf neg (second symbols))
    ;(format t "~&rate-from4 neg: ~A ~A" (type-of neg) neg)
            
    (rate-new :positive pos :negative neg)
  )   
)

;;; Return the union of two rates.
(defun rate-union (ratex ratey) ; -> rate
  (assert (rate-p ratex))
  (assert (rate-p ratey))

  (rate-new :positive (+ (rate-positive ratex) (rate-positive ratey))
            :negative (+ (rate-negative ratex) (rate-negative ratey)))
)

