;;;; Implement the sample struct.
;;;; It represents a initial state and a result of some action.
(defstruct sample
  initial ; A state, before an action.
  result  ; A state, after an action.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (sample-<field name> <instance>) -> struct field.
;   (sample-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> sample
;   (typep <instance> 'sample) -> bool
;
; Probably shouldn't use:
;   (make-sample [:<field-name> <field-sample>]*), use sample-new instead.
;   (copy-sample <instance>) copies a sample instance.
(defun sample-new (&key initial result) ; -> sample.
  (assert (state-p initial))
  (assert (state-p result))
  (assert (= (state-num-bits initial) (state-num-bits result)))

  (make-sample :initial initial :result result)
)

; Return a string to represent a sample.
(defun sample-str (smpl) ; -> string.
  (format nil "#S(SAMPLE ~A->~A)" (state-str (sample-initial smpl)) (state-str (sample-result smpl)))
)

;;; Return the number of bits used by a sample.
(defun sample-num-bits (smpl) ; -> number
  (assert (sample-p smpl))

  (state-num-bits (sample-initial smpl))
)

