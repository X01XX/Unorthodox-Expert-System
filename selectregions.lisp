;;;; Implement the selectregions struct and functions.




;;; The selectregions struct.
(defstruct selectregions
  regionscorr   ; A store of correspondung regions.
  rate          ; A rate instance, indicating a value for matching Domain regions.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (selectregions-<field name> <instance>) -> struct field.
;   (selectregions-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> selectregions
;   (typep <instance> 'selectregions) -> bool
;
; Probably shouldn't use:
;   (make-selectregions [:<field-name> <field-selectregions>]*), use selectregions-new instead.
;   (copy-selectregions <instance>) copies a selectregions instance.

;;; Return a new selectregions, made up of corresponding regions and a value.
(defun selectregions-new (regions ratex) ; -> selectregions.
  (assert (regionscorr-p regions))
  (assert (> (regionscorr-length regions) 0))
  (assert (rate-p ratex))

  (make-selectregions :regionscorr regions :rate ratex)
)

;;; Return a list of regions.
(defun selectregions-region-list (sregsx) ; -> A list ogf regions.
  (regionscorr-region-list (selectregions-regionscorr sregsx))
)

;;; Return a string for a selectregions.
(defun selectregions-str (sregsx)  ; -> string.
  (assert (selectregions-p sregsx))

  (format nil "SR ~A ~A" (regionscorr-str (selectregions-regionscorr sregsx)) (rate-str (selectregions-rate sregsx)))
)

;;; Return true if two selectregions are equal.
(defun selectregions-eq (sregs1 sregs2) ; -> bool
  ;(format t "~&selectregions-eq: ~A ? ~A" sregs1 sregs2)
  (assert (selectregions-p sregs1))
  (assert (selectregions-p sregs2))

  (and (regionscorr-eq (selectregions-regionscorr sregs1) (selectregions-regionscorr sregs1))
       (rate-eq (selectregions-rate sregs1) (selectregions-rate sregs2)))
)

;;; Return true if two selectregionss are not equal.
(defun selectregions-ne (sregs1 sregs2) ; -> bool
  (assert (selectregions-p sregs1))
  (assert (selectregions-p sregs2))

  (not (selectregions-eq sregs1 sregs2))
)

;;; Return true if two selectregions are equal in regions, not considering rates.
(defun selectregions-eq-regions (sregs1 sregs2) ; -> bool
  ;(format t "~&selectregions-eq: ~A ? ~A" sregs1 sregs2)
  (assert (selectregions-p sregs1))
  (assert (selectregions-p sregs2))

  (regionscorr-eq (selectregions-regionscorr sregs1) (selectregions-regionscorr sregs1))
)

;;; Return true if a list is a list of selectregionss.
;;; An empty list will return true.
(defun selectregions-list-p (sregslst) ; -> bool
  ;(format t "~&selectregions-list-p: ~A" sregslst)
  (if (not (listp sregslst))
    (return-from selectregions-list-p false))

  (let (last-item)
    (loop for sregsx in sregslst do
      (if (not (selectregions-p sregsx))
        (return-from selectregions-list-p false))

      (if last-item
	(if (not (regionscorr-congruent (selectregions-regionscorr sregsx) (selectregions-regionscorr last-item)))
	  (return-from selectregions-list-p false))
        (setf last-item sregsx)
      )
    )
  )
  true
)

;;; Return the number of regions in seleectregions-regionscorr.
(defun selectregions-length (sregsx) ; -> a number gt 0.
  (assert (selectregions-p sregsx))

  (regionscorr-length (selectregions-regionscorr sregsx))
)

;;; Translate a list of symbols into a selectregions instance.
;;; Like (SR (RC (r1010)) (RT 0 2-1))
(defun selectregions-from (symbols) ; -> selectregions
   ;(format t "~&selectregions-from ~A" (type-of symbols))
   (assert (listp symbols))
   (assert (not (null symbols)))
   (assert (symbolp (car symbols)))
   (assert (eq (car symbols) 'SR))     

   (setf symbols (cdr symbols))

    ;(format t "~&selectregions-from2 ~A ~A" (type-of symbols) symbols)

    (let (rc rate)
        (if (/= 2 (length symbols))
            (error "The token list should have two parts"))

        (setf rc (car symbols))
        ;(format t "~&selectregions-from3 rc: ~A ~A" (type-of rc) rc)
        (setf rc (regionscorr-from rc))
            
        (setf rate (rate-from (second symbols)))

        ;(format t "~&selectregions-from4 rate: ~A ~A" (type-of rate) (rate-str rate))

        (selectregions-new rc rate)
    )
)

;;; Return net value.
(defun selectregions-net-value (sregsx) ; -> integer
  (rate-net-value (selectregions-rate sregsx))
)

