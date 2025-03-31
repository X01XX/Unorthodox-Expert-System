;;;; Implement a series of regions, with bit-number values corresponding to a list of domains.




; Implement a store of corresponding regions.
(defstruct regionscorr
  regionstore  ; A regionstore of zero, or more, regions.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (regionscorr-<field name> <instance>) -> struct field.
;   (regionscorr-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> regionscorr
;   (typep <instance> 'regionscorr) -> bool
;
; Probably shouldn't use:
;   (make-regionscorr [:<field-name> <field-regionscorr>]*), use regionscorr-new instead.
;   (copy-regionscorr <instance>) copies a regionscorr instance.

;;; Return a new regionscorr instance, from a list of regions.
(defun regionscorr-new (regions) ; -> regionscorr, or nil.
  ;(format t "~&regionscorr-new: regions ~A" regions)
  (assert (region-list-p regions))

  (make-regionscorr :regionstore (regionstore-new regions))
)

;;; Return a list of regions from a regionscorr.
(defun regionscorr-region-list (regionscorrx) ; -> list of regions.
  (assert (regionscorr-p regionscorrx))

  (regionstore-regions (regionscorr-regionstore regionscorrx))
)

;;; Add region to the end of a regionscorr.
(defun regionscorr-add-end (regionscorrx regx) ; -> nothing, side-effect regionscorr changed.
  (assert (regionscorr-p regionscorrx))
  (assert (region-p regx))

  (regionstore-add-end (regionscorr-regionstore regionscorrx) regx)
)

;;; Return the number of regions in a regionscorr.
(defun regionscorr-length (regionscorrx) ; -> number.
  ;(format t "~&regionscorr-length: ~A" (type-of regionscorrx))
  (assert (regionscorr-p regionscorrx))

  (regionstore-length (regionscorr-regionstore regionscorrx))
)

;;; Return true if a regionscorr is empty.
(defun regionscorr-is-empty (regionscorrx) ; -> bool
  ;(format t "~&regionscorr-is-empty: arg ~A" (type-of regionscorrx))
  (assert (regionscorr-p regionscorrx))

  (zerop (regionscorr-length regionscorrx))
)

;;; Return true if a regionscorr is not empty.
(defun regionscorr-is-not-empty (regionscorrx) ; -> bool
  (assert (regionscorr-p regionscorrx))

  (plusp (regionscorr-length regionscorrx))
)

;;; Return a string representing a regionscorr.
(defun regionscorr-str (regionscorrx) ; -> string.
  (assert (regionscorr-p regionscorrx))

  (format nil "(RC ~A)" (regionstore-str2 (regionscorr-regionstore regionscorrx)))
)

;;; Return true if a regionscorr contains a given region.
(defun _regionscorr-contains (regionscorrx regx) ; -> bool
  ;(format t "regionscorr-contains regionscorrx ~A regx ~A" regionscorrx regx)
  (assert (regionscorr-p regionscorrx))
  (assert (region-p regx))

  (regionstore-member (regionscorr-regionstore regionscorrx) regx)
)

;;; Return true if two regionscorrs intersect.
(defun regionscorr-intersects (regscorr1 regscorr2) ; -> bool
  (assert (regionscorr-p regscorr1))
  (assert (regionscorr-p regscorr2))
  (assert (regionscorr-congruent regscorr1 regscorr2))

  (loop for reg1 in (regionscorr-region-list regscorr1)
	for reg2 in (regionscorr-region-list regscorr2) do

    (if (not (region-intersects reg1 reg2))
      (return-from regionscorr-intersects false))
  )
  true
)

;;; Return the intersection of two regionscorr, or nil.
(defun regionscorr-intersection (regscorr1 regscorr2) ; -> regionscorr, or nil.
  (assert (regionscorr-p regscorr1))
  (assert (regionscorr-p regscorr2))
  (assert (regionscorr-congruent regscorr1 regscorr2))

  (let ((ret (regionscorr-new nil)) regx)
    (loop for reg1 in (regionscorr-region-list regscorr1)
          for reg2 in (regionscorr-region-list regscorr2) do
      
      (setf regx (region-intersection reg1 reg2))
      (if regx
        (regionscorr-add-end ret regx)
        (return-from regionscorr-intersection nil))
   )
   ret
  )
)

;;; Return the union of two regionscorr.
(defun regionscorr-union (regscorr1 regscorr2) ; -> regionscorr.
  (assert (regionscorr-p regscorr1))
  (assert (regionscorr-p regscorr2))
  (assert (regionscorr-congruent regscorr1 regscorr2))

  (let ((ret (regionscorr-new nil)))
    (loop for reg1 in (regionscorr-region-list regscorr1)
          for reg2 in (regionscorr-region-list regscorr2) do
      
       (regionscorr-add-end ret (region-union reg1 reg2))
    )
    ret
  )
)

;;; Return true if two regionscorr are equal.
(defun regionscorr-eq (regscorr1 regscorr2) ; -> bool
  ;(format t "~&regionscorr-eq: ~A ~A" regscorr1 regscorr2)
  (assert (regionscorr-p regscorr1))
  (assert (regionscorr-p regscorr2))
  (assert (regionscorr-congruent regscorr1 regscorr2))

  (loop for reg1 in (regionscorr-region-list regscorr1)
        for reg2 in (regionscorr-region-list regscorr2) do
    (if (not (region-eq reg1 reg2))
      (return-from regionscorr-eq false))
  )
  true
)

;;; Return true if two regionscorr are not equal.
(defun regionscorr-ne (regscorr1 regscorr2) ; -> bool
  (assert (regionscorr-p regscorr1))
  (assert (regionscorr-p regscorr2))
  (assert (regionscorr-congruent regscorr1 regscorr2))

  (not (regionscorr-eq regscorr1 regscorr2))
)

;;;; Return true if a regionscorr is a superset of another.
(defun regionscorr-superset-of (&key sub sup) ; -> bool
  ;(format t "~&regionscorr-superset-of: sup ~A sub ~A" sup sub) 
  (assert (regionscorr-p sub))
  (assert (regionscorr-p sup))
  (assert (regionscorr-congruent sup sub))

  (loop for reg1 in (regionscorr-region-list sup)
        for reg2 in (regionscorr-region-list sub) do
    (if (not (region-superset-of :sup reg1 :sub reg2))
      (return-from regionscorr-superset-of false))
  )
  true
)

;;;; Return true if a regionscorr is a superset of a statescorr.
(defun regionscorr-superset-of-states (rcx scx) ; -> bool
  ;(format t "~&regionscorr-superset-of-states: ~A ~A" rcx scx) 
  (assert (regionscorr-p rcx))
  (assert (statescorr-p scx))
  (assert (regionscorr-congruent-states rcx scx))

  (loop for regx in (regionscorr-region-list rcx)
        for stax in (statescorr-state-list scx) do
    (if (not (region-superset-of-state regx stax))
      (return-from regionscorr-superset-of-states false))
  )
  true
)

;;; Return a list of regionscorr from subtracting two regionscorr.
(defun regionscorr-subtract (&key min sub) ; -> regionscorrstore.
  ;(format t "~&regionscorr-subtract: ~A ~A" min sub)
  (assert (regionscorr-p min))
  (assert (regionscorr-p sub))
  (assert (regionscorr-congruent min sub))

  (if (not (regionscorr-intersects min sub))
    (return-from regionscorr-subtract (regionscorrstore-new (list min))))

  (if (regionscorr-superset-of :sub min :sup sub)
    (return-from regionscorr-subtract (regionscorrstore-new nil)))

  (let (ret tmp-regs new-regs)

    (loop for regx in (regionscorr-region-list  min)
          for regy in (regionscorr-region-list  sub)
	  for inx from 0 below (regionscorr-length min) do
	     
      ; Subtract two regions.
      (setf tmp-regs (region-subtract :min-reg regx :sub-reg regy))

      ; Produce a new regionscorr for each remainder region.
      (loop for regz in (regionstore-regions tmp-regs) do

	(setf new-regs (regionscorr-new nil))

        (loop for regw in (regionscorr-region-list min)
	      for iny from 0 below (regionscorr-length min) do

	  (if (= inx iny)
	    (regionscorr-add-end new-regs regz)
	    (regionscorr-add-end new-regs regw)
	  )
        )
	;; Save new regionscorr.
	(push new-regs ret)
      )
    )
    (regionscorrstore-new ret)
  )
)

;;; Return true if a list is a list of regionscorr.
;;; An empty list will return true.
(defun regionscorr-list-p (region-list) ; -> bool
  ;; Check arg type.
  (if (not (listp region-list))
    (return-from regionscorr-list-p false))

  (let (first-item)
    (loop for regx in region-list do

      ;; Check item type.
      (if (not (regionscorr-p regx))
        (return-from regionscorr-list-p false))

      ;; Check item characteristics.
      (if first-item
	(if (not (regionscorr-congruent regx first-item))
          (return-from regionscorr-list-p false))
	(setf first-item regx))
    )
    true
  )
)

;;; Return true if two regionscorr have the same length and corresponding region-num-bits values.
(defun regionscorr-congruent (regionscorr1 regionscorr2) ; -> bool
  ;(format t "~&regionscorr-congruent: ~A ~A" regionscorr1 regionscorr2)
  (assert (regionscorr-p regionscorr1))
  (assert (regionscorr-p regionscorr2))

  (if (/= (regionscorr-length regionscorr1) (regionscorr-length regionscorr2))
    (return-from regionscorr-congruent false))

  (loop for reg1 in (regionscorr-region-list regionscorr1)
	for reg2 in (regionscorr-region-list regionscorr2) do

    (if (/= (region-num-bits reg1) (region-num-bits reg2))
      (return-from regionscorr-congruent false))
  )
  true
)

;;; Return true if a regionscorr and statescorr have the same length and corresponding num-bits values.
(defun regionscorr-congruent-states (rcx scx) ; -> bool
  ;(format t "~&regionscorr-congruent-states: ~A ~A" rcx scx)
  (assert (regionscorr-p rcx))
  (assert (statescorr-p scx))

  (if (/= (regionscorr-length rcx) (statescorr-length scx))
    (return-from regionscorr-congruent-states false))

  (loop for regx in (regionscorr-region-list rcx)
        for stax in (statescorr-state-list scx) do

      (if (/= (region-num-bits regx) (state-num-bits stax))
        (return-from regionscorr-congruent-states false))
  )
  true
)

;;; Return an edge mask for a regionscorr.
(defun regionscorr-edge-mask (regionscorr1) ; -> maskscorr
  (assert (regionscorr-p regionscorr1))

  (let (mask-list)
    (loop for regx in (regionscorr-region-list regionscorr1) do
      (setf mask-list (append mask-list (list (region-edge-mask regx))))
    )
    (maskscorr-new mask-list)
  )
)

;;; Return regionscorr X mask.
(defun regionscorr-x-maskscorr (regionscorr1) ; -> maskscorr
  (assert (regionscorr-p regionscorr1))

  (let (mask-list)
    (loop for regx in (regionscorr-region-list regionscorr1) do
      (setf mask-list (append mask-list (list (region-x-mask regx))))
    )
    (maskscorr-new mask-list)
  )
)

;;; Return regionscorr 1 mask.
(defun regionscorr-1-maskscorr (regionscorr1) ; -> maskscorr
  (assert (regionscorr-p regionscorr1))

  (let (mask-list)
    (loop for regx in (regionscorr-region-list regionscorr1) do
      (setf mask-list (append mask-list (list (region-1-mask regx))))
    )
    (maskscorr-new mask-list)
  )
)

;;; Return regionscorr 0 mask.
(defun regionscorr-0-maskscorr (regionscorr1) ; -> maskscorr
  (assert (regionscorr-p regionscorr1))

  (let (mask-list)
    (loop for regx in (regionscorr-region-list regionscorr1) do
      (setf mask-list (append mask-list (list (region-0-mask regx))))
    )
    (maskscorr-new mask-list)
  )
)

;;; Return a regionscorr with selected bits changed to 1.
(defun regionscorr-set-to-ones (regionscorr1 maskscorr1) ; -> regionscorr.
  (assert (regionscorr-p regionscorr1))
  (assert (maskscorr-p maskscorr1))

  (let (reg-list)
    (loop for regx in (regionscorr-region-list regionscorr1)
          for mskx in (maskscorr-mask-list maskscorr1) do
      (setf reg-list (append reg-list (list (region-set-to-ones regx mskx))))
    )
    (regionscorr-new reg-list)
  )
)

;;; Return a regionscorr with selected bits changed to 0.
(defun regionscorr-set-to-zeros (regionscorr1 maskscorr1) ; -> regionscorr.
  (assert (regionscorr-p regionscorr1))
  (assert (maskscorr-p maskscorr1))

  (let (reg-list)
    (loop for regx in (regionscorr-region-list regionscorr1)
          for mskx in (maskscorr-mask-list maskscorr1) do
      (setf reg-list (append reg-list (list (region-set-to-zeros regx mskx))))
    )
    (regionscorr-new reg-list)
  )
)

;;; Return minimun translation of one regionscorr to another.
;;; A 1->X change will default to 1->1.
;;; A 0->X change will default to 0->0.
(defun regionscorr-translate-to (regionscorr1 regionscorr2) ; -> regionscorr
  (assert (regionscorr-p regionscorr1))
  (assert (regionscorr-p regionscorr2))
  (assert (regionscorr-congruent regionscorr1 regionscorr2))

  (let ((to-ones (maskscorr-or
		   (maskscorr-and (regionscorr-0-maskscorr regionscorr1) (regionscorr-1-maskscorr regionscorr2))
		   (maskscorr-and (regionscorr-x-maskscorr regionscorr1) (regionscorr-1-maskscorr regionscorr2))))
        (to-zeros (maskscorr-or
		   (maskscorr-and (regionscorr-1-maskscorr regionscorr1) (regionscorr-0-maskscorr regionscorr2))
		   (maskscorr-and (regionscorr-x-maskscorr regionscorr1) (regionscorr-0-maskscorr regionscorr2))))
	)
    (regionscorr-set-to-zeros (regionscorr-set-to-ones regionscorr1 to-ones) to-zeros)
  )
)


;;; Return a regionscorr instance, given a list of symbols.
;;; Like (RC (()), (RC (1010)), or (RC (101, 1000)).
(defun regionscorr-from (symbols) ; -> regionscorr instance.
    ;(format t "~&regionscorr-from: ~A" (type-of symbols))
    (assert (listp symbols))
    (assert (not (null symbols)))
    (assert (symbolp (car symbols)))
    (assert (eq (car symbols) 'RC))
    ;(format t "~&regionscorr-from: ~A" symbols)

    (make-regionscorr :regionstore (regionstore-from (second symbols)))
)

;;; Return the distance between two regionscorrs.
(defun regionscorr-distance (regscorr1 regscorr2) ; -> integer
  (assert (regionscorr-p regscorr1))
  (assert (regionscorr-p regscorr2))
  (assert (regionscorr-congruent regscorr1 regscorr2))

  (let ((cnt 0))
    (loop for reg1 in (regionscorr-region-list regscorr1)
  	      for reg2 in (regionscorr-region-list regscorr2) do
  
      (setf cnt (+ cnt (region-distance reg1 reg2)))
    )
    cnt
  )
)
