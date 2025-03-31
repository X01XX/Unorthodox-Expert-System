;;;; Implement a series of regionscorr that intersect.
;;;;
;;;; A pathscorr will be used to plan a path from the from-regionscorr (within a superset regionscorr)
;;;; to the goal-regionscorr (within a superset regionscorr),
;;;; within bridging (intersecting) selectregion-same-negative-rate regionscorrs. 
;;;;
;;;; Hopefully avoiding negative, or at least more negative, selectregions.
;;;;
;;;; --------       ----------
;;;; - from -       - bridge -
;;;; --------       ----------
;;;;       ----------       --------
;;;;       - bridge -       - goal -
;;;;       ----------       --------
;;;;
;;;; So translate the from-regionscorr from intersection to intersection,
;;;; and, finally, to the goal-regionscorr.
;;;;
;;;; The from-regionscorr within a superset regionscorr, and the goal-regionscorr within
;;;; a superset regionscorr, are not bridging intresections, but are intersections.
;;;;
;;;;  A trival pathscorr would be like (1111, 00) - (XXXX, XX) - (0000, 10).
;;;;
;;;; The logic will be:
;;;;
;;;;  Current from-regionscorr in an intersection.
;;;;  While not in last intersection
;;;;    Translate to the next intersection, within the current superset regionscorr.

;;; Implement a store of regions.
(defstruct pathscorr
  regionscorrstore  ; A store of of zero, or more, regionscorr.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (pathscorr-<field name> <instance>) -> struct field.
;   (pathscorr-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> pathscorr
;   (typep <instance> 'pathscorr) -> bool
;
; Probably shouldn't use:
;   (make-pathscorr [:<field-name> <field-pathscorr>]*), use pathscorr-new instead.
;   (copy-pathscorr <instance>) copies a pathscorr instance.

;;; Return a new pathscorr instance, from a regionscorr.
(defun pathscorr-new (regions) ; -> pathscorr, or nil.
  ;(format t "~&pathscorr-new: regions ~A" regions)
  (assert (regionscorr-list-p regions))

  (let (ret)
    (setf ret (make-pathscorr :regionscorrstore (regionscorrstore-new regions)))
    (assert (pathscorr-is-valid ret))
    ret
  )
)

;;; Return a list of regionscorr.
(defun pathscorr-regionscorr-list (pathscorrx) ; -> A list of regionscorr.
  (regionscorrstore-regionscorrs (pathscorr-regionscorrstore pathscorrx))
)

;;; Push region into a pathscorr.
(defun pathscorr-add-start (pathscorrx regx) ; -> nothing, side-effect pathscorr is changed.
  ;(format t "~&pathscorr-add-start: ~A add-start ~A" pathscorrx regx) ; nothing, side effect changed pathscorr.
  (assert (pathscorr-p pathscorrx))
  (assert (regionscorr-p regx))

  (assert (or (pathscorr-is-empty pathscorrx) (regionscorr-intersects regx (pathscorr-first-region pathscorrx))))

  (regionscorrstore-push (pathscorr-regionscorrstore pathscorrx) regx)
)

;;; Add region to the end of a pathscorr.
(defun pathscorr-add-end (pathscorrx regx) ; -> nothing, side-effect pathscorr changed.
  (assert (pathscorr-p pathscorrx))
  (assert (regionscorr-p regx))

  (assert (or (pathscorr-is-empty pathscorrx) (regionscorr-intersects regx (pathscorr-last-region pathscorrx))))

  (regionscorrstore-add-end (pathscorr-regionscorrstore pathscorrx) regx)
)

;;; Return the number of regions in a pathscorr.
(defun pathscorr-length (pathscorrx) ; -> number.
  (assert (pathscorr-p pathscorrx))

  (regionscorrstore-length (pathscorr-regionscorrstore pathscorrx))
)

;;; Return true if a pathscorr is empty.
(defun pathscorr-is-empty (pathscorrx) ; -> bool
  (assert (pathscorr-p pathscorrx))

  (zerop (pathscorr-length pathscorrx))
)

;;; Return true if a pathscorr is not empty.
(defun pathscorr-is-not-empty (pathscorrx) ; -> bool
  (assert (pathscorr-p pathscorrx))

  (plusp (pathscorr-length pathscorrx))
)

;;; Return a string representing a pathscorr.
(defun pathscorr-str (pathscorrx) ; -> string.
  (assert (pathscorr-p pathscorrx))

  (if  (pathscorr-is-empty pathscorrx)
    (return-from pathscorr-str "#S(pathscorr REGIONS NIL)")
  )

  (let ((ret "#S(pathscorr ") (first t))

    (loop for rcx in (pathscorr-regionscorr-list pathscorrx) do
      (if first
        (setf first nil)
        (setf ret (concatenate 'string ret "-"))
      )
      (setf ret (concatenate 'string ret (format nil "~A" (regionscorr-str rcx))))
    )
    (setf ret (concatenate 'string ret ")"))
    ret
  )
)

;;; Return true if a pathscorr contains a given region.
(defun pathscorr-contains (pathscorrx regx) ; -> bool
  ;(format t "pathscorr-contains pathscorrx ~A regx ~A" pathscorrx regx)
  (assert (pathscorr-p pathscorrx))
  (assert (regionscorr-p regx))

  (regionscorrstore-member (pathscorr-regionscorrstore pathscorrx) regx)
)

;;; Return the first regionscorr in a non-empty pathscorr.
(defun pathscorr-first-region (pathscorrx) ; -> regioncorr
  (assert (pathscorr-p pathscorrx))
  (assert (pathscorr-is-not-empty pathscorrx))

  (regionscorrstore-first-regionscorr (pathscorr-regionscorrstore pathscorrx))
)

;;; Return the last regionscorr in a non-empty pathscorr.
(defun pathscorr-last-region (pathscorrx) ; -> region
  (assert (pathscorr-p pathscorrx))
  (assert (pathscorr-is-not-empty pathscorrx))

  (regionscorrstore-last-regionscorr (pathscorr-regionscorrstore pathscorrx))
)

;;; Return the cdr of a non-empty pathscorr.
(defun pathscorr-cdr (pathscorrx) ; -> pathscorr.
  (assert (pathscorr-p pathscorrx))
  (assert (pathscorr-is-not-empty pathscorrx))

  (make-pathscorr :regionscorrstore (regionscorrstore-cdr (pathscorr-regionscorrstore pathscorrx)))
)

;;; Append two pathscorrs.
;;; Preserve order.
(defun pathscorr-append (store1 store2) ; -> pathscorr
  (assert (pathscorr-p store1))
  (assert (pathscorr-p store2))

  (make-pathscorr :regionscorrstore (regionscorrstore-append (pathscorr-regioncorrstore store1)
                                                            (pathscorr-regioncorrstore store2)))
)

;;; Return true if a pathscorr contains a series of intersecting regions.
(defun pathscorr-is-valid (pathscorrx) ; -> bool
  (assert (pathscorr-p pathscorrx))

  (if (< (pathscorr-length pathscorrx) 2)
    (return-from pathscorr-is-valid true))

  (let ((last-reg (pathscorr-first-region pathscorrx)))

    (loop for regx in (cdr (pathscorr-regionscorr-list pathscorrx)) do
      ;; Each two successive regions must intersect.
      (when (not (regionscorr-intersects regx last-reg))
	(format t "~&~A does not intersect ~A" regx last-reg)
	(return-from pathscorr-is-valid false))
 
      ;; Each two successive regions cannot be the same.
      (when (regionscorr-eq regx last-reg)
	(format t "~&~A eq ~A" regx last-reg)
	(return-from pathscorr-is-valid false))

      (setf last-reg regx)
    )
    true
  )
)

