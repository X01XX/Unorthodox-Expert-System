;;;; Implement a store of regions.




; Implement a store of regions.
(defstruct regionstore
  regions  ; A list of zero, or more, regions.
)
; Automatically created by defstruct:
;
; Most used:
;   (regionstore-<field name> <instance>) -> struct field.
;   (regionstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> regionstore
;   (typep <instance> 'regionstore) -> bool
;
; Probably shouldn't use:
;   (make-regionstore [:<field-name> <field-regionstore>]*), use regionstore-new instead.
;   (copy-regionstore <instance>) copies a regionstore instance.

;;; Return a new regionstore instance, from a list of regions.
(defun regionstore-new (regions) ; -> regionstore.
  ;(format t "~&regions ~A" regions)
  (assert (region-list-p regions))

  (make-regionstore :regions regions)
)

;;; Push region into a regionstore.
(defun regionstore-push (storex regx) ; -> nothing, side-effect regionstore is changed.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (push regx (regionstore-regions storex))
)

;;; Add region to the end of a regionstore.
(defun regionstore-add-end (storex regx) ; -> nothing, side-effect regionstore changed.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (setf (regionstore-regions storex) (append (regionstore-regions storex) (list regx)))
)

;;; Add a region to a regionstore if there are no regions that are superset.
;;; Delete subsets of new region.
(defun regionstore-push-nosubs (storex regx) ; -> bool, true if regionstore is changed.
  ;(format t "~&regionstore-push-nosubs ~A ~A" storex regx)
  (assert (regionstore-p storex))
  (assert (region-p regx))

  ;; Check for region in store that is a superset (or dup) of the new region.
  (loop for regy in (regionstore-regions storex) do
    (if (region-superset-of :sup regy :sub regx)
      (return-from regionstore-push-nosubs false))
  )

  ;; Check for regions that are a subset of the new region.
  (let (del-regs)
    ;; Find regions that are a subset of the new region.
    (loop for regy in (regionstore-regions storex) do
      (if (region-superset-of :sup regx :sub regy)
        (push regy del-regs)
      )
    )
    ;; Remove the subset regions.
    (loop for regy in del-regs do
      (setf (regionstore-regions storex) (remove regy (regionstore-regions storex) :test #'region-eq))
    )
  )

  ;; Add the region.
  (regionstore-add-end storex regx)
  true
)

;;; Return true if any region in a store is a superset (or eq) of a given region.
(defun regionstore-any-superset-of (storex regx) ; -> bool
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (loop for regy in (regionstore-regions storex) do
    (if (region-superset-of :sup regy :sub regx)
      (return-from regionstore-any-superset-of true))
  )
  false
)

;;; Add a region to a regionstore if there are no regions that are subset.
;;; Delete supersets of new region.
(defun regionstore-push-nosups (storex regx) ; -> bool, true if regionstore is changed.
  ;(format t "~&regionstore-push-nosups ~A ~A" storex regx)
  (assert (regionstore-p storex))
  (assert (region-p regx))

  ;; Check if the new region is a superset of any store region.
  (loop for regy in (regionstore-regions storex) do
    (if (region-superset-of :sup regx :sub regy)
      (return-from regionstore-push-nosups false))
  )

  ;; Check for regions that are a superset of the new region.
  (let (del-regs)
    ;; Find regions that are a subset of the new region.
    (loop for regy in (regionstore-regions storex) do
      (if (region-superset-of :sup regy :sub regx)
        (push regy del-regs)
      )
    )
    ;; Remove the superset regions.
    (loop for regy in del-regs do
      (setf (regionstore-regions storex) (remove regy (regionstore-regions storex) :test #'region-eq))
    )
  )

  ;; Add the region.
  (regionstore-add-end storex regx)
  true
)

;;; Return the number of regions in a regionstore.
(defun regionstore-length (storex) ; -> number.
  ;(format t "~&regionstore-length: ~A" storex)
  (assert (regionstore-p storex))

  (length (regionstore-regions storex))
)

;;; Return true if a regionstore is empty.
(defun regionstore-is-empty (storex) ; -> bool
  (assert (regionstore-p storex))

  (zerop (regionstore-length storex))
)

;;; Return true if a regionstore is not empty.
(defun regionstore-is-not-empty (storex) ; -> bool
  (assert (regionstore-p storex))

  (plusp (regionstore-length storex))
)

;;; Return a string representing a regionstore.
(defun regionstore-str (storex) ; -> string, RS[...]
  (assert (regionstore-p storex))

  (concatenate 'string "RS" (regionstore-str2 storex))
)

(defun regionstore-str2 (storex) ; -> string, [...]
  (assert (regionstore-p storex))

  (let ((ret "[") (start t))
    (loop for regx in (regionstore-regions storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (region-str regx)))
    )
    (setf ret (concatenate 'string ret "]"))
    ret
  )
)

;;; Return true if a regionstore contains a given region.
(defun regionstore-member (storex regx) ; -> bool
  ;(format t "regionstore-member storex ~A regx ~A" storex regx)
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (if (member regx (regionstore-regions storex) :test #'region-eq) true false)
)

;;; Return the first region in a non-empty regionstore.
(defun regionstore-first-region (storex) ; -> region
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))

  (car (regionstore-regions storex))
)

;;; Return the last region in a non-empty regionstore.
(defun regionstore-last-region (storex) ; -> region
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))

  (car (last (regionstore-regions storex)))
)

;;; Return the cdr of a non-empty regionstore.
(defun regionstore-cdr (storex) ; -> regionstore.
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))

  (make-regionstore :regions (cdr (regionstore-regions storex)))
)

;;; Return a regionstore minus a region.
(defun regionstore-subtract-region (storex regx) ; -> regionstore.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (let ((ret (regionstore-new nil))
	tmpstore
       )

    (loop for regy in (regionstore-regions storex) do
        (cond ((region-superset-of :sup regx :sub regy) nil)
	      ((region-intersects regy regx)
	         (setf tmpstore (region-subtract :min-reg regy :sub-reg regx))
		     (loop for regz in (regionstore-regions tmpstore) do
		       (regionstore-push-nosubs ret regz)
		     )
	       )
	      (t (regionstore-push-nosubs ret regy))
	)
    )
    ret
  )
)

;;; Return a regionstore minus a state.
(defun regionstore-subtract-state (storex stax) ; -> regionstore.
  (assert (regionstore-p storex))
  (assert (state-p stax))

  (let ((ret (regionstore-new nil)) tmpstore)

    (loop for regy in (regionstore-regions storex) do
      (cond ((region-superset-of-state regy stax)
	         (setf tmpstore (region-subtract-state regy stax))

		     (loop for regz in (regionstore-regions tmpstore) do
		       (regionstore-push-nosubs ret regz)
		     )
	       )
	       (t (regionstore-push-nosubs ret regy))
	  )
    )
    ret
  )
)

;;; Return true if there is any region in a regionstore that intersects a given region.
(defun regionstore-any-intersection (storex regx) ; -> bool.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (loop for regy in (regionstore-regions storex) do
    (if (region-intersects regy regx)
      (return-from regionstore-any-intersection true))
  )
  false 
)

;;; Append two regionstores.
;;; Preserve order.
(defun regionstore-append (store1 store2) ; -> regionstore
  (assert (regionstore-p store1))
  (assert (regionstore-p store2))

  (let ((ret (make-regionstore :regions (regionstore-regions store1))))

    ;; Add store2 regions.
    (loop for regx in (regionstore-regions store2) do
      (regionstore-add-end ret regx)
    )
    ret
  )
)

;;; Return a regionstore instance, given a list of symbols.
;;; Like (), (r1010), or (r101, r1000).
(defun regionstore-from (symbols) ; -> regionstore instance.
  ;(format t "~&regionstore-from: ~A ~A" (type-of symbols) symbols)
  (assert (listp symbols))
  (assert (or (null symbols) (symbolp (car symbols))))

  (let (regions)
    (loop for tokx in symbols do
      ;(format t "~&regionstore-from2 ~A ~A" (type-of tokx) tokx)
      (push (region-from tokx) regions)
    )   
    (regionstore-new (reverse regions))
  )
)

;;; Return the largest intersections of regions within a regionstore.
(defun regionstore-largest-intersections (strx) ; -> regionstore
  (assert (regionstore-p strx))
  ;(format t "~&regionstore-largest-intersections: ~A" strx)

  ;; Check for regions that are a subset of the new region.
  (let ((int-regs (regionstore-new nil)) regx regy)
    ;; Find regions that are a subset of the new region.
    (loop for inx from 0 below (1- (regionstore-length strx)) do
      (setf regx (nth inx (regionstore-regions strx)))

      (loop for iny from (1+ inx) below (regionstore-length strx) do
        (setf regy (nth iny (regionstore-regions strx)))

        (if (region-intersects regx regy)
          (regionstore-push-nosubs int-regs (region-intersection regx regy))
        )
      )
    )
    ;(format t "~&regionstore-largest-intersections: returning ~A" int-regs)
    int-regs
  )
)

;;; Return a regionstore minus another.
(defun regionstore-subtract (&key min-store sub-store) ; -> regionstore.
  ;(format t "~&regionstore-subtract ~A - ~A" min-store sub-store)
  (assert (regionstore-p min-store))
  (assert (regionstore-p sub-store))

  (let ((ret min-store))
    (loop for regx in (regionstore-regions sub-store) do
      (setf ret (regionstore-subtract-region ret regx))
    )
    
    ;(format t "~&regionstore-subtract returning" ret)
    ret
  )
)

;;; Return self split by intersections.
;;; Each fragment returned will be a subset of any original item it intersects.
(defun regionstore-split-by-intersections (strx) ; -> regionstore
  (assert (regionstore-p strx))
  ;(format t "~&regionstore-split-by-intersections ~A" strx)

  (let ((fragments (regionstore-new nil)) (remaining (regionstore-new nil)) intersections intreg)

    ;; Remove duplicates, if any.
    (loop for regx in (regionstore-regions strx) do
        (if (not (regionstore-member remaining regx))
	  (regionstore-push remaining regx))
    )

    (if (< (regionstore-length remaining) 2)
      (return-from regionstore-split-by-intersections remaining))

    (loop while (regionstore-is-not-empty remaining) do
      ;(format t "~&remaining ~A" remaining)

      (setf intersections (regionstore-largest-intersections remaining))
      ;(format t "~& ~&intersections ~A" intersections)

      (setf remaining (regionstore-subtract :min-store remaining :sub-store intersections))
      ;(format t "~&remaining ~A" remaining)

      (setf fragments (regionstore-append fragments remaining))
      ;(format t "~&fragments ~A" fragments)

      ;; Gather remaining regions from the original store.
      (setf remaining (regionstore-new nil))
      (loop for regx in (regionstore-regions strx) do
        (loop for regy in (regionstore-regions intersections) do
	  (when (region-intersects regx regy)
	    (setf intreg (region-intersection regx regy))
            (if (not (regionstore-member remaining intreg))
	      (regionstore-push remaining intreg))
	  )
        )
      )
    )
    ;(format t "~& ~&returning ~A" fragments)
    fragments
  )
)

;;; Return the intersection of two regionstores.
(defun regionstore-intersection (storex storey) ; -> RegionStore.
  (assert (regionstore-p storex))
  (assert (regionstore-p storey))

  (let ((ret (regionstore-new nil)))

    (loop for regx in (regionstore-regions storex) do

      (loop for regy in (regionstore-regions storey) do

        (if (region-intersects regx regy)
          (regionstore-push-nosubs ret (region-intersection regx regy))
        )
      )
    )
    ret
  )
)

;;; Return the union of two regionstores.
(defun regionstore-union (storex storey) ; -> RegionStore.
  (assert (regionstore-p storex))
  (assert (regionstore-p storey))

  (let ((ret (regionstore-new nil)))

    (loop for regx in (regionstore-regions storex) do
      (regionstore-push-nosubs ret regx)
    )

    (loop for regy in (regionstore-regions storey) do
      (regionstore-push-nosubs ret regy)
    )

    ret
  )
)

;;; Return a list of regions that are superset, or equal, to a given region.
(defun regionstore-regions-superset (storex regx) ; -> list of regions.
  (assert (regionstore-p storex))
  (assert (region-p regx))

  (let (ret)
    (loop for regy in (regionstore-regions storex) do
      (if (region-superset-of :sup regy :sub regx)
        (push regy ret))
    )
    ret
  )
)

;;; Return the number of bits used by regions in a non-empty regionstore.
(defun regionstore-num-bits (storex) ; -> number
  (assert (regionstore-p storex))
  (assert (regionstore-is-not-empty storex))

  (region-num-bits (regionstore-first-region storex))
)

