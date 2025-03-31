; Implement a store of regionscorr instances.

(defstruct regionscorrstore
  regionscorrs  ; A list of zero, or more, regionscorr.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (regionscorrstore-<field name> <instance>) -> struct field.
;   (regionscorrstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> regionscorrstore
;   (typep <instance> 'regionscorrstore) -> bool
;
; Probably shouldn't use:
;   (make-regionscorrstore [:<field-name> <field-regionscorrstore>]*), use regionscorrstore-new instead.
;   (copy-regionscorrstore <instance>) copies a regionscorrstore instance.

;;; Return a new regionscorrstore instance, from a list of regions.
(defun regionscorrstore-new (regions) ; -> regionscorrstore.
  ;(format t "~&regions ~A" regions)
  (assert (regionscorr-list-p regions))

  (make-regionscorrstore :regionscorrs regions)
)

;;; Push region into a regionscorrstore.
(defun regionscorrstore-push (storex regx) ; -> nothing, side-effect regionscorrstore is changed.
  (assert (regionscorrstore-p storex))
  (assert (regionscorr-p regx))

  (push regx (regionscorrstore-regionscorrs storex))
)

;;; Add region to the end of a regionscorrstore.
(defun regionscorrstore-add-end (storex regx) ; -> nothing, side-effect regionscorrstore changed.
  (assert (regionscorrstore-p storex))
  (assert (regionscorr-p regx))

  (setf (regionscorrstore-regionscorrs storex) (append (regionscorrstore-regionscorrs storex) (list regx)))
)

;;; Return a regionscorrstore, suppressing subsets.
;;; Preserve region order.
(defun regionscorrstore-push-nosubs (storex regx) ; -> bool, side-effect regionscorrstore is changed.
  ;(format t "~&regionscorrstore-push-nosubs ~A ~A" storex regx)
  (assert (regionscorrstore-p storex))
  (assert (regionscorr-p regx))

  ;; Check for region in store that is a superset (or dup) of the new region.
  (loop for regy in (regionscorrstore-regionscorrs storex) do
    (if (regionscorr-superset-of :sup regy :sub regx)
      (return-from regionscorrstore-push-nosubs false))
  )

  ;; Check for regions that are a subset of the new region.
  (let (del-regs)
    ;; Find regions that are a subset of the new region.
    (loop for regy in (regionscorrstore-regionscorrs storex) do
      (if (regionscorr-superset-of :sup regx :sub regy)
        (push regy del-regs)
      )
    )
    ;; Remove the subset regions.
    (loop for regy in del-regs do
      (setf (regionscorrstore-regionscorrs storex) (remove regy (regionscorrstore-regionscorrs storex) :test #'regionscorr-eq))
    )
  )

  ;; Add the region.
  (regionscorrstore-push storex regx)
  true
)

;;; Return a regionscorrstore with a regionscorr removed.
(defun regionscorrstore-remove (storex rcx) ; -> regionscorrstore
  (assert (regionscorrstore-p storex))
  (assert (regionscorr-p rcx))

  (if (not (regionscorrstore-member storex rcx))
    (format t "~&Problem: regionscorrstore-remove: regionscorr not in the store?"))

  (regionscorrstore-new (remove rcx (regionscorrstore-regionscorrs storex) :test #'regionscorr-eq))
)

;;; Return the number of regions in a regionscorrstore.
(defun regionscorrstore-length (storex) ; -> number.
  (assert (regionscorrstore-p storex))

  (length (regionscorrstore-regionscorrs storex))
)

;;; Return true if a regionscorrstore is empty.
(defun regionscorrstore-is-empty (storex) ; -> bool
  (assert (regionscorrstore-p storex))

  (zerop (regionscorrstore-length storex))
)

;;; Return true if a regionscorrstore is not empty.
(defun regionscorrstore-is-not-empty (storex) ; -> bool
  (assert (regionscorrstore-p storex))

  (plusp (regionscorrstore-length storex))
)

;;; Return a string representing a regionscorrstore.
(defun regionscorrstore-str (storex) ; -> string.
  (assert (regionscorrstore-p storex))

  (let ((ret "#S(REGIONCORRSTORE ") (start t))

    (loop for regx in (regionscorrstore-regionscorrs storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (regionscorr-str regx)))
    )
    (if (zerop (regionscorrstore-length storex))
      (setf ret (concatenate 'string ret "NIL)"))
      (setf ret (concatenate 'string ret ")"))
    )
    ret
  )
)

;;; Return true if a regionscorrstore contains a given region.
(defun regionscorrstore-member (storex regx) ; -> bool
  ;(format t "regionscorrstore-member storex ~A regx ~A" storex regx)
  (assert (regionscorrstore-p storex))
  (assert (regionscorr-p regx))
  (assert (or (regionscorrstore-is-empty storex) (regionscorr-congruent (regionscorrstore-first-regionscorr storex) regx)))

  (if (member regx (regionscorrstore-regionscorrs storex) :test #'regionscorr-eq) true false)
)

;;; Return the first region in a non-empty regionscorrstore.
(defun regionscorrstore-first-regionscorr (storex) ; -> region
  (assert (regionscorrstore-p storex))
  (assert (regionscorrstore-is-not-empty storex))

  (car (regionscorrstore-regionscorrs storex))
)

;;; Return the last region in a non-empty regionscorrstore.
(defun regionscorrstore-last-regionscorr (storex) ; -> region
  (assert (regionscorrstore-p storex))
  (assert (regionscorrstore-is-not-empty storex))

  (car (last (regionscorrstore-regionscorrs storex)))
)

;;; Return the cdr of a non-empty regionscorrstore.
(defun regionscorrstore-cdr (storex) ; -> regionscorrstore.
  (assert (regionscorrstore-p storex))
  (assert (regionscorrstore-is-not-empty storex))

  (make-regionscorrstore :regionscorrs (cdr (regionscorrstore-regionscorrs storex)))
)

;;; Return a regionscorrstore minus a region.
(defun regionscorrstore-subtract-regionscorr (storex regx) ; -> regionscorrstore.
  (assert (regionscorrstore-p storex))
  (assert (regionscorr-p regx))

  (let ((ret (regionscorrstore-new nil))
	tmpstore
       )

    (loop for regy in (regionscorrstore-regionscorrs storex) do
        (cond ((regionscorr-superset-of :sup regx :sub regy) nil)
	      ((regionscorr-intersects regy regx)
	         (setf tmpstore (regionscorr-subtract :min regy :sub regx))
		 (loop for regz in (regionscorrstore-regionscorrs tmpstore) do
		   (regionscorrstore-push-nosubs ret regz)
		 )
	       )
	      (t (regionscorrstore-push-nosubs ret regy))
	)
    )
    ret
  )
)

;;; Return true if there is any region in a regionscorrstore that intersects a given region.
(defun regionscorrstore-any-intersection (storex regx) ; -> bool.
  (assert (regionscorrstore-p storex))
  (assert (regionscorr-p regx))

  (loop for regy in (regionscorrstore-regionscorrs storex) do
    (if (regionscorr-intersects regy regx)
      (return-from regionscorrstore-any-intersection true))
  )
  false 
)

;;; Append two regionscorrstores.
;;; Preserve order.
(defun regionscorrstore-append (store1 store2) ; -> regionscorrstore
  (assert (regionscorrstore-p store1))
  (assert (regionscorrstore-p store2))

  (let ((ret (make-regionscorrstore :regionscorrs (regionscorrstore-regionscorrs store1))))

    ;; Add store2 regions.
    (loop for regx in (regionscorrstore-regionscorrs store2) do
      (regionscorrstore-add-end ret regx)
    )
    ret
  )
)

;;; Return true if a given region intersects regions that another region does not intersect.
(defun regionscorrstore-other-intersections (&key store int-reg not-reg) ; -> bool
  (assert (regionscorrstore-p store))
  (assert (regionscorr-p int-reg))
  (assert (regionscorr-p not-reg))
  (assert (regionscorr-congruent int-reg not-reg))

  (loop for regx in (regionscorrstore-regionscorrs store) do
    (if (and (regionscorr-intersects regx int-reg) (not (regionscorr-intersects regx not-reg)))
      (return-from regionscorrstore-other-intersections true))
  )
  false
)

;;; Find a path of intersecting regions between two given regions.
;;; Return a regionscorrstore of reg1 + series-of-intersecting-regions + reg2.
;;; Failure to find a path returns nil.
;;; The strategy is to keep dividing the problem into two smaller problems.
;;; Later, a path can be calculated from intersection to intersection.
(defun regionscorrstore-find-path (pathscorr-options left-reg right-reg) ; -> pathscorr, or nil.
  ;(format t "~&regionscorrstore-find-path ~A and ~A" left-reg right-reg)
  (assert (regionscorrstore-p pathscorr-options))
  (assert (regionscorr-p left-reg))
  (assert (regionscorr-p right-reg))
  (assert (not (regionscorr-intersects left-reg right-reg)))
  (assert (regionscorr-congruent left-reg right-reg))

  ;; No point without at least one intersection of the left region.
  (when (not (regionscorrstore-any-intersection pathscorr-options left-reg))
    (format t "~&regionscorrstore-find-path: left-reg no intersection")
    (return-from regionscorrstore-find-path nil))

  ;; No point without at least one intersection of the right region.
  (when (not (regionscorrstore-any-intersection pathscorr-options right-reg))
    (format t "~&regionscorrstore-find-path: right-reg no intersection")
    (return-from regionscorrstore-find-path nil))

  ;; Regions should not intersect already.
  (when (regionscorr-intersects left-reg right-reg)
    (format t "~&regionscorrstore-find-path: left-reg intersects right-reg")
    (return-from regionscorrstore-find-path nil))

  ;; Try to find a path between the regions.
  (regionscorrstore-find-path2 pathscorr-options left-reg right-reg)
)
(defun regionscorrstore-find-path2 (pathscorr-options left-reg right-reg) ; -> pathscorr list, or nil. Probably should not call this function directly.
  ;(format t "~&regionscorrstore-find-path2 ~A and ~A" (regionscorr-str left-reg) (regionscorr-str right-reg))
  (assert (regionscorrstore-p pathscorr-options))
  (assert (regionscorr-p left-reg))
  (assert (regionscorr-p right-reg))
  (assert (regionscorr-congruent left-reg right-reg))

  ;; Check for the successful end of a search, or sub-search.
  ;; Look for one region that intersects both regions.
  (let (links ; Store of regions that intersect both given regions.
       )
    (loop for regx in (regionscorrstore-regionscorrs pathscorr-options) do
      (if (and (regionscorr-intersects regx left-reg) (regionscorr-intersects regx right-reg))
        (push regx links)
      )
    )
    (when links
      ;(format t "~&regionscorrstore-find-path2: links found. return 1")
      (return-from regionscorrstore-find-path2 (pathscorr-new (list left-reg (nth (random (length links)) links) right-reg)))
    )
  )

  ;; Look for regions that do not intersect either region.
  ;; These split the search into two smaller searches.
  (let (middle-region	; Region between the two given regions.
        links		; Store of regions between the two given regions.
        left-path	; Path from first given region to the middle-region.
        right-path	; Path from middle-region to the second region.
        (glide-path (regionscorr-union left-reg right-reg)) ; Region containing straight-forward paths between regions.
       )

    ;; Gather non-intersecting regions roughly between the two given regions.
    (loop for regx in (regionscorrstore-regionscorrs pathscorr-options) do
      (if (and (not (regionscorr-intersects regx left-reg)) (not (regionscorr-intersects regx right-reg))
	       (regionscorr-intersects regx glide-path))
	    (push regx links)
      )
    )
    (when links
      ;; Choose a region to split the problem in two.
      (setf middle-region (nth (random (length links)) links))
      ;(format t "~&regionscorrstore-find-path2: middle-region ~A" (regionscorr-str middle-region))
 
      ;(format t "~&regionscorrstore-find-path2: try left-middle")
      (setf left-path (regionscorrstore-find-path2
            (regionscorrstore-remove pathscorr-options middle-region) left-reg middle-region))

      (when (null left-path)
        ;(format t "~&regionscorrstore-find-path2: return 2")
        (return-from regionscorrstore-find-path2 nil))

      ;(format t "~&regionscorrstore-find-path2: try middle-right")
      (setf right-path (regionscorrstore-find-path2 
         (regionscorrstore-remove pathscorr-options middle-region) middle-region right-reg))

      (when (null right-path)
        ;(format t "~&regionscorrstore-find-path2: return 3")
        (return-from regionscorrstore-find-path2 nil))

      (when (regionscorr-eq (regionscorrstore-last-regionscorr left-path) (regionscorrstore-first-regionscorr right-path))
        ;(format t "~&regionscorrstore-find-path2: return 4")
        (return-from regionscorrstore-find-path2 (pathscorr-append left-path (regionscorrstore-cdr right-path))))

      ;(format t "~&regionscorrstore-find-path2: return 5")
      (return-from regionscorrstore-find-path2 (pathscorr-append left-path right-path))
    )
  )

  ;; Look for regions that intersect the left, or right, region.
  (let (next-region	; A region that intersects one of the given regions.
        links		; Store of next-regions.
        left-path	; Path from left region to the next-region.
        right-path	; Path from next-region to the right region.
       )

    (loop for regx in (regionscorrstore-regionscorrs pathscorr-options) do

      ;; Find regions that intersect the left region, and at least one other region.
      (when (and (regionscorr-ne regx left-reg) (regionscorr-intersects regx left-reg))
        ;; Check if the region intersects any other region, that left-reg does not intersect.
        (if (regionscorrstore-other-intersections :store pathscorr-options :int-reg regx :not-reg left-reg)
         (push regx links)
        )
      )
      ;; Find regions that intersect the right region, and at least one other region.
      (when (and (regionscorr-ne regx right-reg) (regionscorr-intersects regx right-reg))
        ;; Check if the region intersects any other region, that left-reg does not intersect.
        (if (regionscorrstore-other-intersections :store pathscorr-options :int-reg regx :not-reg right-reg)
          (push regx links)
        )	
      )
    )
    (when links
      ;; Choose a region to continue with.
      (setf next-region (nth (random (length links)) links))

      ;; Process a region that intersects the left region.
      (when (regionscorr-intersects next-region left-reg)
        ;(format t "~&regionscorrstore-find-path2: try next-right")
        (setf right-path (regionscorrstore-find-path2 pathscorr-options next-region right-reg))
        (when (pathscorr-is-empty right-path)
          ;(format t "~&regionscorrstore-find-path2: return 6")
          (return-from regionscorrstore-find-path2 right-path))

        (pathscorr-add-start right-path left-reg)
        ;(format t "~&regionscorrstore-find-path2: return 7")
        (return-from regionscorrstore-find-path2 right-path)
      )
      ;; Process a region that intersects the right region.
      (when (regionscorr-intersects next-region right-reg)
        ;(format t "~&regionscorrstore-find-path2: try left-next")
        (setf left-path (regionscorrstore-find-path2 pathscorr-options left-reg next-region))
        (when (pathscorr-is-empty left-path)
          ;(format t "~&regionscorrstore-find-path2: return 8")
          (return-from regionscorrstore-find-path2 left-path))

        ;(format t "~&regionscorrstore-find-path2: return 9")
        (pathscorr-add-end left-path right-reg)
        (return-from regionscorrstore-find-path2 left-path)
      )
    )
  ) 
  ;; Default return.
  (format t "~&regionscorrstore-find-path2: return 10")
  nil
)

;;; Return a regionscorrstore split by intersections.
;;; Every fragment will be a subset of any original regionscorr it intersects.
(defun regionscorrstore-split-by-intersections (store1) ; -> regionscorrstore
  (assert (regionscorrstore-p store1))

  (let ((store2 (regionscorrstore-new nil)))

    ;; Remove dups, if any.
    (loop for regscorrx in (regionscorrstore-regionscorrs store1) do
      (if (not (regionscorrstore-member store2 regscorrx))
	    (regionscorrstore-push store2 regscorrx)
      )
    )

    (if (< (regionscorrstore-length store2) 2)
      (return-from regionscorrstore-split-by-intersections store2))

    (let (ints-not-found (store3 (regionscorrstore-new nil)) tmpstore (any-change true) store4 (ret (regionscorrstore-new nil)))

      (while any-change
        (setf any-change false)

        (loop for regscorrx in (regionscorrstore-regionscorrs store2) do
          (setf ints-not-found true)
  
	      (setf tmpstore (regionscorrstore-new (list regscorrx)))

          (loop for regscorry in (regionscorrstore-regionscorrs store2) do
    
            (if (regionscorr-ne regscorrx regscorry)
  
              (when (regionscorrstore-any-intersection tmpstore regscorry)
  
                (setf ints-not-found false)
                (setf any-change true)
  
                (setf tmpstore (regionscorrstore-subtract-regionscorr tmpstore regscorry))
              )
     	   )
          ) ; next regscorry

          (loop for regscorrz in (regionscorrstore-regionscorrs tmpstore) do
            (regionscorrstore-push-nosubs store3 regscorrz)
          )
        ) ; next regscorrx

	    (setf store4 (regionscorrstore-subtract :min-store store2 :sub-store store3))

        (when any-change
 	      (setf store2 store4)
	      (setf ret (regionscorrstore-append ret store3))
 	      (setf store3 (regionscorrstore-new nil))
 	    )
      ) ; end while

      (setf ret (regionscorrstore-append ret store3)) ; pick up last regions, with no intersections.
      ;; Return results.
      ret
    )
  )
)

;;; Return a regionscorrstore minus another.
(defun regionscorrstore-subtract (&key min-store sub-store) ; -> regionscorrstore.
  (assert (regionscorrstore-p min-store))
  (assert (regionscorrstore-p sub-store))

  (let ((ret min-store))
    (loop for regscorrx in (regionscorrstore-regionscorrs sub-store) do
        (if (regionscorrstore-any-intersection ret regscorrx)
	  (setf ret (regionscorrstore-subtract-regionscorr ret regscorrx))
	)
    )
    ret
  )
)

