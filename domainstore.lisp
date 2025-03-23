;;;; Implement a store of domains.

(defstruct domainstore
  domains  ; A list of zero, or more, domains with unique id values.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (domainstore-<field name> <instance>) -> struct field.
;   (domainstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> domainstore
;   (typep <instance> 'domainstore) -> bool
;
; Probably shouldn't use:
;   (make-domainstore [:<field-name> <field-domainstore>]*), use domainstore-new instead.
;   (copy-domainstore <instance>) copies a domainstore instance.

;;; Return a new domainstore instance.
;;; Domains cannot have duplicate ids.
(defun domainstore-new () ; -> domainstore.
  ;(format t "~&domainstore-new")

  (make-domainstore
    :domains nil
  )
)

;;; Push a new domain into a domainstore.
;;; Enforce domain-id = index in domain list.
(defun domainstore-add-domain (storex domx) ; -> side effect, domainstore changed.
  ;(format t "~&domainstore add domain ~d" (domainstore-length storex))
  (assert (domainstore-p storex))
  (assert (domain-p domx))

  (domain-set-id domx (domainstore-length storex))

  (setf (domainstore-domains storex) 
     (append (domainstore-domains storex) (list domx)))
)

;;; Return the number of domains in a domainstore.
(defun domainstore-length (storex) ; -> number.
  (assert (domainstore-p storex))

  (length (domainstore-domains storex))
)

;;; Return true if a domainstore is empty.
(defun domainstore-is-empty (storex) ; -> bool
  (assert (domainstore-p storex))

  (zerop (domainstore-length storex))
)

;;; Return true if a domainstore is not empty.
(defun domainstore-is-not-empty (storex) ; -> bool
  (assert (domainstore-p storex))

  (plusp (domainstore-length storex))
)

;;; Return a string representing a domainstore.
(defun domainstore-str (storex) ; -> string.
  (assert (domainstore-p storex))

  (let ((ret "#S(DOMAINSTORE ") (start t))

    (loop for domx in (domainstore-domains storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    
      (setf ret (concatenate 'string ret (domain-str domx)))
    )

    (setf ret (concatenate 'string ret ")"))

    ret
  )
)

;;; Print a domainstore.
(defun domainstore-print (doms) 
  (assert (domainstore-p doms))

  (loop for domx in (domainstore-domains doms) do
    (format t "~& ")
    (domain-print domx)
  )
)

;;; Return a regionscorrstore of the maximum regions of each domain, in order.
(defun domainstore-max-regions (storex) ; -> regionscorr
  (assert (domainstore-p storex))

  (let (regions)
    (loop for domx in (domainstore-domains storex) do
	(push (domain-max-region domx) regions)
    )
    (regionscorr-new (reverse regions))
  )
)

;;; Return true if a domainstore is congruent with a regionstorecorr.
(defun domainstore-congruent (storex regionscorrx) ; -> bool
  (assert (domainstore-p storex))
  (assert (regionscorr-p regionscorrx))

  (loop for domx in (domainstore-domains storex)
        for regx in (regionscorr-region-list regionscorrx) do

    (if (/= (domain-num-bits domx) (region-num-bits regx))
      (return-from domainstore-congruent false))
  )
  true
)

;;; Return plans to go from one region to another.
(defun domainstore-get-plans (storex from-regs to-regs within)
  (assert (domainstore-p storex))
  (assert (regionscorr-p from-regs))
  (assert (regionscorr-p to-regs))
  (assert (selectregionscorrstore-p within))
  (assert (domainstore-congruent storex from-regs))
  (assert (domainstore-congruent storex to-regs))
  (assert (not (regionscorr-intersects from-regs to-regs)))
  (format t "~&domainstore-get-plans from ~A to ~A" (regionscorr-str from-regs) (regionscorr-str to-regs))

  (let (last-regs pathx last-int cur-int) 

    (setf pathx (regionscorrstore-find-path (domainstore-non-negative-regionscorrstore storex) from-regs to-regs))
    (format t "~&pathx ~A" pathx)

    (setf last-regs (car (pathscorr-regionscorr-list pathx)))
    (setf last-int (car (pathscorr-regionscorr-list pathx)))
    (loop for regsx in (cdr (pathscorr-regionscorr-list pathx)) do
      ;(format t "~&last regs ~A regs ~A" last-regs regsx)
      (when (not (regionscorr-superset-of :sup regsx :sub last-regs))
	(setf cur-int (regionscorr-intersection last-regs regsx))
	(format t "~&from ~A to ~A within ~A" last-int cur-int last-regs)
	(setf last-int (regionscorr-translate-to last-int cur-int)) ; todo change from regionscorr-translate-to to get-plan.
      )

      (setf last-regs regsx)
    )
  )
)

;;; Return plans to go from one region to another, within a given region.
(defun domainstore-get-plan (storex from-regs to-regs within) ; -> planstore, or nil.
  ;(format t "~&domainstore-get-plan: from ~A to ~A within ~A" from-regs to-regs within)
  (assert (domainstore-p storex))
  (assert (regionscorr-p from-regs))
  (assert (regionscorr-p to-regs))
  (assert (regionscorr-p within))
  (assert (domainstore-congruent storex from-regs))
  (assert (domainstore-congruent storex to-regs))
  (assert (domainstore-congruent storex within))
  (assert (not (regionscorr-intersects from-regs to-regs)))
  
  (if (not (regionscorr-superset-of :sup within :sub from-regs))
    (format t "~&domainstore-get-plan: within ~A not superset from regs ~A" within from-regs)
    )
  (assert (regionscorr-superset-of :sup within :sub from-regs))

  (if (not (regionscorr-superset-of :sup within :sub to-regs))
    (format t "~&domainstore-get-plan: within ~A not superset from regs ~A" within to-regs)
    )
  (assert (regionscorr-superset-of :sup within :sub to-regs))

  (let ((ret-store (planstore-new nil)) domx-plan from-next)

    (setf from-next (car (regionscorr-region-list from-regs)))

    (loop for domx in (domainstore-domains storex)
          for from-regx in (regionscorr-region-list from-regs)
          for to-regx   in (regionscorr-region-list to-regs)
          for with-regx in (regionscorr-region-list within) do

      (when (not (region-superset-of :sup to-regx :sub from-next))

	(assert (region-superset-of :sup from-regx :sub from-next))

        (setf domx-plan (domain-get-plan domx from-next to-regx with-regx 5))

	(if (null domx-plan)
	  (return-from domainstore-get-plan nil))

        (if (plan-is-not-empty domx-plan)
	  (planstore-add-end-link ret-store domx-plan))
      )
    )
    (format t "~&domainstore-get-plan: returning ~A" ret-store)
    ret-store
  )
)

(defun domainstore-get-needs (dmxs) ; -> (values needs can-do cant-do)
  ;(format t "~&domainstore-get-needs ~A" (type-of dmxs))
  (assert (domainstore-p dmxs))

  (let ((needs (needstore-new nil)) (can-do (needstore-new nil)) (cant-do (needstore-new nil)))
    (loop for domx in (domainstore-domains dmxs) do
      (setf needs (needstore-append needs (domain-get-needs domx)))
    )
    ; Process needs to get can-do, cant-do.
    (loop for nedx in (needstore-needs needs) do
      (if (need-plan nedx)
        (needstore-push can-do nedx)
        (needstore-push cant-do nedx)
     )
    )
    (values needs can-do cant-do)
  )
)

;;; Return a domainstore instance, given a list of symbols.
(defun domainstore-from (symbols) ; -> domainstore instance.
    ;(format t "~&domainstore-from: ~A" (type-of symbols))
    (assert (listp symbols)) 
    (assert (not (null symbols)))
    (assert (symbolp (car symbols)))
    (assert (eq (car symbols) 'DS))

    (setf symbols (second symbols))

    (let (domains ret domx)                                                                                   
        (loop for tokx in symbols do
            ;(format t "~&domainstore-from ~A ~A" (type-of tokx) tokx)
            (setf domx (domain-from tokx))
            (if domx
                (push  domx domains)
                (error (format nil "~&domain-from returned nil from ~A" (list 'quote tokx))))
        ) 
        (setf ret (domainstore-new))
        (loop for domx in (reverse domains) do
            (domainstore-add-domain ret domx)
        )
        ret
    )   
)

;;; Process a need.
(defun domainstore-process-need (dmxs nedx)
  ;(format t "~&domainstore-process-need: ~A ~A" (type-of dmxs) (type-of nedx)) 
  (assert (domainstore-p dmxs))
  (assert (need-p nedx))
  (assert (< (need-dom-id nedx) (domainstore-length dmxs)))

  (let ((dom-id (need-dom-id nedx)))
      (domain-process-need (domainstore-nth dmxs dom-id) nedx)
  )
)

;;; Return the nth element of a DomainStore.
(defun domainstore-nth (storex inx) ; -> domain instance, or nil.
  ;(format t "~&domainstore-nth: ~A ~A" (type-of storex) (type-of inx)) 
  (assert (domainstore-p storex))
  (assert (and (integerp inx) (< inx (domainstore-length storex))))

  (nth inx (domainstore-domains storex))
)

;;; Return all domain current states.
(defun domainstore-all-current-states (storex) ; -> StatesCorr
  (assert (domainstore-p storex))

  (let ((ret (statescorr-new (statestore-new nil))))
    (loop for domx in (domainstore-domains storex) do
      (statescorr-add-end ret (domain-current-state domx))
    )
    ret
  )
)

;;; Return all domain current states as regions.
(defun domainstore-all-current-regions (storex) ; -> RegionsCorr
  (assert (domainstore-p storex))

  (let ((ret (regionscorr-new nil)))
    (loop for domx in (domainstore-domains storex) do
      (regionscorr-add-end ret (region-new (domain-current-state domx)))
    )
    ret
  )
)

;;; Seh the domain states.
(defun domainstore-set-states (storex stacrx) ; side-effect, domains changed.
  (assert (domainstore-p storex))
  (assert (statescorr-p stacrx))
  (assert (statescorr-congruent stacrx (domainstore-all-current-states storex)))

  (loop for domx in (domainstore-domains storex) 
        for stax in (statescorr-state-list stacrx) do
   
    (domain-set-state domx stax)
  )
)

