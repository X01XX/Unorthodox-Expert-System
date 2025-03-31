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

;;; Return plans to go from one regionscorr to another.
(defun domainstore-get-plans (storex from-regs to-regs pathx) ; -> planscorrstore
  (assert (domainstore-p storex))
  (assert (regionscorr-p from-regs))
  (assert (regionscorr-p to-regs))
  (assert (pathscorr-p pathx))
  (assert (domainstore-congruent storex from-regs))
  (assert (domainstore-congruent storex to-regs))
  (assert (not (regionscorr-intersects from-regs to-regs)))
  ;(format t "~&domainstore-get-plans: from ~A to ~A within ~A" (regionscorr-str from-regs) (regionscorr-str to-regs) (pathscorr-str pathx))

  (let (last-int cur-regs next-regs next-int planx (ret (planscorrstore-new nil))) 

    (setf last-int (car (pathscorr-regionscorr-list pathx)))

    (loop for inx from 0 below (- (pathscorr-length pathx) 2) do

      (setf cur-regs  (nth (+ 1 inx) (pathscorr-regionscorr-list pathx)))
      (setf next-regs (nth (+ 2 inx) (pathscorr-regionscorr-list pathx)))

      (setf next-int (regionscorr-intersection cur-regs next-regs))

      (setf planx (domainstore-get-plan storex last-int next-int cur-regs))
      (if planx
        (planscorrstore-add-end ret planx)
        (return-from domainstore-get-plans nil)
      )
      (setf last-int (planscorr-result-regions planx)) ; s/b subset of next-int.
   )
   ret
  )
)

;;; Return plans to go from one region to another, within a given region.
(defun domainstore-get-plan (storex from-regs to-regs within) ; -> planscorr, or nil.
  ;(format t "~&domainstore-get-plan: from ~A to ~A within ~A" (regionscorr-str from-regs) (regionscorr-str to-regs) (regionscorr-str within))
  (assert (domainstore-p storex))
  (assert (regionscorr-p from-regs))
  (assert (regionscorr-p to-regs))
  (assert (regionscorr-p within))
  (assert (domainstore-congruent storex from-regs))
  (assert (domainstore-congruent storex to-regs))
  (assert (not (regionscorr-intersects from-regs to-regs)))
  
  ;(if (not (regionscorr-superset-of :sup within :sub from-regs))
  ;  (format t "~&domainstore-get-plan: within ~A not superset from regs ~A" (regionscorr-str within) (regionscorr-str from-regs))
  ;)
  (assert (regionscorr-superset-of :sup within :sub from-regs))

  ;(if (not (regionscorr-superset-of :sup within :sub to-regs))
  ;  (format t "~&domainstore-get-plan: within ~A not superset from regs ~A" (regionscorr-str within) (regionscorr-str to-regs))
  ;)
  (assert (regionscorr-superset-of :sup within :sub to-regs))

  (let ((ret-store (planscorr-new nil)) domx-plan)

    (loop for domx in (domainstore-domains storex)
          for from-regx in (regionscorr-region-list from-regs)
          for to-regx   in (regionscorr-region-list to-regs)
          for with-regx in (regionscorr-region-list within) do

	  (assert (region-superset-of :sub from-regx :sup with-regx))
	  (assert (region-superset-of :sub to-regx :sup with-regx))

      (setf domx-plan (domain-get-plan domx from-regx to-regx with-regx 5))

      (if (null domx-plan)
       (return-from domainstore-get-plan nil))

      (planscorr-add-end ret-store domx-plan)
    )

    ;(format t "~&domainstore-get-plan: returning ~A" (planscorr-str ret-store))
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
    ) ; next nedx

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

  (if (planscorrstore-p (need-plan nedx))
    (let (smpl domx)
      (domainstore-run-plans dmxs (need-plan nedx))
      (setf domx (domainstore-nth dmxs (need-dom-id nedx)))
      (when (or  ; If target is a regionscorr, no additional action is taken.
           (and (state-p (need-target nedx)) (state-eq (domain-current-state domx) (need-target nedx)))
           (and (region-p (need-target nedx)) (region-superset-of-state (need-target nedx) (domain-current-state domx)))
          )   
          (setf smpl (action-take-sample-for-need (actionstore-nth (domain-actions domx) (need-act-id nedx)) (domain-current-state domx) nedx))
          (setf (domain-current-state domx) (sample-result smpl))
      )
    )
    (let ((dom-id (need-dom-id nedx)))
      (domain-process-need (domainstore-nth dmxs dom-id) nedx)
    )
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

;;; Set the domain states.
(defun domainstore-set-states (storex stacrx) ; side-effect, domains changed.
  (assert (domainstore-p storex))
  (assert (statescorr-p stacrx))
  (assert (statescorr-congruent stacrx (domainstore-all-current-states storex)))

  (loop for domx in (domainstore-domains storex) 
        for stax in (statescorr-state-list stacrx) do
   
    (domain-set-state domx stax)
  )
)

;;; Run a planscorrstore struct.
;;; Ruturn nil as soon as there is an unexpected result.
;;; Otherwise return true.
(defun domainstore-run-plans (storex plans) ; bool, side-effect, domain current-states changed.
  (assert (domainstore-p storex))
  (assert (planscorrstore-p plans))

  (let (rslt)
    (loop for pcx in (planscorrstore-planscorrs plans) do

      (loop for plnx in (planscorr-plan-list pcx)
            for domx in (domainstore-domains storex) do

        (setf rslt (domain-run-plan domx plnx))
        (if (null rslt)
          (return-from domainstore-run-plans false))
      )
    )
    true
  )
)

