;;; Run planscorr tests.
(defun planscorr-tests ()

  (format t "~&planscorr-tests beginning")

  ;; Test planscorr-new.
  (let (plncorr1)
    ; Test new, empty, planscorr.
    (setf plncorr1 (planscorr-new nil))
    (assert (planscorr-p plncorr1))

    (format t "~&  planscorr-new OK")
  )

  ;; Test planscorr-length.
  (let (plnsc1 plan1 plan2 step1 step2)
    (setf step1 (step-new :act-id 0 :rule (rule-from "[00/XX/01/Xx]")))
    (setf step2 (step-new :act-id 0 :rule (rule-from "[01/XX/11/XX]")))
    (setf plan1 (plan-new (list step1 step2)))

    (setf step1 (step-new :act-id 0 :rule (rule-from "[01/Xx]")))
    (setf step2 (step-new :act-id 0 :rule (rule-from "[11/XX]")))
    (setf plan2 (plan-new (list step1 step2)))

    (setf plnsc1 (planscorr-new (list plan1 plan2)))
    (assert (= (planscorr-length plnsc1) 2))

    (format t "~&  planscorr-length OK")
  )

  ;; Test planscorr-congruent.
  (let (plnsc1 plnsc2 step1 plan1 plan2)
    (setf step1 (step-new :act-id 0 :rule (rule-from "[00/XX/01/Xx]")))
    (setf plan1 (plan-new (list step1)))

    (setf step1 (step-new :act-id 0 :rule (rule-from "[01/Xx]")))
    (setf plan2 (plan-new (list step1)))

    (setf plnsc1 (planscorr-new (list plan1 plan2)))

    (setf step1 (step-new :act-id 0 :rule (rule-from "[00/XX/01/Xx]")))
    (setf plan1 (plan-new (list step1)))

    (setf step1 (step-new :act-id 0 :rule (rule-from "[00/X1]")))
    (setf plan2 (plan-new (list step1)))

    (setf plnsc2 (planscorr-new (list plan1 plan2)))

    (assert (planscorr-congruent plnsc1 plnsc2))

    (format t "~&  planscorr-congruent OK")
  )

  ;; Test planscorr-are-sequence.
  (let (plnsc1 plnsc2 step1 plan1)
    (setf step1 (step-new :act-id 0 :rule (rule-from "[00/11/01/XX]")))
    (setf plan1 (plan-new (list step1)))

    (setf plnsc1 (planscorr-new (list plan1)))
    ;(format t "~&plnsc1 ~A" plnsc1)

    (setf step1 (step-new :act-id 0 :rule (rule-from "[01/11/11/XX]")))
    (setf plan1 (plan-new (list step1)))

    (setf plnsc2 (planscorr-new (list plan1)))
    ;(format t "~&plnsc2 ~A" plnsc2)

    (assert (planscorr-are-sequence plnsc1 plnsc2))
    (assert (not (planscorr-are-sequence plnsc2 plnsc1)))

    (format t "~&  planscorr-are-sequence OK")
  )

  ;; Test planscorr-can-be-linked.
  (let (plnsc1 plnsc2 step1 plan1)
    (setf step1 (step-new :act-id 0 :rule (rule-from "[00/00/01/XX]")))
    (setf plan1 (plan-new (list step1)))

    (setf plnsc1 (planscorr-new (list plan1)))
    ;(format t "~&plnsc1 ~A" plnsc1)

    (setf step1 (step-new :act-id 0 :rule (rule-from "[01/XX/11/00]")))
    (setf plan1 (plan-new (list step1)))

    (setf plnsc2 (planscorr-new (list plan1)))
    ;(format t "~&plnsc2 ~A" plnsc2)

    (assert (planscorr-can-be-linked plnsc1 plnsc2))
    (assert (not (planscorr-can-be-linked plnsc2 plnsc1)))

    (format t "~&  planscorr-can-be-linked OK")
  )

  ;; Test planscorr-link.
  (let (plnsc1 plnsc2 step1 plan1 lnk plnsc1a plnsc2a)
     
    (setf step1 (step-new :act-id 0 :rule (rule-from "[00/00/01/XX]")))
    (setf plan1 (plan-new (list step1)))

    (setf plnsc1 (planscorr-new (list plan1)))
    ;(format t "~&plnsc1 ~A" plnsc1)

    (setf step1 (step-new :act-id 0 :rule (rule-from "[01/XX/11/00]")))
    (setf plan1 (plan-new (list step1)))

    (setf plnsc2 (planscorr-new (list plan1)))
    ;(format t "~&plnsc2 ~A" plnsc2)

    (setf lnk (planscorr-link plnsc1 plnsc2))
    (setf plnsc1a (car lnk))
    (setf plnsc2a (second lnk))
    ;(format t "~&plnsc1a: ~A" plnsc1a)
    ;(format t "~&plnsc2a: ~A" plnsc2a)
    (assert (rule-eq
		  (step-rule (car (plan-step-list (car (planscorr-plan-list plnsc1a)))))
		  (rule-from "[00/00/01/00]")))
    (assert (rule-eq
		  (step-rule (car (plan-step-list (car (planscorr-plan-list plnsc2a)))))
		  (rule-from "[01/00/11/00]")))

    (setf lnk (planscorr-link plnsc2 plnsc1))
    ;(format t "~&lnk ~A" lnk)
    (assert (null lnk))

    (format t "~&  planscorr-link OK")
  )

  (format t "~&planscorr-tests done")
  t
)

