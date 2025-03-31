;;;; The Unorthodox-Expert-System, in LISP.
;;;;
;;;; The obvious question is: What is it an expert of?
;;;;
;;;; It is an expert of its own state.
;;;;
;;;; For use outside of the GPL-3.0 license, contact the Wisconsin Alumni Research Foundation (WARF).
;;;;

(load #p "err.lisp")
(load #p "tools.lisp")

(load #p "value.lisp")
(load #p "value_t.lisp")

(load #p "state.lisp")
(load #p "state_t.lisp")
(load #p "statestore.lisp")
(load #p "statestore_t.lisp")

(load #p "mask.lisp")
(load #p "mask_t.lisp")
(load #p "maskstore.lisp")
(load #p "maskstore_t.lisp")
(load #p "maskscorr.lisp")
(load #p "maskscorr_t.lisp")

(load #p "region.lisp")
(load #p "region_t.lisp")
(load #p "regionstore.lisp")
(load #p "regionstore_t.lisp")

(load #p "sample.lisp")
(load #p "sample_t.lisp")

(load #p "rule.lisp")
(load #p "rule_t.lisp")

(load #p "rulestore.lisp")
(load #p "rulestore_t.lisp")

(load #p "group.lisp")
(load #p "group_t.lisp")
(load #p "groupstore.lisp")
(load #p "groupstore_t.lisp")

(load #p "action.lisp")
(load #p "action_t.lisp")
(load #p "actionstore.lisp")
(load #p "actionstore_t.lisp")

(load #p "step.lisp")
(load #p "step_t.lisp")
(load #p "stepstore.lisp")
(load #p "stepstore_t.lisp")

(load #p "change.lisp")
(load #p "change_t.lisp")

(load #p "domain.lisp")
(load #p "domain_t.lisp")

(load #p "anyxofn.lisp")
(load #p "rate.lisp")

(load #p "regionscorr.lisp")
(load #p "regionscorr_t.lisp")

(load #p "pathscorr.lisp")
(load #p "pathscorr_t.lisp")

(load #p "regionscorrstore.lisp")
(load #p "regionscorrstore_t.lisp")

(load #p "domainstore.lisp")
(load #p "domainstore_t.lisp")

(load #p "plan.lisp")
(load #p "plan_t.lisp")

(load #p "planstore.lisp")
(load #p "planstore_t.lisp")

(load #p "selectregions.lisp")
(load #p "selectregions_t.lisp")

(load #p "selectregionsstore.lisp")
(load #p "selectregionsstore_t.lisp")

(load #p "pn.lisp")

(load #p "square.lisp")
(load #p "square_t.lisp")

(load #p "planscorr.lisp")
(load #p "planscorr_t.lisp")

(load #p "planscorrstore.lisp")
(load #p "planscorrstore_t.lisp")

(load #p "need.lisp")
(load #p "need_t.lisp")

(load #p "needstore.lisp")
(load #p "needstore_t.lisp")

(load #p "sessiondata.lisp")
(load #p "statescorr.lisp")
(load #p "squarestore.lisp")




(defun main ()
  (run)
)

;;; (do-interactive-session nil)
(defun default-session ()
  (let (dmxs)
    (setf dmxs (domainstore-new)) ; Init domainstore.
    (domainstore-add-domain (state-from 's0000)) ; Add a domain.
    (domainstore-add-domain (state-from 's00))   ; Add a domain.

    (do-interactive-session dmxs)
  )
)

;;; Do commands against a given sessiondata instance.
(defun do-interactive-session (sessx)
  (assert (sessiondata-p sessx))
  (command-loop sessx)
)

(defun display-needs (sessx)
    ;(format t "~&sessx ~A" (type-of sessx))
    (assert (sessiondata-p sessx))

    (let ((can-do (sessiondata-can-do sessx)) (cant-do (sessiondata-cant-do sessx)))
      (format t "~& ~&Needs that cannot be done:")
      (loop for needx in (needstore-need-list cant-do) do
        (format t "~&   ~A" (need-str needx))
      )
      (format t "~& ~&Needs that can be done:")
      (loop for needx in (needstore-need-list can-do)
            for inx from 0 to (needstore-length can-do) do
        (format t "~&~2,' d ~A" inx (need-str needx))
      )
    )
)

(defun generate-and-display-needs (sessx) ; -> side-effect, sessiondata instance changed.
  ;(format t "~&generate-and-display-needs ~A" (type-of sessx))
  (assert (sessiondata-p sessx))

  (sessiondata-get-needs sessx)
  (display-needs sessx)
)

(defun command-loop (sessx)
  ;(format t "~& ~&command-loop ~A" (type-of sessx))
  ;(format t "~&~A" (sessiondata-str sessx))
  (format t "~& ~&command-loop: Commands:")
  (format t "~& ~&    Nothing, just press Enter - Attempt to satisfy a need that can be done, if any.")
  (format t "~& ~&    q - Quit.")
  (format t "~& ~&    dn <number> - Do Need.")
  (format t "~& ~&    ss <domain-number> <action-number> state - Sample State for a domain and action.")
  (format t "~& ~&    act-sqrs <domain-number> <action-number> - Show squares of a domain and action.")
  (format t "~& ~&    grp-sqrs <domain-number> <action-number> <region> - Show squares of a domain and action.")
  (format t "~& ~&    run - Run cycles until no more needs can be done.")
  (format t "~& ~&    to <regionscorr> - Change position to. Like: to (rc (r1010 r111))")
  (format t "~& ~&    write-session file-path - Write session to a file.")
  (format t "~& ~&    read-session  file-path - Read a session from a file.")

  (assert (sessiondata-p sessx))

  (let (inp tokens token (run 0) just-read-in)

    (loop 
      ;; Update cycle and needs, unless session just read in.
      (if just-read-in
        (setf just-read-in nil)
        (progn 
          (sessiondata-inc-cycle-num sessx)
          (sessiondata-get-needs sessx)
        )
      )

      (format t "~& ~&Cycle: ~D --------------------------------------------" (sessiondata-cycle-num sessx))
      (sessiondata-print sessx)
      (display-needs sessx)

      (setf inp "")
      (when (or (= run 0) (needstore-is-empty (sessiondata-can-do sessx)))
        (setf run 0)
        (format t "~& ~&Press Enter or type a command: ")
        (setf inp (read-line *STANDARD-INPUT*))
      )

      ; Parse tokens from the input string
      (setf tokens nil token nil)
      (loop for char across inp do
        ;(format t "c ~A" char)
        (when (char= char #\ )
            (if (not (null token))
                (push token tokens))
            (setf token nil)
        )
        (when (char/= char #\ )
            (if token
                (setf token (format nil "~A~A" token char))
                (setf token (format nil "~A" char)))
        )
      )

      (when token
        (push token tokens))

        (setf tokens (reverse tokens))

      ;(format t "~&tokens: ~A" tokens)

      ;; Check for Quit.
      (if (string-equal (car tokens) #\q)
        (return-from command-loop))

      ;; Check for run command.
      (if (string-equal (car tokens) "run")
        (setf run 1)
      )

      (if (string-equal (car tokens) "write-session")
        (let (inp create-flag (fname (second tokens)))
          (setf create-flag t)
          (when (open fname :direction :probe)
            (format t "~&File ~A exists, overwrite? yes/no: " fname)
            (setf create-flag (yes-or-no-p))
          )
          (when create-flag
            (with-open-file (stream fname :direction :output)
               (format stream "~S~%" sessx))
            (format t "~&File ~A written" fname)
            (format t "~& ~&Press Enter to continue: ")
            (setf inp (read-line *STANDARD-INPUT*))
          )
        )
      ) 

      (if (string-equal (car tokens) "read-session")
        (let (inp (fname (second tokens)) sessx2)
          (if (open fname :direction :probe)
            (progn
              (with-open-file (stream fname) (setf sessx2 (read stream)))
              (format t "~&File ~A read. Type of input ~A" fname (type-of sessx2))
              (when (typep sessx2 'sessiondata)
                (setf sessx sessx2)
                (setf just-read-in t)
              )
            )
            (format t "~&File ~A not found" fname)
          )
          (format t "~& ~&Press Enter to continue: ")
          (setf inp (read-line *STANDARD-INPUT*))
        )
      )

      ;; Check for to regionscorr
      ;; Like: to (rc (r1010 r111))
      ;; Where domain 0 uses 4 bits and domain 1 uses 3 bits.
      (if (string-equal (car tokens) "to")
        (let (to-regs plans)
          ;(format t "~&tokens: ~A" tokens)
          (setf to-regs (read-from-string (subseq inp 3)))
          ;(format t "~&to-regs: ~A" to-regs)
          (setf to-regs (regionscorr-from to-regs))
          (if to-regs
            (progn
              (if (regionscorr-congruent to-regs (sessiondata-domain-current-regions sessx))
                (progn
                  ;(format t "~&to RegionsCorr ~A" (regionscorr-str to-regs))
                  (if (regionscorr-superset-of-states to-regs (sessiondata-domain-current-states sessx))
                    (format t "~&Current states satisfy the request")
                    (progn
                      (setf plans (sessiondata-get-plans sessx to-regs))
                      (if plans
                        (progn
                          ;(format t "~&plans: ~A" (planscorrstore-str plans))
                          ;(format t "~&TODO run plans")
                          (domainstore-run-plans (sessiondata-domains sessx) plans)
                          (if (regionscorr-superset-of :sup (domainstore-max-regions (sessiondata-domains sessx)) :sub to-regs)
                            (format t "~&Plans worked")
                            (format t "~&Plans failed")
                          )
                        )
                        (format t "~&plans not found")
                      )
                    )
                  )
                )
                (format t "~&The regionscorr definition in the to command is not congruent")
              )
            )
            (format t "~&Could not convert the regionscorr definition in the to command")
          )
          (format t "~& ~&Press Enter to continue: ")
          (setf inp (read-line *STANDARD-INPUT*))
        )
      )

      ;; Check for do need.
      (if (string-equal (car tokens) "dn")
        (let (inx)
          (if (= (length tokens) 2)
           (progn
              (setf inx (read-from-string (second tokens)))
              (if (integerp inx)
                (if (< inx (needstore-length (sessiondata-can-do sessx)))
                  (progn
                    (format t "~&Need chosen: ~A~& " (need-str (needstore-nth (sessiondata-can-do sessx) inx)))
                    (sessiondata-process-need sessx (needstore-nth (sessiondata-can-do sessx) inx))
                  )
                  (format t "~&Invalid need number in dn command")
                )
                (format t "~&Invalid need number in on command")
              )
            )
            (format t "~&Did not understand dn command")
          )
        )
      )

      (if (string-equal (car tokens) "ss")
        (let (dom-id act-id statex)
          (if (= (length tokens) 4)
            (progn
              (setf dom-id (read-from-string (second tokens)))
              (if (and (integerp dom-id) (>= dom-id 0) (< dom-id (sessiondata-num-domains sessx)))
                (progn
                  (setf act-id (read-from-string (third tokens)))
                  (if (and (integerp act-id) (>= act-id 0) (< act-id (sessiondata-num-actions sessx dom-id)))
                    (progn
                      (setf statex (state-from-str (fourth tokens)))
                      (if statex
                         (sessiondata-take-action-need sessx dom-id act-id statex)
                         (format t "~&Did not understand state in ss command")
                      )
                    )
                    (format t "~&Did not understand action id in ss command")
                  )
                )
                (format t "~&Did not understand domain id in ss command")
              )
            )
            (format t "~&Did not understand ss command")
          )
        )
      )

      (if (string-equal (car tokens) "act-sqrs")
        (let (dom-id act-id)
          (if (= (length tokens) 3)
            (progn
              (setf dom-id (read-from-string (second tokens)))
              (if (and (integerp dom-id) (>= dom-id 0) (< dom-id (sessiondata-num-domains sessx)))
                (progn
                  (setf act-id (read-from-string (third tokens)))
                  (if (and (integerp act-id) (>= act-id 0) (< act-id (sessiondata-num-actions sessx dom-id)))
                    (format t "~&squares: ~A" (squarestore-str (action-squares
                                                 (actionstore-nth
                                                   (domain-actions (domainstore-nth (sessiondata-domains sessx) dom-id))
                                                 act-id))))
                    (format t "~&Did not understand action id in act-sqrs command")
                  )
                )
                (format t "~&Did not understand domain id in act-sqrs command")
              )
            )
            (format t "~&Did not understand act-sqrs command")
          )
        )
      )

      (if (string-equal (car tokens) "grp-sqrs")
        (let (dom-id act-id regx grpx sqrx actx)
          (if (= (length tokens) 4)
            (progn
              (setf dom-id (read-from-string (second tokens)))
              (if (and (integerp dom-id) (>= dom-id 0) (< dom-id (sessiondata-num-domains sessx)))
                (progn
                  (setf act-id (read-from-string (third tokens)))
                  (if (and (integerp act-id) (>= act-id 0) (< act-id (sessiondata-num-actions sessx dom-id)))
                    (progn
                      (setf regx (region-from-str (fourth tokens)))
                      (if regx
                        (progn
                          (setf actx (actionstore-nth
                                          (domain-actions (domainstore-nth (sessiondata-domains sessx) dom-id)) act-id))
                          (setf grpx (groupstore-find (action-groups actx) regx))
                          (if grpx
                            (progn
                              (loop for stax in (region-state-list (group-region grpx)) do
                                (setf sqrx (squarestore-find (action-squares actx) stax))
                                (if sqrx
                                   (format t "~&~A" (square-str sqrx))
                                   (format t "~&Square ~A not found?" (state-str sqrx)))
                              )
                            )
                            (format t "~&Group not found in grp-sqrs command"))
                        )
                        (format t "~&Did not understand region in grp-sqrs command"))
                    )
                    (format t "~&Did not understand action id in grp-sqrs command")
                  )
                )
                (format t "~&Did not understand domain id in grp-sqrs command")
              )
            )
            (format t "~&Did not understand grp-sqrs command")
          )
        )
      )

      ;; Force specific domain action state sample, print square and square-count.
      (if (null tokens) ; An unrecognized token will cause this to be skipped. so the effect is to just rerun get-needs.
	    ;; Process needs.
	    (if (needstore-is-not-empty (sessiondata-can-do sessx))
	      (do-any-need sessx)
	    )
      )
      ;; Check previous states.      
      (sessiondata-check-previous-position sessx)
    ) ; end loop
  ) ; end let
) ; end command-loop

(defun do-any-need (sessx) 
  ;(format t "~&do-any-need")
  (assert (sessiondata-p sessx))

  (let (inx nedx (can-do (sessiondata-can-do sessx)))
      (when (needstore-is-not-empty can-do)

        ;; Check for *between-ip*
        (loop for nedy in (needstore-needs can-do)
              while (null nedx) do
          (if (= (need-reason nedy) *between-ip*)
            (setf nedx nedy))
        )

        ;; Check for *expand-group*
        (loop for nedy in (needstore-needs can-do)
              while (null nedx) do
          (if (= (need-reason nedy) *expand-group*)
            (setf nedx nedy))
        )

        ;; Make a random choice.
        (when (null nedx)
          (setf inx (random (needstore-length can-do)))
          (setf nedx (needstore-nth can-do inx))
        )

        (format t "~&Need chosen: ~A~& " (need-str nedx))
        (sessiondata-process-need sessx nedx)
        (return-from do-any-need)
      )
      (format t "~&No needs to do?")
  )
)

;;; Run a new session.
(defun run (&optional fname) 
    (if (null fname) (setf fname "default.kmp"))

    (let ((in (open fname :if-does-not-exist nil)) (str "") sdx sdx-in)
        (when in
            (loop for line = (read-line in nil)
                while line do
                   (setf str (concatenate 'string str line))
                   (setf str (concatenate 'string str (coerce (list #\NewLine) 'string)))
            )
            (close in)
            ;(setf str (remove-comments str))
            ;(format t "~&final: ~A" str)
            (setf sdx-in (read-from-string str)) ; read in data, check that parentheses are balanced.
            (when sdx-in
                ;(pprint sdx-in)
                (setf sdx (eval sdx-in))
 ;              (format t "~&sdx ~A" sdx)
            )
            (do-interactive-session sdx)
        )
    )
)

(defun all-tests ()
  (format t "~&All tests beginning")
  (value-tests)

  (state-tests)
  (statestore-tests)

  (mask-tests)
  (maskstore-tests)

  (rule-tests)
  (rulestore-tests)

  (region-tests)

  (sample-tests)

  (group-tests)
  (groupstore-tests)

  (action-tests)
  (actionstore-tests)

  (step-tests)
  (stepstore-tests)

  (change-tests)

  (regionstore-tests)

  (pathscorr-tests)

  (regionscorr-tests)
  (regionscorrstore-tests)

  (square-tests)

  (domain-tests)
  (domainstore-tests)

  (plan-tests)
  (planstore-tests)

  (selectregions-tests)
  (selectregionsstore-tests)

  (maskscorr-tests)
  (planscorr-tests)
  (planscorrstore-tests)

  (format t "~&All tests done")
  t
)
