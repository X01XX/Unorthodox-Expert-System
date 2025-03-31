; Script for compiling lisp files to find problems.
(load #p "main.lisp")
(progn (format t "~& ") (compile-file "value.lisp"))
(progn (format t "~& ") (compile-file "mask.lisp"))
(progn (format t "~& ") (compile-file "state.lisp"))
(progn (format t "~& ") (compile-file "region.lisp"))
(progn (format t "~& ") (compile-file "regionstore.lisp"))
(progn (format t "~& ") (compile-file "sample.lisp"))
(progn (format t "~& ") (compile-file "rule.lisp"))
(progn (format t "~& ") (compile-file "statestore.lisp"))
(progn (format t "~& ") (compile-file "main.lisp"))
(progn (format t "~& ") (compile-file "err.lisp"))
(progn (format t "~& ") (compile-file "rulestore.lisp"))
(progn (format t "~& ") (compile-file "group.lisp"))
(progn (format t "~& ") (compile-file "groupstore.lisp"))
(progn (format t "~& ") (compile-file "step.lisp"))
(progn (format t "~& ") (compile-file "stepstore.lisp"))
(progn (format t "~& ") (compile-file "change.lisp"))
;(progn (format t "~& ") (compile-file "cngstps.lisp"))
;(progn (format t "~& ") (compile-file "cngstpsstore.lisp"))
(progn (format t "~& ") (compile-file "regionscorr.lisp"))
(progn (format t "~& ") (compile-file "pathscorr.lisp"))
(progn (format t "~& ") (compile-file "regionscorrstore.lisp"))
(progn (format t "~& ") (compile-file "domain.lisp"))
(progn (format t "~& ") (compile-file "selectregions.lisp"))
(progn (format t "~& ") (compile-file "selectregionsstore.lisp"))
(progn (format t "~& ") (compile-file "domainstore.lisp"))
(progn (format t "~& ") (compile-file "plan.lisp"))
(progn (format t "~& ") (compile-file "planstore.lisp"))
(progn (format t "~& ") (compile-file "pn.lisp"))
(progn (format t "~& ") (compile-file "square.lisp"))
(progn (format t "~& ") (compile-file "maskscorr.lisp"))
(progn (format t "~& ") (compile-file "maskstore.lisp"))
;(progn (format t "~& ") (compile-file "rulescorr.lisp"))
(progn (format t "~& ") (compile-file "planscorr.lisp"))
(progn (format t "~& ") (compile-file "planscorrstore.lisp"))
(progn (format t "~& ") (compile-file "action.lisp"))
(progn (format t "~& ") (compile-file "actionstore.lisp"))
(progn (format t "~& ") (compile-file "anyxofn.lisp"))
(progn (format t "~& ") (compile-file "need.lisp"))
(progn (format t "~& ") (compile-file "needstore.lisp"))
(progn (format t "~& ") (compile-file "sessiondata.lisp"))
(progn (format t "~& ") (compile-file "statescorr.lisp"))
(progn (format t "~& ") (compile-file "squarestore.lisp"))
(progn (format t "~& ") (compile-file "rate.lisp"))


(progn (format t "~& ") (compile-file "value_t.lisp"))
(progn (format t "~& ") (compile-file "mask_t.lisp"))
(progn (format t "~& ") (compile-file "maskstore_t.lisp"))
(progn (format t "~& ") (compile-file "state_t.lisp"))
(progn (format t "~& ") (compile-file "region_t.lisp"))
(progn (format t "~& ") (compile-file "regionstore_t.lisp"))
(progn (format t "~& ") (compile-file "sample_t.lisp"))
(progn (format t "~& ") (compile-file "rule_t.lisp"))
(progn (format t "~& ") (compile-file "statestore_t.lisp"))
(progn (format t "~& ") (compile-file "rulestore_t.lisp"))
(progn (format t "~& ") (compile-file "group_t.lisp"))
(progn (format t "~& ") (compile-file "groupstore_t.lisp"))
(progn (format t "~& ") (compile-file "step_t.lisp"))
(progn (format t "~& ") (compile-file "stepstore_t.lisp"))
(progn (format t "~& ") (compile-file "change_t.lisp"))
;(progn (format t "~& ") (compile-file "cngstps_t.lisp"))
;(progn (format t "~& ") (compile-file "cngstpsstore_t.lisp"))
(progn (format t "~& ") (compile-file "regionscorr_t.lisp"))
(progn (format t "~& ") (compile-file "pathscorr_t.lisp"))
(progn (format t "~& ") (compile-file "regionscorrstore_t.lisp"))
(progn (format t "~& ") (compile-file "domain_t.lisp"))
(progn (format t "~& ") (compile-file "selectregions_t.lisp"))
(progn (format t "~& ") (compile-file "selectregionsstore_t.lisp"))
(progn (format t "~& ") (compile-file "domainstore_t.lisp"))
(progn (format t "~& ") (compile-file "plan_t.lisp"))
(progn (format t "~& ") (compile-file "planstore_t.lisp"))
(progn (format t "~& ") (compile-file "square_t.lisp"))
(progn (format t "~& ") (compile-file "maskscorr_t.lisp"))
;(progn (format t "~& ") (compile-file "rulescorr_t.lisp"))
(progn (format t "~& ") (compile-file "planscorr_t.lisp"))
(progn (format t "~& ") (compile-file "planscorrstore_t.lisp"))
(progn (format t "~& ") (compile-file "action_t.lisp"))
(progn (format t "~& ") (compile-file "actionstore_t.lisp"))
(progn (format t "~& ") (compile-file "need_t.lisp"))
(progn (format t "~& ") (compile-file "needstore_t.lisp"))
