    use crate::pn::Pn;
//    use crate::rulestore::RuleStore;
    use crate::truth::Truth;
    use crate::compare::Compare;

    /// Can two structure rules, that implement the Compare trait, be combined?
    ///
    /// This is one of the most tricky, and crucial, functions.
    ///
    /// At program start, you want a few easy wins by combining squares
    /// with only one sample each, even if a few combinations turn out to be invalid.
    ///
    /// Combinations allow plans to be formed to get desired samples,
    /// to improve understanding of the logic.
    ///
    /// Combinations will be invalidated if they produce an unexpected result
    /// when used in a plan.
    ///
    /// No more than the last four samples are considered.
    ///
    /// The pattern number (pn) and the number of samples are
    /// deciding factors.
    ///
    /// With continuing samples, a square might cycle through different
    /// patterns, although that should be rare.
    ///
    /// This returns three possible results: True, False or MoreSamplesNeeded.
    ///
    /// If this function returns True, the next question is: Are there any squares
    /// between them that will invalidate the combination?
    ///
    /// For combinations of (square square), (group group) or (group square).
    pub fn can_combine<T: Compare, U: Compare>(arg1: &T, arg2: &U) -> Truth {
        //println!("arg1.get_pn_ref() {} arg1.get_pnc() {} arg1.get_rules_ref() {} arg2.get_pn_ref() {} arg2.get_pnc() {} arg2.get_rules_ref() {}", arg1.get_pn_ref(), &arg1.get_pnc(), arg1.get_rules_ref(), arg2.get_pn_ref(), &arg2.get_pnc(), arg2.get_rules_ref());
        match arg1.get_pn_ref() {
            Pn::One => {
                match arg2.get_pn_ref() {
                    Pn::One => {
                        // arg1.get_pn_ref() == One, arg2.get_pn_ref() == One
                        // If the rules can be combined, the squares can be combined.
                        if let Some(rulx) = arg1.get_rules_ref().union(&arg2.get_rules_ref()) {
                            if arg1.get_rules_ref().initial_region().union(&arg2.get_rules_ref().initial_region()) == rulx.initial_region() {
                                return Truth::T;
                            }
                        }
                        // else
                        return Truth::F;
                    }

                    Pn::Two => {
                        // arg1.get_pn_ref() == One, arg2.get_pn_ref() == Two
                        if arg1.get_pnc() {
                            return Truth::F;
                        }

                        // If the pn==One, samples==1 square rule is combinable with one of the
                        // pn==Two square rules, more samples are needed.
                        let rulx = arg1.get_rules_ref()[0].union(&arg2.get_rules_ref()[0]);
                        if rulx.is_valid_union() {
                            return Truth::M;
                        }

                        let rulx = arg1.get_rules_ref()[0].union(&arg2.get_rules_ref()[1]);
                        if rulx.is_valid_union() {
                            return Truth::M;
                        }

                        // else
                        return Truth::F;
                    }

                    Pn::Unpredictable => {
                        // arg1.get_pn_ref() == One, arg2.get_pn_ref() == Unpredictable
                        // If the pn==One square is pnc, the squares cannot be combined.
                        if arg1.get_pnc() {
                            return Truth::F;
                        }

                        // The pn==One square needs more samples.
                        return Truth::M;
                    }
                } // end match arg2.get_pn_ref()
            }
            Pn::Two => {
                match arg2.get_pn_ref() {
                    Pn::One => {
                        // arg1.get_pn_ref() == Two, arg2.get_pn_ref() == One
                        // If the pn==One square is has GT 1 sample, the squares cannot be combined.
                        if arg2.get_pnc() {
                            return Truth::F;
                        }

                        // If the pn==one, samles==1, square has one sample, and
                        // its rule is combinable with one of the pn==Two square rules,
                        // more samples are needed.
                        let rulx = arg2.get_rules_ref()[0].union(&arg1.get_rules_ref()[0]);
                        if rulx.is_valid_union()  {
                            return Truth::M;
                        }

                        let rulx = arg2.get_rules_ref()[0].union(&arg1.get_rules_ref()[1]);
                        if rulx.is_valid_union() {
                            return Truth::M;
                        }
                        
                        // else
                        return Truth::F;
                    }
                    Pn::Two => {
                        // arg1.get_pn_ref() == Two, arg2.get_pn_ref() == Two
                        // The pn values match, if the rules can be combined,
                        // the squares can be combined.
                        //println!("union both {} {}", self.state.formatted_string(), other.state.formatted_string());
                        if let Some(rulx) = arg1.get_rules_ref().union(&arg2.get_rules_ref()) {
                            if arg1.get_rules_ref().initial_region().union(&arg2.get_rules_ref().initial_region()) == rulx.initial_region() {
                                if arg1.get_pnc() && arg2.get_pnc() {
                                    return Truth::T;
                                } else {
                                    return Truth::M;
                                }
                            }
                        }
                        // else
                        return Truth::F;
                    }
                    Pn::Unpredictable => {
                        // arg1.get_pn_ref() == Two, other = Unpredictable
                        // If the pn==Two square is not pnc, more samples needed.
                        if arg1.get_pnc() == false {
                            return Truth::M;
                        }
                        // else
                        return Truth::F;
                    }
                } // end match arg2.get_pn_ref()
            }
            Pn::Unpredictable => {
                match arg2.get_pn_ref() {
                    Pn::Unpredictable => {
                        // arg1.get_pn_ref() == Unpredictable, arg2.get_pn_ref() == Unpredictable
                        // The pn values match, no rules exist to be checked,
                        // the squares can be combined.
                        return Truth::T;
                    }
                    _ => {
                        if arg2.get_pnc() {
                            return Truth::F;
                        }

                        // Needs more samples
                        return Truth::M;
                    }
                } // end match arg2.get_pn_ref()
            }
        } // end match arg1.get_pn_ref()
    } // end can_combine
