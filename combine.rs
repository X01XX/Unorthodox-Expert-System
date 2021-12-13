    use crate::pn::Pn;
//    use crate::rulestore::RuleStore;
    use crate::truth::Truth;
    use crate::compare::Compare;
    use crate::square::SomeSquare;
//    use crate::region::SomeRegion;
    use crate::regionstore::RegionStore;
    use crate::squarestore::SquareStore;

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

    /// Return Truth enum for the combination of any two squares,
    /// and the squares inbetween them.
    pub fn can_combine_check_between<T: Compare, U: Compare>(arg1: &T, arg2: &U, squares: &SquareStore) -> Truth {

        assert!(arg1.get_region() != arg2.get_region());

        // Check the two squares
        let cmbx = can_combine(arg1, arg2);

        if cmbx == Truth::F {
            return cmbx;
        }

        if cmbx == Truth::T {
            return can_combine_check_between_t(arg1, arg2, squares);
        }

        can_combine_check_between_m(arg1, arg2, squares)
    }

    /// Return Truth enum for the combination of any two squares,
    /// and the squares inbetween them, when the two squares can be combined
    /// alone.
    fn can_combine_check_between_t<T: Compare, U: Compare>(arg1: &T, arg2: &U, squares: &SquareStore) -> Truth {

        let reg1 = arg1.get_region();
        let reg2 = arg2.get_region();

        // Get keys for all squares in the region formed by the
        // two given squares.
        let stas = squares
            .stas_in_reg(&arg1.get_region().union(&arg2.get_region()));

        // Handle Pn::Unpredictable squares
        if *arg1.get_pn_ref() == Pn::Unpredictable {
            // Check each inbetween square
            for stax in stas.iter() {
                if reg1.is_superset_of_state(stax) || reg2.is_superset_of_state(stax) {
                    continue;
                }

                let sqrz = squares.find(stax).unwrap();
                if sqrz.results.pn == Pn::Unpredictable {
                } else {
                    if sqrz.results.pnc {
                        return Truth::F;
                    }
                }
            }
            return Truth::T;
        }

        // Get rules union
        let rulsx = arg1.get_rules_ref().union(arg2.get_rules_ref()).unwrap();

        // Check squares between for compatibility to the rules.
        for stax in stas.iter() {
            if reg1.is_superset_of_state(stax) || reg2.is_superset_of_state(stax) {
                continue;
            }

            let sqrz = squares.find(stax).unwrap();

            if sqrz.results.pn != *arg1.get_pn_ref() {
                if sqrz.results.pnc {
                    return Truth::F;
                }
                if sqrz.results.pn > *arg1.get_pn_ref() {
                    return Truth::F;
                }
            }

            if sqrz.rules.is_subset_of(&rulsx) == false {
                return Truth::F;
            }
        } // next stax
        Truth::T

    } // end can_combine_check_between_t

    /// Return Truth enum for the combination of any two squares,
    /// and the squares inbetween them, when the two squares cannot
    /// be combined alone due to one needing more samples.
    /// No more samples should be sought, if there is a square between
    /// that will cause a problem.
    fn can_combine_check_between_m<T: Compare, U: Compare>(arg1: &T, arg2: &U, squares: &SquareStore) -> Truth {

        let reg1 = arg1.get_region();
        let reg2 = arg2.get_region();

        // Get keys for all squares in the region formed by the
        // two given squares.
        let stas = squares
            .stas_in_reg(&reg1.union(&reg2));

        // Get refs of squares inbetween arg1 and arg2.
        let mut reglst = RegionStore::new();
        reglst.push(reg1);
        reglst.push(reg2);

        let mut sqrs_inbetween = Vec::<&SomeSquare>::with_capacity(stas.len());
        for stax in stas.iter() {
            if reglst.any_superset_of_state(stax) {
            } else {
                sqrs_inbetween.push(squares.find(stax).unwrap());
            }
        }

        if sqrs_inbetween.len() == 0 {
            return Truth::M;
        }

        // Try squares combining with each defining square.
        for sqrz in &sqrs_inbetween {

            if can_combine(*sqrz, arg1) == Truth::F {
                return Truth::F;
            }
            if can_combine(*sqrz, arg2) == Truth::F {
                return Truth::F;
            }
        }

        if sqrs_inbetween.len() == 1 {
            return Truth::M;
        }

        // Try combining each pair of inbetween squares.
        let mut inx = 0;
        let max_inx = sqrs_inbetween.len() - 1;
        for sqrz in &sqrs_inbetween {
            if inx < max_inx {
                for inx2 in (inx + 1)..max_inx {
                    if can_combine(*sqrz, sqrs_inbetween[inx2]) == Truth::F {
                        return Truth::F;
                    }
                }
            }

            inx += 1;
        } // next sqrz

        Truth::M
    } // end can_combine_check_between_m
