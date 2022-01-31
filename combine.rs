    use crate::pn::Pn;
    use crate::truth::Truth;
    use crate::compare::Compare;
    use crate::square::SomeSquare;
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

    /// Return the Truth of a combination where the Pn value of the first argument is LT the
    /// Pn value of the second argument.
    fn can_combine_lt<T: Compare, U: Compare>(arg1: &T, arg2: &U) -> Truth {
        //println!("can_combine_check_lt {} {}", &arg1.get_region(), &arg2.get_region());
        assert!(arg1.get_pn_ref() < arg2.get_pn_ref());

        // If arg1 is pnc, it must be disimilar to arg2.
        if arg1.get_pnc() {
            return Truth::F;
        }

        // If arg2 is unpredictable, more samples are needed of arg1.
        if *arg2.get_pn_ref() == Pn::Unpredictable {
            return Truth::M;
        }

        // If the initial region of arg1 rules are a subset of the initial region of arg2 rules,
        // check that the rules of arg1 are a subset of arg2 rules.
        if arg1.get_rules_ref().initial_region().is_subset_of(&arg2.get_rules_ref().initial_region()) {
            if arg1.get_rules_ref().is_subset_of(arg2.get_rules_ref()) {
                return Truth::T;
            } else {
                return Truth::F;
            }
        }

        // If the initial regions of arguments rules do not have a subset/superset
        // relation, return the Truth of their possible union.
        arg1.get_rules_ref().can_form_union(arg2.get_rules_ref())
    }

    /// Return the Truth of a combination for any two types that have the Compare Trait.
    pub fn can_combine<T: Compare, U: Compare>(arg1: &T, arg2: &U) -> Truth {
        //println!("arg1.get_pn_ref() {} arg1.get_pnc() {} arg1.get_rules_ref() {} arg2.get_pn_ref() {} arg2.get_pnc() {} arg2.get_rules_ref() {}", arg1.get_pn_ref(), &arg1.get_pnc(), arg1.get_rules_ref(), arg2.get_pn_ref(), &arg2.get_pnc(), arg2.get_rules_ref());
        //println!("can_combine {} {}", &arg1.get_region(), &arg2.get_region());

        // Calc Truth for equal Pn values.
        if arg1.get_pn_ref() == arg2.get_pn_ref() {

            // If the Pn value for both is unpredictable, they can be combined.
            if *arg1.get_pn_ref() == Pn::Unpredictable {
                //println!("can_combine: 1 returning T");
                return Truth::T;
            }

            // If the initial region of arg1 rules are a subset of the initial region of arg2 rules,
            // check that the rules of arg1 are a subset of arg2 rules.
            if arg1.get_rules_ref().initial_region().is_subset_of(&arg2.get_rules_ref().initial_region()) {
                if arg1.get_rules_ref().is_subset_of(arg2.get_rules_ref()) {
                    //println!("can_combine: 2 returning T");
                    return Truth::T;
                } else {
                    //println!("can_combine: 3 returning F");
                    return Truth::F;
                }
            }

            // If the initial region of arg2 rules are a subset of the initial region of arg1 rules,
            // check that the rules of arg2 are a subset of arg1 rules.
            if arg2.get_rules_ref().initial_region().is_subset_of(&arg1.get_rules_ref().initial_region()) {
                if arg2.get_rules_ref().is_subset_of(arg1.get_rules_ref()) {
                    //println!("can_combine: 4 returning T");
                    return Truth::T;
                } else {
                    //println!("can_combine: 5 returning ");
                    return Truth::F;
                }
            }

            // If the initial regions of arguments rules do not have a subset/superset
            // relation, return the Truth of their possible union.
            let ret = arg1.get_rules_ref().can_form_union(arg2.get_rules_ref());
            //println!("can_combine: 6 returning {}", ret);
            return ret;
        }

        // Calc Truth for Not Equal Pn values.
        if arg1.get_pn_ref() < arg2.get_pn_ref() {
            let ret = can_combine_lt(arg1, arg2);
            //println!("can_combine: 7 returning {}", ret);
            return ret;
        }

        // Must be arg2.get_pn_ref() < arg1.get_pn_ref()
        let ret = can_combine_lt(arg2, arg1);
        //println!("can_combine: 8 returning {}", ret);
        ret
    } // end can_combine

    /// Return Truth enum for the combination of any two structs implementing the Compare trait,
    /// and the squares between them.
    pub fn can_combine_check_between<T: Compare, U: Compare>(arg1: &T, arg2: &U, squares: &SquareStore) -> Truth {
        //println!("can_combine_check_between {} {}", &arg1.get_region(), &arg2.get_region());
        assert!(arg1.get_region() != arg2.get_region());

        // Check the two structs
        let cmbx = can_combine(arg1, arg2);

        if cmbx == Truth::T {
            return can_combine_check_between_t(arg1, arg2, squares);
        }

        if cmbx == Truth::M {
            return can_combine_check_between_m(arg1, arg2, squares);
        }
        panic!("Should not happen!");
    }

    /// Return Truth enum for the combination of any two structs implementing the Compare trait,
    /// and the squares between them, when the two structs rules can be combined.
    fn can_combine_check_between_t<T: Compare, U: Compare>(arg1: &T, arg2: &U, squares: &SquareStore) -> Truth {

        let reg1 = arg1.get_region();
        let reg2 = arg2.get_region();

        // Get keys for all squares in the region formed by the
        // two given structs.
        let stas = squares
            .stas_in_reg(&arg1.get_region().union(&arg2.get_region()));

        // Handle Pn::Unpredictable squares
        if *arg1.get_pn_ref() == Pn::Unpredictable {
            // Check each between square
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

    /// Return Truth enum for the combination of any two structs implementing the Compare trait,
    /// and the squares between them, when the two squares can be combined (can_combine returns Truth::M)
    /// so one has a smaller pn value, without being pnc.
    /// More samples should be sought, unless there is a square, or a square pair, between
    /// that will cause a problem.
    fn can_combine_check_between_m<T: Compare, U: Compare>(arg1: &T, arg2: &U, squares: &SquareStore) -> Truth {
        //println!("can_combine_check_between_m {} {}", &arg1.get_region(), &arg2.get_region());
        //println!("can_combine_check_between_m pnA {} pnB {}", arg1.get_pn_ref(), arg2.get_pn_ref());

        if arg1.get_pn_ref() < arg2.get_pn_ref() {
            return can_combine_check_between_m2(arg1, arg2, squares);
        }
        return can_combine_check_between_m2(arg2, arg1, squares);
    }

    /// Return truth value of combining arg1 (Pn::One) and arg2 (Pn:Two), checking
    /// squares between.
    fn can_combine_check_between_m2<T: Compare, U: Compare>(arg1: &T, arg2: &U, squares: &SquareStore) -> Truth {
        //println!("can_combine_check_between_m2 {} {}", &arg1.get_region(), &arg2.get_region());
        let reg1 = arg1.get_region();
        let reg2 = arg2.get_region();

        // Get refs for squares in the region formed by the
        // two given structs.
        let sqrs: Vec<&SomeSquare> = squares.squares_in_reg(&reg1.union(&reg2));

        // Get refs of squares inbetween arg1 and arg2.
        let mut sqrs_inbetween = Vec::<&SomeSquare>::with_capacity(sqrs.len());
        for sqrx in sqrs.iter() {
            if reg1.is_superset_of_state(&sqrx.state) || reg2.is_superset_of_state(&sqrx.state) {
            } else {
                sqrs_inbetween.push(sqrx);
            }
        }

        if sqrs_inbetween.len() == 0 {
            return Truth::M;
        }

        // Try squares combining with each defining struct.
        // A Pn::One (non-pnc) and a Pn::One (non-pnc) should not be checked for compatibility.
        // A 0->1 square would be incompatible with a 0->0 square, but both would
        // be compatible with a [0->0, 0->1] square.
        for sqrz in &sqrs_inbetween {

            if sqrz.results.pn == Pn::Two {
                if can_combine(*sqrz, arg1) == Truth::F {
                    return Truth::F;
                }
            }
            // A Pn::One or Pn::Two square can be compared with the Pn::Two argument.
            if can_combine(*sqrz, arg2) == Truth::F {
                return Truth::F;
            }
        }

        if sqrs_inbetween.len() == 1 {
            return Truth::M;
        }

        // Try combining each pair of between squares.
        let mut inx = 0;
        let max_inx = sqrs_inbetween.len() - 1;
        for sqrz in &sqrs_inbetween {
            if inx < max_inx { // skip last square, as there is no + 1 element.

                for inx2 in (inx + 1)..sqrs_inbetween.len() {
                    let sqry = sqrs_inbetween[inx2];
                    if *sqrz.get_pn_ref() == Pn::One && *sqry.get_pn_ref() == Pn::One {
                    } else {
                        if can_combine(*sqrz, sqry) == Truth::F {
                            return Truth::F;
                        }
                    }
                } // next inx2, sqry
            }
            inx += 1;
        } // next sqrz

        Truth::M
    } // end can_combine_check_between_m
