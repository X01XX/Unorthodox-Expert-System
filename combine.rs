    use crate::pn::Pn;
    use crate::truth::Truth;
    use crate::compare::Compare;
    use crate::squarestore::SquareStore;

    /// Can two structure rules, that implement the Compare trait (SomeSquare, SomeGroup), be combined?
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
    ///
    ///

    pub fn can_combine<T: Compare, U: Compare>(arg1: &T, arg2: &U) -> Truth {
        //println!("arg1.get_pn_ref() {} arg1.get_pnc() {} arg1.get_rules_ref() {} arg2.get_pn_ref() {} arg2.get_pnc() {} arg2.get_rules_ref() {}", arg1.get_pn_ref(), &arg1.get_pnc(), arg1.get_rules_ref(), arg2.get_pn_ref(), &arg2.get_pnc(), arg2.get_rules_ref());
        //println!("can_combine {} {}", &arg1.get_region(), &arg2.get_region());

        // Special check for single result bootstrapping
        if *arg1.get_pn_ref() == Pn::One && *arg2.get_pn_ref() == Pn::One {
            if arg1.get_rules_ref().can_form_union(arg2.get_rules_ref()) == Truth::T {
                return Truth::T;
            }
        }

        // When both are not pnc, more samples needed.
        if arg1.get_pnc() == false && arg2.get_pnc() == false {
            return Truth::M;
        }

        // When both are pnc
        if arg1.get_pnc() && arg2.get_pnc() {
            if *arg1.get_pn_ref() != *arg2.get_pn_ref()  { return Truth::F; }
            if *arg1.get_pn_ref() == Pn::Unpredictable { return Truth::T; }
            // Return the Truth of their possible union or rules.
            return arg1.get_rules_ref().can_form_union(arg2.get_rules_ref());
        }

        // When one is pnc
        if arg1.get_pnc() {
            if *arg1.get_pn_ref() == Pn::Unpredictable { return Truth::M; }
            if *arg2.get_pn_ref() > *arg1.get_pn_ref() { return Truth::F; }
            return arg1.get_rules_ref().can_form_union(arg2.get_rules_ref());
        }
        
        // arg2 must be pnc
        if *arg2.get_pn_ref() == Pn::Unpredictable { return Truth::M; }
        if *arg1.get_pn_ref() > *arg2.get_pn_ref() { return Truth::F; }
        arg1.get_rules_ref().can_form_union(arg2.get_rules_ref())
    } // end can_combine

    /// Return Truth enum for the combination of any two structs implementing the Compare trait,
    /// and the squares between them.
    pub fn can_combine_check_between<T: Compare, U: Compare>(arg1: &T, arg2: &U, squares: &SquareStore) -> Truth {
        //println!("can_combine_check_between {} {}", &arg1.get_region(), &arg2.get_region());
        assert!(arg1.get_region() != arg2.get_region());

        // Check the two structs
        let cmbx = can_combine(arg1, arg2);

        if cmbx != Truth::T {
            return cmbx;
        }

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

#[cfg(test)]
mod tests {
    use crate::square::SomeSquare;
    use crate::squarestore::SquareStore;
    use crate::state::SomeState;
    use crate::combine::*;
    use crate::truth::Truth;

    #[test]
    fn test_pn1_pn1_compatible() -> Result<(), String> {

        let sqr1 = SomeSquare::new(SomeState::new_from_string(2, "s0011").unwrap(), SomeState::new_from_string(2, "s0011").unwrap());
        let sqr2 = SomeSquare::new(SomeState::new_from_string(2, "s1001").unwrap(), SomeState::new_from_string(2, "s1001").unwrap());

        let ret = can_combine(&sqr1, &sqr2);

        if ret != Truth::T { 
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }

    #[test]
    fn test_pn1_pn1_not_compatible() -> Result<(), String> {

        let sqr1 = SomeSquare::new(SomeState::new_from_string(2, "s0011").unwrap(), SomeState::new_from_string(2, "s0011").unwrap());
        let sqr2 = SomeSquare::new(SomeState::new_from_string(2, "s1001").unwrap(), SomeState::new_from_string(2, "s1000").unwrap());

        let ret = can_combine(&sqr1, &sqr2);

        if ret != Truth::M { 
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }

    #[test]
    fn test_pn1_pn1_pnc_not_compatible() -> Result<(), String> {

        let mut sqr1 = SomeSquare::new(SomeState::new_from_string(2, "s0011").unwrap(), SomeState::new_from_string(2, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0011").unwrap());
        let sqr2 = SomeSquare::new(SomeState::new_from_string(2, "s1001").unwrap(), SomeState::new_from_string(2, "s1000").unwrap());

        let ret = can_combine(&sqr1, &sqr2);

        if ret != Truth::F { 
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }

    #[test]
    fn test_pn1_pn2_pnc_compatible() -> Result<(), String> {

        let mut sqr1 = SomeSquare::new(SomeState::new_from_string(2, "s0011").unwrap(), SomeState::new_from_string(2, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0010").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0010").unwrap());
        let sqr2 = SomeSquare::new(SomeState::new_from_string(2, "s1001").unwrap(), SomeState::new_from_string(2, "s1001").unwrap());

        let ret = can_combine(&sqr1, &sqr2);

        if ret != Truth::M { 
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }
    
    #[test]
    fn test_pn1_pnu() -> Result<(), String> {

        let mut sqr1 = SomeSquare::new(SomeState::new_from_string(2, "s0011").unwrap(), SomeState::new_from_string(2, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0010").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s011").unwrap());
        let sqr2 = SomeSquare::new(SomeState::new_from_string(2, "s1001").unwrap(), SomeState::new_from_string(2, "s1001").unwrap());

        let ret = can_combine(&sqr1, &sqr2);

        if ret != Truth::M { 
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }

    #[test]
    fn test_pn1_pnc_pnu() -> Result<(), String> {

        let mut sqr1 = SomeSquare::new(SomeState::new_from_string(2, "s0011").unwrap(), SomeState::new_from_string(2, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0010").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s011").unwrap());
        let mut sqr2 = SomeSquare::new(SomeState::new_from_string(2, "s1001").unwrap(), SomeState::new_from_string(2, "s1001").unwrap());
        sqr2.add_result(SomeState::new_from_string(2, "s1001").unwrap());

        let ret = can_combine(&sqr1, &sqr2);

        if ret != Truth::F { 
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }

    #[test]
    fn test_pn2_pn2_compatible() -> Result<(), String> {

        let mut sqr1 = SomeSquare::new(SomeState::new_from_string(2, "s0011").unwrap(), SomeState::new_from_string(2, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0010").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0010").unwrap());
        let mut sqr2 = SomeSquare::new(SomeState::new_from_string(2, "s1001").unwrap(), SomeState::new_from_string(2, "s1001").unwrap());
        sqr2.add_result(SomeState::new_from_string(2, "s1000").unwrap());

        let ret = can_combine(&sqr1, &sqr2);

        if ret != Truth::T { 
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }

    #[test]
    fn test_pn2_pn2_not_compatible() -> Result<(), String> {

        let mut sqr1 = SomeSquare::new(SomeState::new_from_string(2, "s0011").unwrap(), SomeState::new_from_string(2, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0010").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0010").unwrap());
        let mut sqr2 = SomeSquare::new(SomeState::new_from_string(2, "s1001").unwrap(), SomeState::new_from_string(2, "s1001").unwrap());
        sqr2.add_result(SomeState::new_from_string(2, "s1011").unwrap());

        let ret = can_combine(&sqr1, &sqr2);

        if ret != Truth::F { 
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }

    #[test]
    fn test_pn2_pn2_too_compatible() -> Result<(), String> {

        let mut sqr1 = SomeSquare::new(SomeState::new_from_string(2, "s0011").unwrap(), SomeState::new_from_string(2, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0010").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0011").unwrap());
        sqr1.add_result(SomeState::new_from_string(2, "s0010").unwrap());
        let mut sqr2 = SomeSquare::new(SomeState::new_from_string(2, "s1000").unwrap(), SomeState::new_from_string(2, "s1001").unwrap());
        sqr2.add_result(SomeState::new_from_string(2, "s1000").unwrap());

        let ret = can_combine(&sqr1, &sqr2);

        if ret != Truth::F { 
            return Err(format!("ret: {}", ret));
        }

        Ok(())
    }
    
    #[test]
    fn test_check_between_nothing_between() -> Result<(), String> {

        let mut squares = SquareStore::new();

        let sqr1 = SomeSquare::new(SomeState::new_from_string(2, "s1111").unwrap(), SomeState::new_from_string(2, "s1111").unwrap());
        squares.insert(sqr1, 0, 0);

        let sqr2 = SomeSquare::new(SomeState::new_from_string(2, "s0000").unwrap(), SomeState::new_from_string(2, "s0000").unwrap());
        squares.insert(sqr2, 0, 0);

        // Non compatible square, but not between.
        let sqr4 = SomeSquare::new(SomeState::new_from_string(2, "s10001").unwrap(), SomeState::new_from_string(2, "s10000").unwrap());
        squares.insert(sqr4, 0, 0);

        let sqrx: &SomeSquare = squares.find(&SomeState::new_from_string(2, "s1111").unwrap()).unwrap();
        let sqry: &SomeSquare = squares.find(&SomeState::new_from_string(2, "s0000").unwrap()).unwrap();

        let rslt = can_combine_check_between(sqrx, sqry, &squares);

        if rslt != Truth::T {
            return Err(format!("rslt: {}", rslt));
        }
        Ok(())
    }

    #[test]
    fn test_check_between_compat_between() -> Result<(), String> {

        let mut squares = SquareStore::new();
        let sqr1 = SomeSquare::new(SomeState::new_from_string(2, "s1111").unwrap(), SomeState::new_from_string(2, "s1111").unwrap());
        squares.insert(sqr1, 0, 0);

        let sqr2 = SomeSquare::new(SomeState::new_from_string(2, "s0000").unwrap(), SomeState::new_from_string(2, "s0000").unwrap());
        squares.insert(sqr2, 0, 0);

        // Compatible square between.
        let sqr3 = SomeSquare::new(SomeState::new_from_string(2, "s0001").unwrap(), SomeState::new_from_string(2, "s0001").unwrap());
        squares.insert(sqr3, 0, 0);

        // Non compatible square, but not between.
        let sqr4 = SomeSquare::new(SomeState::new_from_string(2, "s10001").unwrap(), SomeState::new_from_string(2, "s10000").unwrap());
        squares.insert(sqr4, 0, 0);

        let sqrx: &SomeSquare = squares.find(&SomeState::new_from_string(2, "s1111").unwrap()).unwrap();
        let sqry: &SomeSquare = squares.find(&SomeState::new_from_string(2, "s0000").unwrap()).unwrap();

        let rslt = can_combine_check_between(sqrx, sqry, &squares);

        if rslt != Truth::T {
            return Err(format!("rslt: {}", rslt));
        }
        Ok(())
    }

    #[test]
    fn test_check_between_noncompat_between() -> Result<(), String> {

        let mut squares = SquareStore::new();
        let sqr1 = SomeSquare::new(SomeState::new_from_string(2, "s1111").unwrap(), SomeState::new_from_string(2, "s1111").unwrap());
        squares.insert(sqr1, 0, 0);

        let sqr2 = SomeSquare::new(SomeState::new_from_string(2, "s0000").unwrap(), SomeState::new_from_string(2, "s0000").unwrap());
        squares.insert(sqr2, 0, 0);

        // Non compatible square between.
        let sqr3 = SomeSquare::new(SomeState::new_from_string(2, "s0001").unwrap(), SomeState::new_from_string(2, "s0000").unwrap());
        squares.insert(sqr3, 0, 0);

        // Compatible square between.
        let sqr4 = SomeSquare::new(SomeState::new_from_string(2, "s0011").unwrap(), SomeState::new_from_string(2, "s0011").unwrap());
        squares.insert(sqr4, 0, 0);

        let sqrx: &SomeSquare = squares.find(&SomeState::new_from_string(2, "s1111").unwrap()).unwrap();
        let sqry: &SomeSquare = squares.find(&SomeState::new_from_string(2, "s0000").unwrap()).unwrap();

        let rslt = can_combine_check_between(sqrx, sqry, &squares);

        if rslt != Truth::F {
            return Err(format!("rslt: {}", rslt));
        }
        Ok(())
    }
}
