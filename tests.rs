//! Tests for an Unorthodox Expert System.
//!

#[cfg(test)]
mod tests {
    use crate::bits::SomeBits;
    use crate::domain::SomeDomain;
//    use crate::mask::SomeMask;
//    use crate::maskstore::MaskStore;
    use crate::action::SomeAction;
//    use crate::region::SomeRegion;
    use crate::regionstore::RegionStore;
//    use crate::resultstore::ResultStore;
//    use crate::rule::SomeRule;
//    use crate::rulestore::RuleStore;
//    use crate::state::SomeState;
//    use crate::statestore::StateStore;
//    use crate::step::SomeStep;
//    use crate::stepstore::StepStore;

    // Form a group, X1X1 from two squares that have alternating (pn=Two) results.
    //
    // Sample a square, 0111, in the group, once.  There should be no change.
    //
    // Sample the square a second time, with the same result, proving it cannot have an
    // alternting result.
    //
    // Then group X1X1 should be invalidated and removed.
    // **********************************************************************************
    #[test]
    fn group_pn_2_union_then_invalidation() -> Result<(), String> {
        let mut dm1 = SomeDomain::new(1, "s1", "r1");
        dm1.push(SomeAction::new(1), 0);

        let s5 = dm1.state_from_string("s101").unwrap();

        let s4 = dm1.state_from_string("s100").unwrap();

        let sf = dm1.state_from_string("s1111").unwrap();

        let se = dm1.state_from_string("s1110").unwrap();

        let s7 = dm1.state_from_string("s111").unwrap();

        let rx1x1 = dm1.region_from_string("rx1x1").unwrap();

        dm1.eval_sample_arbitrary(0, &s5, &s5);
        dm1.eval_sample_arbitrary(0, &s5, &s4);
        dm1.eval_sample_arbitrary(0, &sf, &se);
        dm1.eval_sample_arbitrary(0, &sf, &sf);

        if let Some(_regx) = dm1.actions[0].groups.find(&rx1x1) {
            dm1.eval_sample_arbitrary(0, &s7, &s7);

            if let Some(_regx) = dm1.actions[0].groups.find(&rx1x1) {
                dm1.eval_sample_arbitrary(0, &s7, &s7); // cause not-pn=2 condition

                if let Some(_) = dm1.actions[0].groups.find(&rx1x1) {
                    //println!("\nActs: {}", &dm1.actions[0]);
                    //println!(" {}", dm1.actions[0].squares);
                    return Err(String::from("failed, rx1x1 should have been deleted"));
                } else {
                    // println!("\nActs: {}", &dm1.actions[0]);
                    // println!("       Sqrs: ({})", dm1.actions[0].squares);
                    return Ok(());
                }
            } else {
                //println!("\nActs: {}", &dm1.actions[0]);
                //println!("       Sqrs: ({})", dm1.actions[0].squares);
                //println!("Group deleted too soon!");
                return Err(String::from("failed, rx1x1 deleted too soon"));
            }
        } else {
            //  println!("\nActs: {}", &dm1.actions[0]);
            return Err(String::from(
                "failed, group rx1x1 was not formed by two squares",
            ));
        }
    } // end group_pn_2_union_then_invalidation

    // Form a group, X1X1 from two squares that have unpredictable results.
    //
    // Sample a square, 0111, in the group, several times.  There should be no change until pnc (4 samples).
    //
    // Then group X1X1 should be invalidate and removed.
    // **********************************************************************************
    #[test]
    fn group_pn_u_union_then_invalidation() -> Result<(), String> {
        let mut dm1 = SomeDomain::new(1, "s1", "r1");
        dm1.push(SomeAction::new(1), 0);

        let s5 = dm1.state_from_string("s101").unwrap();

        let s4 = dm1.state_from_string("s100").unwrap();

        let sf = dm1.state_from_string("s1111").unwrap();

        let se = dm1.state_from_string("s1110").unwrap();

        let s7 = dm1.state_from_string("s111").unwrap();

        let rx1x1 = dm1.region_from_string("rx1x1").unwrap();

        //println!(
        //    "state 5 = {} s4 {} sF {} sE {} rxx1x1 {}",
        //    s5, s4, sf, se, rx1x1
        //);
        dm1.eval_sample_arbitrary(0, &s5, &s5);
        dm1.eval_sample_arbitrary(0, &s5, &s4);
        dm1.eval_sample_arbitrary(0, &s5, &se);

        dm1.eval_sample_arbitrary(0, &sf, &se);
        dm1.eval_sample_arbitrary(0, &sf, &sf);
        dm1.eval_sample_arbitrary(0, &sf, &s4);

        if let Some(_regx) = dm1.actions[0].groups.find(&rx1x1) {
            //println!("\nActs: {}", &dm1.actions[0]);
            dm1.eval_sample_arbitrary(0, &s7, &s7);
            dm1.eval_sample_arbitrary(0, &s7, &s7);
            dm1.eval_sample_arbitrary(0, &s7, &s7);

            if let Some(_regx) = dm1.actions[0].groups.find(&rx1x1) {
                dm1.eval_sample_arbitrary(0, &s7, &s7); // cause pn-not-Two invalidation
                if let Some(_regx) = dm1.actions[0].groups.find(&rx1x1) {
                    println!("\nActs: {}", &dm1.actions[0]);
                    println!(" {}", dm1.actions[0].squares);
                    return Err(String::from(
                        "Four samples for s7 failed to invalidate group xx1x1",
                    ));
                } else {
                    return Ok(());
                }
            } else {
                // println!("\nActs: {}", &dm1.actions[0]);
                // println!("       Sqrs: ({})", dm1.actions[0].squares);
                return Err(String::from("Group deleted too soon"));
            }
        } else {
            //  println!("\nActs: {}", &dm1.actions[0]);
            return Err(String::from("group rx1x1 was not formed by two squares!"));
        }
    } // end group_pn_u_union_then_invalidation

    #[test]
    fn shift_left() -> Result<(), String> {
        let bts1 = SomeBits {
            ints: vec![0 as u8, 129 as u8],
        };

        let bts2 = bts1.shift_left();
        let bts3 = SomeBits {
            ints: vec![1 as u8, 2 as u8],
        };

        if bts2 != bts3 {
            return Err(String::from("bts2 NEQ bts3"));
        }
        Ok(())
    }
    
    #[test]
    fn region_subtraction() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(1, "s1", "r1");
        dm0.push(SomeAction::new(1), 0);

        let reg0 = dm0.region_from_string("rX10X").unwrap();

        let reg1 = dm0.region_from_string("r0XX1").unwrap();

        let regs = RegionStore {
            avec: reg0._subtract(&reg1),
        };

        if regs.len() != 2 {
            return Err(format!("{} minus {} = {} ??", &reg0, &reg1, &regs.formatted_string()));
        }

        if regs.contains(&dm0.region_from_string("rX100").unwrap()) {
        } else {
            return Err(format!("{} minus {} = {} ??", &reg0, &reg1, &regs.formatted_string()));
        }

        if regs.contains(&dm0.region_from_string("r110X").unwrap()) {
        } else {
            return Err(format!("{} minus {} = {} ??", &reg0, &reg1, &regs.formatted_string()));
        }

        Ok(())
    }
    
} // end mod tests
