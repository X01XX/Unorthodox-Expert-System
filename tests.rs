//! Tests for an Unorthodox Expert System.
//!

#[cfg(test)]
mod tests {
//    use crate::bits::SomeBits;
    use crate::domain::SomeDomain;
//    use crate::change::SomeChange;
    use crate::mask::SomeMask;
//    use crate::maskstore::MaskStore;
//    use crate::action::SomeAction;
//    use crate::region::SomeRegion;
    use crate::regionstore::RegionStore;
//    use crate::resultstore::ResultStore;
    use crate::rule::SomeRule;
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
        let mut dm1 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm1.add_action();

        let s5 = dm1.state_from_string("s101").unwrap();

        let s4 = dm1.state_from_string("s100").unwrap();

        let sf = dm1.state_from_string("s1111").unwrap();

        let se = dm1.state_from_string("s1110").unwrap();

        let s7 = dm1.state_from_string("s111").unwrap();

        let rx1x1 = dm1.region_from_string("rx1x1").unwrap();

        dm1.eval_sample_arbitrary(0, &s5, &s5);
        dm1.eval_sample_arbitrary(0, &s5, &s4);
        dm1.eval_sample_arbitrary(0, &s5, &s5);
        dm1.eval_sample_arbitrary(0, &s5, &s4);

        dm1.eval_sample_arbitrary(0, &sf, &se);
        dm1.eval_sample_arbitrary(0, &sf, &sf);
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
                    //println!("\nActs: {}", &dm1.actions[0]);
                    //println!("       Sqrs: ({})", dm1.actions[0].squares);
                    return Ok(());
                }
            } else {
                //println!("\nActs: {}", &dm1.actions[0]);
                //println!("       Sqrs: ({})", dm1.actions[0].squares);
                //println!("Group deleted too soon!");
                return Err(String::from("failed, rx1x1 deleted too soon"));
            }
        } else {
            //println!("\nActs: {}", &dm1.actions[0]);
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
        let mut dm1 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm1.add_action();

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
            println!("\nActs: {}", &dm1.actions[0]);
            dm1.eval_sample_arbitrary(0, &s7, &s7);

            if let Some(_regx) = dm1.actions[0].groups.find(&rx1x1) {
                dm1.eval_sample_arbitrary(0, &s7, &s7); // cause pn-not-Two invalidation
                if let Some(_regx) = dm1.actions[0].groups.find(&rx1x1) {
                    //println!("\nActs: {}", &dm1.actions[0]);
                    //println!(" {}", dm1.actions[0].get_squares());
                    return Err(String::from(
                        "Four samples for s7 failed to invalidate group xx1x1",
                    ));
                } else {
                    return Ok(());
                }
            } else {
                 //println!("\nActs: {}", &dm1.actions[0]);
                 //println!("       Sqrs: ({})", dm1.actions[0].get_squares());
                return Err(String::from("Group deleted too soon"));
            }
        } else {
            //println!("\nActs: {}", &dm1.actions[0]);
            return Err(String::from("group rx1x1 was not formed by two squares!"));
        }
    } // end group_pn_u_union_then_invalidation

    #[test]
    fn region_subtraction() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        let reg0 = dm0.region_from_string("rX10X").unwrap();

        let reg1 = dm0.region_from_string("r0XX1").unwrap();

        let regvec = reg0.subtract(&reg1);
        
        let mut regs = RegionStore::new();
        for regx in &regvec {
            regs.push(regx.clone());
        }

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

    #[test]
    fn possible_regions_for_group_by_elimination() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        let reg_1110x = dm0.region_from_string("r1110x").unwrap();
        
        let s0 = dm0.state_from_string("s0").unwrap();

        let s1a = dm0.state_from_string("s11010").unwrap();

        let s1c = dm0.state_from_string("s11100").unwrap();

        let s1d = dm0.state_from_string("s11101").unwrap();

        dm0.eval_sample_arbitrary(0, &s1d, &s0);
        dm0.eval_sample_arbitrary(0, &s1c, &s0);  // Group r1110x

        dm0.eval_sample_arbitrary(0, &s1a, &s1a.s_not());  // Group r0010

        if let Some(grpx) = dm0.actions[0].groups.find(&reg_1110x) {
            println!("Region r1110x found");

            let regs_new = dm0.actions[0].possible_regions_for_group(&grpx, &SomeMask::_from_string(1, "m110").unwrap());

            if regs_new.len() != 2 {
                return Err(format!("possible regions for {} given {} is ? {}", &grpx.region, &s1a, &regs_new.formatted_string()));
            }

            if regs_new.contains(&dm0.region_from_string("r111XX").unwrap()) {
            } else {
                return Err(format!("Region r111XX not found in {} ??", &regs_new.formatted_string()));
            }

            if regs_new.contains(&dm0.region_from_string("r11X0X").unwrap()) {
            } else {
                return Err(format!("Region r1XX0X not found in {} ??", &regs_new.formatted_string()));
            }
        } else {
            return Err(format!("Region r1110x not created? {}", dm0.actions[0]));
        }
        
        Ok(())
    }

    #[test]
    fn rule_valid_subset() -> Result<(), String> {
        let rul1 = SomeRule {
            b00: SomeMask::_from_string(1, "m00111001").unwrap(),
            b01: SomeMask::_from_string(1, "m11000110").unwrap(),
            b11: SomeMask::_from_string(1, "m10010011").unwrap(),
            b10: SomeMask::_from_string(1, "m01101100").unwrap(),
        };

        let rul2 = SomeRule {
            b00: SomeMask::_from_string(1, "m11000000").unwrap(),
            b01: SomeMask::_from_string(1, "m00110000").unwrap(),
            b11: SomeMask::_from_string(1, "m00001100").unwrap(),
            b10: SomeMask::_from_string(1, "m00000011").unwrap(),
        };

        let rul3 = rul1.union(&rul2);

        let rul4 = rul3.valid_subset().unwrap();

        let rul5 = SomeRule {
            b00: SomeMask::_from_string(1, "m00001001").unwrap(),
            b01: SomeMask::_from_string(1, "m00000110").unwrap(),
            b11: SomeMask::_from_string(1, "m10010000").unwrap(),
            b10: SomeMask::_from_string(1, "m01100000").unwrap(),
        };
        
        if rul4 == rul5 {
            return Ok(());
        }
        
        return Err(format!("subset is? {}", rul4.formatted_string()));
    }

} // end mod tests
