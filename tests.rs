//! Tests for an Unorthodox Expert System.
//!

#[cfg(test)]
mod tests {
//    use crate::bits::SomeBits;
    use crate::domain::SomeDomain;
    use crate::change::SomeChange;
    use crate::mask::SomeMask;
//    use crate::maskstore::MaskStore;
//    use crate::action::SomeAction;
    use crate::region::SomeRegion;
    use crate::regionstore::RegionStore;
//    use crate::resultstore::ResultStore;
    use crate::rule::SomeRule;
//    use crate::rulestore::RuleStore;
//    use crate::state::SomeState;
//    use crate::statestore::StateStore;
//    use crate::step::SomeStep;
//    use crate::stepstore::StepStore;
    use crate::need::SomeNeed;
    use std::cmp::Ordering;
    use crate::truth::Truth;
    use crate::pn::Pn;

    #[test]
    /// Form a group, X1X1 from two squares that have alternating (pn=Two) results.
    ///
    /// Sample a square, 0111, in the group, once.  There should be no change.
    ///
    /// Sample the square a second time, with the same result, proving it cannot have an
    /// alternting result.
    ///
    /// Then group X1X1 should be invalidated and removed.
    /// **********************************************************************************
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

    // Test X10X - 0XX1 = X100, 110X.
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

    // Test the expansion of a group with similar external squares.
    #[test]
    fn possible_regions_for_group_with_sim_sqrs() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        let reg_11xx = dm0.region_from_string("r11xx").unwrap();
        let reg_x1x1 = dm0.region_from_string("rx1x1").unwrap();
        let reg_x10x = dm0.region_from_string("rx10x").unwrap();
        let reg_x1xx = dm0.region_from_string("rx1xx").unwrap();
        let reg_1xxx = dm0.region_from_string("r1xxx").unwrap();

        let sq5 = dm0.state_from_string("s101").unwrap();

        let sqc = dm0.state_from_string("s1100").unwrap();

        let sqf = dm0.state_from_string("s1111").unwrap();

        let chg_mask7 = SomeMask::_from_string(1, "m111").unwrap();
        
        let chg_maskf = SomeMask::_from_string(1, "m1111").unwrap();

        // Form group r11xx
        dm0.eval_sample_arbitrary(0, &sqc, &sqc);
        dm0.eval_sample_arbitrary(0, &sqf, &sqf);

        // Add square 5, forming groups rx1x1, x10x.
        dm0.eval_sample_arbitrary(0, &sq5, &sq5);

        assert!(dm0.actions[0].groups.len() == 3);
        assert!(if let Some(_) = dm0.actions[0].groups.find(&reg_11xx) { true } else { false });
        assert!(if let Some(_) = dm0.actions[0].groups.find(&reg_x1x1) { true } else { false });
        assert!(if let Some(_) = dm0.actions[0].groups.find(&reg_x10x) { true } else { false });

        if let Some(grpx) = dm0.actions[0].groups.find(&reg_11xx) {
            let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_maskf);
            println!("for {} seek regs {}", &reg_11xx, &regs_exp);
            assert!(regs_exp.len() == 1);
            assert!(regs_exp.contains(&reg_x1xx));

            // Test non-sim-expansion under change mask, reverts to no-sim, no-dis, under change mask.
            let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_mask7);
            println!("for {} seek regs {}", &reg_11xx, &regs_exp);
            assert!(regs_exp.len() == 1);
            assert!(regs_exp.contains(&reg_1xxx));
        }

        Ok(())
    }

    // Test the expansion of a group with no external squares.
    #[test]
    fn possible_regions_for_group_no_dis_or_sim_sqrs() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        let reg_110x = dm0.region_from_string("r110x").unwrap();
        
        let sq0 = dm0.state_from_string("s0").unwrap();

        let sqc = dm0.state_from_string("s1100").unwrap();

        let sqd = dm0.state_from_string("s1101").unwrap();

        dm0.eval_sample_arbitrary(0, &sqd, &sq0);

        let reg_sqd = SomeRegion::new(&sqd, &sqd);

        let chg_mask5 = SomeMask::_from_string(1, "m101").unwrap();
        let chg_mask6 = SomeMask::_from_string(1, "m110").unwrap();
        let chg_mask7 = SomeMask::_from_string(1, "m111").unwrap();
        let chg_maskf = SomeMask::_from_string(1, "m1111").unwrap();

        // Test logic for a group, with no external similar, or disimilar, squares.
        if let Some(grpx) = dm0.actions[0].groups.find(&reg_sqd) {
            // Test with 3-bit change mask
            let mut regs_found = RegionStore::with_capacity(6);
            let mut cnt = 0;
            let limit = 60;
            while regs_found.len() < 6 {
                cnt += 1;
                if cnt > limit {
                    return Err(format!("failed to find 6 options in {} tries", limit));
                }
                // Repeated runs should elicit XX11, X0X1, X01X, 1XX1, 1X1X, 10XX.
                let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_maskf);
                println!("for {} seek regs {}", &reg_sqd, &regs_exp);
                assert!(regs_exp.len() == 1);
                assert!(regs_exp[0].x_mask().num_one_bits() == 2);
                assert!(regs_exp[0].x_mask().m_and(&chg_maskf.m_not()).is_low());
                assert!(regs_exp[0].is_superset_of(&reg_sqd));
                if regs_found.contains(&regs_exp[0]) {
                } else {
                    regs_found.push(regs_exp[0].clone());
                }
            }
            println!("regs found {}", &regs_found);

            // Test with 3-bit change mask.
            let mut regs_found = RegionStore::with_capacity(3);
            let mut cnt = 0;
            let limit = 30;
            while regs_found.len() < 3 {
                cnt += 1;
                if cnt > limit {
                    return Err(format!("failed to find 3 options in {} tries", limit));
                }
                // Repeated runs should elicit 1X01, 11X1, 110X. (int(3 bits/2) = 1 X bit)
                let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_mask7);
                println!("for {} seek regs {}", &reg_sqd, &regs_exp);
                assert!(regs_exp.len() == 1);
                assert!(regs_exp[0].x_mask().num_one_bits() == 1);
                assert!(regs_exp[0].x_mask().m_and(&chg_mask7.m_not()).is_low());
                assert!(regs_exp[0].is_superset_of(&reg_sqd));
                if regs_found.contains(&regs_exp[0]) {
                } else {
                    regs_found.push(regs_exp[0].clone());
                }
            }
            println!("regs found {}", &regs_found);

            // Test with 2-bit change mask.
            let mut regs_found = RegionStore::with_capacity(2);
            let mut cnt = 0;
            let limit = 20;
            while regs_found.len() < 2 {
                cnt += 1;
                if cnt > limit {
                    return Err(format!("failed to find 2 options in {} tries", limit));
                }
                // Repeated runs should elicit 1X01, 110X.
                let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_mask5);
                println!("for {} seek regs {}", &reg_sqd, &regs_exp);
                assert!(regs_exp.len() == 1);
                assert!(regs_exp[0].x_mask().num_one_bits() == 1);
                assert!(regs_exp[0].x_mask().m_and(&chg_mask5.m_not()).is_low());
                assert!(regs_exp[0].is_superset_of(&reg_sqd));
                if regs_found.contains(&regs_exp[0]) {
                } else {
                    regs_found.push(regs_exp[0].clone());
                }
            }
            println!("regs found {}", &regs_found);
        } else {
            return Err(format!("Group r1101 not found!"));
        }

        dm0.eval_sample_arbitrary(0, &sqc, &sq0);  // Group r110x

        if let Some(grpx) = dm0.actions[0].groups.find(&reg_110x) {
            // Test region with 1 X, with 2-bit change mask.
            let mut regs_found = RegionStore::with_capacity(2);
            while regs_found.len() < 2 {
                // Repeated runs should elicit 1X0X, 11XX.
                let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_mask6);
                println!("for {} seek regs {}", &reg_110x, &regs_exp);
                assert!(regs_exp.len() == 1);
                assert!(regs_exp[0].x_mask().num_one_bits() == 2);
                assert!(regs_exp[0].is_superset_of(&reg_110x));
                if regs_found.contains(&regs_exp[0]) {
                } else {
                    regs_found.push(regs_exp[0].clone());
                }
            }
            println!("regs found {}", &regs_found);
            
            // Test region with 1 X, with 2-bit change mask that includes a region X-bit position.
            let mut regs_found = RegionStore::with_capacity(2);
            for _ in 0..4 {
                // Repeated runs should elicit 1X0X.
                let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_mask5);
                println!("for {} seek regs {}", &reg_110x, &regs_exp);
                assert!(regs_exp.len() == 1);
                assert!(regs_exp[0].x_mask().num_one_bits() == 2);
                assert!(regs_exp[0].is_superset_of(&reg_110x));
                if regs_found.contains(&regs_exp[0]) {
                } else {
                    regs_found.push(regs_exp[0].clone());
                }
            }
            assert!(regs_found.len() == 1);
            println!("regs found {}", &regs_found);
        } else {
            return Err(format!("Group r110x not found!"));
        }

        Ok(())
    }

    /// Take an invalid rule, see if a valid subset exists.
    ///
    /// 00 + X1 = 11, 0X, return 11.
    /// 00 + Xx = 10, 0X, return 10.
    /// 01 + X0 = 10, 0X, return 10.
    /// 01 + XX = 11, 0X, return 11.
    /// 11 + X0 = 00, 1X, return 00.
    /// 11 + Xx = 01, 1X, return 01.
    /// 10 + X1 = 01, 1X, return 01.
    /// 10 + XX = 00, 1X, return 00.
    #[test]
    fn rule_valid_subset() -> Result<(), String> {
        // Form a rule of X1/Xx/X0/XX/X0/Xx/X1/XX
        let rul1 = SomeRule {
            b00: SomeMask::_from_string(1, "m00111001").unwrap(),
            b01: SomeMask::_from_string(1, "m11000110").unwrap(),
            b11: SomeMask::_from_string(1, "m10010011").unwrap(),
            b10: SomeMask::_from_string(1, "m01101100").unwrap(),
        };

        // Form a rule of 00/00/01/01/11/11/10/10
        let rul2 = SomeRule {
            b00: SomeMask::_from_string(1, "m11000000").unwrap(),
            b01: SomeMask::_from_string(1, "m00110000").unwrap(),
            b11: SomeMask::_from_string(1, "m00001100").unwrap(),
            b10: SomeMask::_from_string(1, "m00000011").unwrap(),
        };

        // Form an invalid rule with the union of:
        // X1/Xx/X0/XX/X0/Xx/X1/XX
        // 00/00/01/01/11/11/10/10
        let rul3 = rul1.union(&rul2);

        let rul4 = rul3.valid_subset().unwrap();

        // Form a rule of 11/10/10/11/00/01/01/00
        let rul5 = SomeRule {
            b00: SomeMask::_from_string(1, "m00001001").unwrap(),
            b01: SomeMask::_from_string(1, "m00000110").unwrap(),
            b11: SomeMask::_from_string(1, "m10010000").unwrap(),
            b10: SomeMask::_from_string(1, "m01100000").unwrap(),
        };

        // From the invalid union of:
        // X1/Xx/X0/XX/X0/Xx/X1/XX
        // 00/00/01/01/11/11/10/10
        // Should get the following valid part.
        // 11/10/10/11/00/01/01/00
        if rul4 == rul5 {
            return Ok(());
        }

        return Err(format!("subset is? {}", rul4.formatted_string()));
    }

    // Test action:get_needs StateNotInGroup, two flavors.
    #[test]
    fn need_for_state_not_in_group() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        // Check need for the current state not in a group.
        let nds1 = dm0.actions.avec[0].state_not_in_group_needs(&dm0.cur_state);

        assert!(nds1.len() == 1);
        assert!(nds1._contains_similar_need("StateNotInGroup", &dm0.region_from_string("r1").unwrap()));

        // Create group for one sample
        let s1 = dm0.state_from_string("s1").unwrap();
        dm0.eval_sample_arbitrary(0, &s1, &s1);

        if let Some(_grpx) = dm0.actions[0].groups.find(&dm0.region_from_string("r1").unwrap()) {
        } else {
            return Err("Group r1 not found ??".to_string());
        }

        // Invalidate group for sample 1 by giving it GT 1 different result.
        // Current state changes to zero.
        let s1 = dm0.state_from_string("s1").unwrap();
        dm0.eval_sample_arbitrary(0, &s1, &s1.toggle_bits(vec![0]));
        
        if let Some(_grpx) = dm0.actions[0].groups.find(&dm0.region_from_string("r1").unwrap()) {
            return Err("Group r1  found ??".to_string());
        }

        // Check needs for pn > 1 and not in group, and current state not in a group.
        let nds1 = dm0.get_needs();

        assert!(nds1.len() == 2);
        assert!(nds1._contains_similar_need("StateNotInGroup", &dm0.region_from_string("r1").unwrap()));
        assert!(nds1._contains_similar_need("StateNotInGroup", &dm0.region_from_string("r0").unwrap()));

        Ok(())
    }

    // Test additional_group_state_samples.
    #[test]
    fn need_additional_group_state_samples()  -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        // Check need for the current state not in a group.
        let nds1 = dm0.actions.avec[0].state_not_in_group_needs(&dm0.cur_state);

        assert!(nds1.len() == 1);
        assert!(nds1._contains_similar_need("StateNotInGroup", &dm0.region_from_string("r1").unwrap()));

        // Create group for one sample
        let s1 = dm0.state_from_string("s1").unwrap();
        dm0.eval_sample_arbitrary(0, &s1, &s1);

        if let Some(_grpx) = dm0.actions[0].groups.find(&dm0.region_from_string("r1").unwrap()) {
        } else {
            return Err("Group r1 not found ??".to_string());
        }

       // Expand group
        let s2 = dm0.state_from_string("s10").unwrap();
        dm0.eval_sample_arbitrary(0, &s2, &s2);

        if let Some(_grpx) = dm0.actions[0].groups.find(&dm0.region_from_string("rXX").unwrap()) {
        } else {
            return Err("Group rXX not found ??".to_string());
        }
        
        let nds2 = dm0.actions[0].additional_group_state_samples();
        //println!("needs {}", nds2);

        assert!(nds2.len() == 2);
        assert!(nds2._contains_similar_need("StateAdditionalSample", &dm0.region_from_string("r1").unwrap()));
        assert!(nds2._contains_similar_need("StateAdditionalSample", &dm0.region_from_string("r10").unwrap()));

        // Satisfy one need.
        dm0.eval_sample_arbitrary(0, &s2, &s2);
        
        let nds3 = dm0.actions[0].additional_group_state_samples();
        //println!("needs {}", nds3);

        assert!(nds3.len() == 1);
        assert!(nds3._contains_similar_need("StateAdditionalSample", &dm0.region_from_string("r1").unwrap()));

        // Satisfy second need.
        dm0.eval_sample_arbitrary(0, &s1, &s1);

        let nds4 = dm0.actions[0].additional_group_state_samples();
        //println!("needs {}", nds4);

        // Check for no more needs.
        assert!(nds4.len() == 0);

        Ok(())
    }

    // Test Seek adjacent overlapping region.
    // Create region X10X and adjacent region 1X1X.
    // The overlapping part is region 11X1, with no squares in it.
    // Should seek samples of the high (f) and low (d) squares of region 11X1.
    // After two samples of each, it should produce the AddGroup need for 11X1.
    #[test]
    fn need_for_samples_in_adjacent_overlapping_part() -> Result<(), String> {

        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        let s5 = dm0.state_from_string("s101").unwrap();

        let s7 = dm0.state_from_string("s111").unwrap();

        let s9 = dm0.state_from_string("s1001").unwrap();

        let sb = dm0.state_from_string("s1011").unwrap();

        let sc = dm0.state_from_string("s1100").unwrap();

        let sd = dm0.state_from_string("s1101").unwrap();

        let se = dm0.state_from_string("s1110").unwrap();

        let sf = dm0.state_from_string("s1111").unwrap();

        let targ_region = dm0.region_from_string("r11X1").unwrap();

        // Create group for region X10X.
        dm0.eval_sample_arbitrary(0, &s5, &s5.toggle_bits(vec![0]));
        dm0.eval_sample_arbitrary(0, &s5, &s5.toggle_bits(vec![0]));

        dm0.eval_sample_arbitrary(0, &sc, &sc.toggle_bits(vec![0]));
        dm0.eval_sample_arbitrary(0, &sc, &sc.toggle_bits(vec![0]));

        // Add square to prevent squares (sc, s5) combining with squares (se, sb).
        dm0.eval_sample_arbitrary(0, &s7, &s7.toggle_bits(vec![1]));
        dm0.eval_sample_arbitrary(0, &s7, &s7.toggle_bits(vec![1]));

        // Add square to prevent squares (sc, s5) combining with squares (se, sb).
        dm0.eval_sample_arbitrary(0, &s9, &s9.toggle_bits(vec![2,1,0]));
        dm0.eval_sample_arbitrary(0, &s9, &s9.toggle_bits(vec![2,1,0]));

        // Create group for region 1X1X.
        dm0.eval_sample_arbitrary(0, &se, &se);
        dm0.eval_sample_arbitrary(0, &se, &se);

        dm0.eval_sample_arbitrary(0, &sb, &sb.toggle_bits(vec![0]));
        dm0.eval_sample_arbitrary(0, &sb, &sb.toggle_bits(vec![0]));

        // Get first needs.
        let nds1 = dm0.actions.avec[0].group_pair_needs();

        // Check for two needs, targets f and d.
        assert!(nds1.len() == 2);
        assert!(nds1._contains_similar_need("AStateMakeGroup", &SomeRegion::new(&sf, &sf)));
        assert!(nds1._contains_similar_need("AStateMakeGroup", &SomeRegion::new(&sd, &sd)));

        dm0.eval_sample_arbitrary(0, &sf, &sf.toggle_bits(vec![0]));
        
        let nds2 = dm0.actions.avec[0].group_pair_needs();

        assert!(nds2.len() == 1);
        assert!(nds1._contains_similar_need("AStateMakeGroup", &SomeRegion::new(&sd, &sd)));

        dm0.eval_sample_arbitrary(0, &sd, &sd.toggle_bits(vec![0]));

        let nds3 = dm0.actions.avec[0].group_pair_needs();

        assert!(nds3.len() == 2);
        assert!(nds3._contains_similar_need("StateAdditionalSample", &SomeRegion::new(&sd, &sd)));
        assert!(nds3._contains_similar_need("StateAdditionalSample", &SomeRegion::new(&sf, &sf)));
        
        dm0.eval_sample_arbitrary(0, &sf, &sf.toggle_bits(vec![0]));

        let nds4 = dm0.actions.avec[0].group_pair_needs();

        assert!(nds4.len() == 1);
        assert!(nds4._contains_similar_need("StateAdditionalSample", &SomeRegion::new(&sd, &sd)));

        dm0.eval_sample_arbitrary(0, &sd, &sd.toggle_bits(vec![0]));

        let nds5 = dm0.actions.avec[0].group_pair_needs();

        assert!(nds5.len() == 1);
        let ned7 = SomeNeed::AddGroup { group_region: targ_region.clone(), };
        assert!(nds5.contains(&ned7));

        Ok(())
    }

    // Seek a part of a group intersection that is contradictory.
    // Group X1XX intersects group XX0X at X10X.
    // The intersection is not wholly contradictory, as would be expected due to
    // the two groups being defined by a square, D, in X10X.
    // The region X100 (4, C) in X10X is the contradictory part due to 
    // different results expected from the least significant bit.
    #[test]
    fn need_for_sample_in_contradictory_intersection() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();
        
        let s0 = dm0.state_from_string("s0").unwrap();
        let s6 = dm0.state_from_string("s110").unwrap();
        let sd = dm0.state_from_string("s1101").unwrap();

        // Create group for region XX0X.
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![0]));
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![0]));

        dm0.eval_sample_arbitrary(0, &sd, &sd);
        dm0.eval_sample_arbitrary(0, &sd, &sd);

        // Create group X1XX
        dm0.eval_sample_arbitrary(0, &s6, &s6);
        dm0.eval_sample_arbitrary(0, &s6, &s6);

        // Get and check needs.
        let nds1 = dm0.actions.avec[0].group_pair_needs();
        assert!(nds1.len() == 1);
        assert!(nds1._contains_similar_need("ContradictoryIntersection", &dm0.region_from_string("rX100").unwrap()));

        Ok(())
    }

    #[test]
    fn test_limit_group_needs() -> Result<(), String> {
        // Init domain with one action.
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        // Set up two groups.
        // Changes for each sample set up two, and only two groups.
        // Changes for bit three insure the seeking external dissimilar squares
        // that differ by the third bit.  Like D and 5, 0 and 8.
        let s0 = dm0.state_from_string("s0").unwrap();
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![3]));
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![3]));

        let s7 = dm0.state_from_string("s111").unwrap();
        dm0.eval_sample_arbitrary(0, &s7, &s7.toggle_bits(vec![3]));
        dm0.eval_sample_arbitrary(0, &s7, &s7.toggle_bits(vec![3]));

        let sd = dm0.state_from_string("s1101").unwrap();
        dm0.eval_sample_arbitrary(0, &sd, &sd.toggle_bits(vec![0,1,3]));
        dm0.eval_sample_arbitrary(0, &sd, &sd.toggle_bits(vec![0,1,3]));

        let sa = dm0.state_from_string("s1010").unwrap();
        dm0.eval_sample_arbitrary(0, &sa, &sa.toggle_bits(vec![0,1,3]));
        dm0.eval_sample_arbitrary(0, &sa, &sa.toggle_bits(vec![0,1,3]));

        println!("dm0 {}", &dm0.actions[0]);

        // Directly run limit_groups_needs.
        let agg_chg = SomeChange { b01: SomeMask::_from_string(1, "m1111").unwrap(), b10: SomeMask::_from_string(1, "m1111").unwrap() };
        let nds1 = dm0.actions[0].limit_groups_needs(&agg_chg);

        // Check for needs of adjacent, external, squares to 0 (8), 7 (F), A (2) , D (5).
        println!("needs are {}", nds1);
        assert!(nds1.len() == 2);
        assert!(nds1._contains_similar_need("LimitGroup", &dm0.region_from_string("r1111").unwrap()) || nds1._contains_similar_need("LimitGroup", &dm0.region_from_string("r1000").unwrap()));
        assert!(nds1._contains_similar_need("LimitGroup", &dm0.region_from_string("r101").unwrap()) || nds1._contains_similar_need("LimitGroup", &dm0.region_from_string("r10").unwrap()));

        // Start homing in with sample of 5, adjacent, external, to D in 1XXX.
        let s5 = dm0.state_from_string("s101").unwrap();
        dm0.eval_sample_arbitrary(0, &s5, &s5.toggle_bits(vec![3]));
        let nds2 = dm0.get_needs();

        // Check for second need for 5, to reach pnc for 5.
        assert!(nds2.len() == 2);
        assert!(nds2._contains_similar_need("LimitGroup", &dm0.region_from_string("r101").unwrap()));

        // Get second sample for 5.
        dm0.eval_sample_arbitrary(0, &s5, &s5.toggle_bits(vec![3]));
        let nds3 = dm0.get_needs();

        // Check for need of square 10, far from square 5, in 0XXX.
        assert!(nds3.len() == 1);
        assert!(nds3._contains_similar_need("LimitGroup", &dm0.region_from_string("r10").unwrap()));

        // Get sample of 2, far from 5 in 0XXX.
        // In 1XXX, A is already far from D, and is pnc, so no further needs for 1XXX.
        let s2 = dm0.state_from_string("s10").unwrap();
        dm0.eval_sample_arbitrary(0, &s2, &s2.toggle_bits(vec![3]));
        let nds4 = dm0.get_needs();

        // Check for need of second sample of square 10, to reach pnc.
        assert!(nds4.len() == 1);
        assert!(nds4._contains_similar_need("LimitGroup", &dm0.region_from_string("r10").unwrap()));

        // Take second sample of square 10.
        dm0.eval_sample_arbitrary(0, &s2, &s2.toggle_bits(vec![3]));
        let nds5 = dm0.get_needs();

        // The two groups, 0XXX and 1XXX, should be limited, and have no further needs.
        assert!(nds5.len() == 0);

        Ok(())
    }

    // For showing something easily understandable, the groups in the program are shown
    // with four, or fewer, edges.
    // It is important to show that any arbitrary number of edges can form a group / rule.
    #[test]
    fn create_group_rule_with_ten_edges() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 2, "s1", RegionStore::new());
        dm0.add_action();
        
        let s0 = dm0.state_from_string("s0001010010101000").unwrap();
        let s1 = dm0.state_from_string("s1111010110101011").unwrap();
        // Region                        XXX1010X101010XX.

        // Create group for region XXX1010X101010XX.
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![4]));
        dm0.eval_sample_arbitrary(0, &s1, &s1.toggle_bits(vec![4]));

        if let Some(_grpx) = dm0.actions[0].groups.find(&dm0.region_from_string("rXXX1010X101010XX").unwrap()) {
        } else {
            return Err("Group rXXX1010X101010XX not found ??".to_string());
        }

        Ok(())
    }

    // Test a simple four-step plan to change the domain current state 
    // from s0111 to s1000.
    #[test]
    fn make_plan_direct() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();

        let s0 = dm0.state_from_string("s0").unwrap();
        let sf = dm0.state_from_string("s1111").unwrap();

        // Create group for region XXXX, Act 0.
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![0]));
        dm0.eval_sample_arbitrary(0, &sf, &sf.toggle_bits(vec![0]));

        // Create group for region XXXX, Act 1.
        dm0.eval_sample_arbitrary(1, &s0, &s0.toggle_bits(vec![1]));
        dm0.eval_sample_arbitrary(1, &sf, &sf.toggle_bits(vec![1]));

        // Create group for region XXXX, Act 2.
        dm0.eval_sample_arbitrary(2, &s0, &s0.toggle_bits(vec![2]));
        dm0.eval_sample_arbitrary(2, &sf, &sf.toggle_bits(vec![2]));

        // Create group for region XXXX, Act 3.
        dm0.eval_sample_arbitrary(3, &s0, &s0.toggle_bits(vec![3]));
        dm0.eval_sample_arbitrary(3, &sf, &sf.toggle_bits(vec![3]));    // Last sample changes current state to s0111

        // Get plan for 7 to 8
        dm0.set_cur_state(&dm0.state_from_string("s111").unwrap());
        let mut toreg = dm0.region_from_string("r1000").unwrap();
        if let Some(aplan) = dm0.make_plan(&toreg) {
            assert!(aplan.len() == 4);
            assert!(*aplan.result_region() == toreg);
        } else {
            return Err("no plan found to r1000?".to_string());
        }

        // Get plan for 8 to 7
        dm0.set_cur_state(&dm0.state_from_string("s1000").unwrap());
        toreg = dm0.region_from_string("r111").unwrap();
        if let Some(aplan) = dm0.make_plan(&toreg) {
            assert!(aplan.len() == 4);
            assert!(*aplan.result_region() == toreg);
        } else {
            return Err("no plan found to r111?".to_string());
        }

        Ok(())
    }
    
    // Test asymmetric chaining.  The plan must step out of the direct
    // glide path X1XX, between 7 and C, into X0XX, to change the third bit,
    // then step back into the glide path to get to the goal.
    #[test]
    fn make_plan_asymmetric() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();

        let s0 = dm0.state_from_string("s0").unwrap();
        let sf = dm0.state_from_string("s1111").unwrap();
        let sb = dm0.state_from_string("s1011").unwrap();

        // Create group for region XXXX, Act 0.
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![0]));
        dm0.eval_sample_arbitrary(0, &sf, &sf.toggle_bits(vec![0]));

        // Create group for region XXXX, Act 1.
        dm0.eval_sample_arbitrary(1, &s0, &s0.toggle_bits(vec![1]));
        dm0.eval_sample_arbitrary(1, &sf, &sf.toggle_bits(vec![1]));

        // Create group for region XXXX, Act 2.
        dm0.eval_sample_arbitrary(2, &s0, &s0.toggle_bits(vec![2]));
        dm0.eval_sample_arbitrary(2, &sf, &sf.toggle_bits(vec![2]));

        // Create group for region X0XX, Act 3.
        dm0.eval_sample_arbitrary(3, &s0, &s0.toggle_bits(vec![3]));
        dm0.eval_sample_arbitrary(3, &sb, &sb.toggle_bits(vec![3]));

        // Get plan for 7 to C
        dm0.set_cur_state(&dm0.state_from_string("s111").unwrap());
        let mut toreg = dm0.region_from_string("r1100").unwrap();
        if let Some(aplan) = dm0.make_plan(&toreg) {
            assert!(aplan.len() == 5);
            assert!(*aplan.result_region() == toreg);
        } else {
            return Err("No plan found to r1100?".to_string());
        }

        // Get plan for C to 7
        dm0.set_cur_state(&dm0.state_from_string("s1100").unwrap());
        toreg = dm0.region_from_string("r111").unwrap();
        if let Some(aplan) = dm0.make_plan(&toreg) {
            assert!(aplan.len() == 5);
            assert!(*aplan.result_region() == toreg);
        } else {
            return Err("No plan found to r111?".to_string());
        }

        Ok(())
    }

    // Check ordering of Truth values.
    // Should be F < M < T
    #[test]
    fn truth_comparisons() -> Result<(), String> {

        assert!(Truth::T.partial_cmp(&Truth::T).unwrap() == Ordering::Equal);
        assert!(Truth::T.partial_cmp(&Truth::M).unwrap() == Ordering::Greater);
        assert!(Truth::T.partial_cmp(&Truth::F).unwrap() == Ordering::Greater);

        assert!(Truth::M.partial_cmp(&Truth::T).unwrap() == Ordering::Less);
        assert!(Truth::M.partial_cmp(&Truth::M).unwrap() == Ordering::Equal);
        assert!(Truth::M.partial_cmp(&Truth::F).unwrap() == Ordering::Greater);

        assert!(Truth::F.partial_cmp(&Truth::T).unwrap() == Ordering::Less);
        assert!(Truth::F.partial_cmp(&Truth::M).unwrap() == Ordering::Less);
        assert!(Truth::F.partial_cmp(&Truth::F).unwrap() == Ordering::Equal);

        Ok(())
    }

    // Check ordering of Pn values.
    // Should be One < Two < Unpredictable
    #[test]
    fn pn_comparisons() -> Result<(), String> {

        assert!(Pn::Unpredictable.partial_cmp(&Pn::Unpredictable).unwrap() == Ordering::Equal);
        assert!(Pn::Unpredictable.partial_cmp(&Pn::Two).unwrap() == Ordering::Greater);
        assert!(Pn::Unpredictable.partial_cmp(&Pn::One).unwrap() == Ordering::Greater);

        assert!(Pn::Two.partial_cmp(&Pn::Unpredictable).unwrap() == Ordering::Less);
        assert!(Pn::Two.partial_cmp(&Pn::Two).unwrap() == Ordering::Equal);
        assert!(Pn::Two.partial_cmp(&Pn::One).unwrap() == Ordering::Greater);

        assert!(Pn::One.partial_cmp(&Pn::Unpredictable).unwrap() == Ordering::Less);
        assert!(Pn::One.partial_cmp(&Pn::Two).unwrap() == Ordering::Less);
        assert!(Pn::One.partial_cmp(&Pn::One).unwrap() == Ordering::Equal);

        Ok(())
    }
} // end mod tests
