//! Tests for an Unorthodox Expert System.
//!

#[cfg(test)]
mod tests {
    use crate::bits::SomeBits;
    use crate::domain::SomeDomain;
    use crate::mask::SomeMask;
    use crate::maskstore::MaskStore;
    use crate::action::SomeAction;
    use crate::regionstore::RegionStore;
    use crate::resultstore::ResultStore;
    use crate::rule::SomeRule;
    use crate::rulestore::RuleStore;
    use crate::state::SomeState;
    use crate::statestore::StateStore;
    use crate::step::SomeStep;
    use crate::stepstore::StepStore;

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
    fn bits_string_length() -> Result<(), String> {
        let bts1 = SomeBits {
            ints: vec![129 as u8],
        };

        let n1 = bts1.formatted_string_length();
        let rs = bts1.formatted_string('b');
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeBits string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let bts2 = SomeBits {
            ints: vec![129 as u8, 18 as u8],
        };

        let n1 = bts2.formatted_string_length();
        let rs = bts2.formatted_string('b');
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of two int SomeBits string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        Ok(())
    }

    #[test]
    fn state_string_length() -> Result<(), String> {
        let sta1 = SomeState {
            bts: SomeBits {
                ints: vec![129 as u8],
            },
        };

        let n1 = sta1.formatted_string_length();
        let rs = sta1.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeState string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let sta2 = SomeState {
            bts: SomeBits {
                ints: vec![129 as u8, 18 as u8],
            },
        };

        let n1 = sta2.formatted_string_length();
        let rs = sta2.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of two int SomeState string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let mut stst = StateStore::new();

        let n1 = stst.formatted_string_length();
        let rs = stst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of empty StateStore string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        stst.push(SomeState {
            bts: SomeBits {
                ints: vec![12 as u8],
            },
        });
        let n1 = stst.formatted_string_length();
        let rs = stst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of StateStore len 1 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        stst.push(SomeState {
            bts: SomeBits {
                ints: vec![1 as u8],
            },
        });
        let n1 = stst.formatted_string_length();
        let rs = stst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of StateStore len 2 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }
        Ok(())
    }

    #[test]
    fn mask_string_length() -> Result<(), String> {
        let msk1 = SomeMask {
            bts: SomeBits {
                ints: vec![129 as u8],
            },
        };

        let n1 = msk1.formatted_string_length();
        let rs = msk1.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeMask string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let msk2 = SomeMask {
            bts: SomeBits {
                ints: vec![129 as u8, 18 as u8],
            },
        };

        let n1 = msk2.formatted_string_length();
        let rs = msk2.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of two int SomeMask string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let mut mkst = MaskStore { avec: vec![] };

        let n1 = mkst.formatted_string_length();
        let rs = mkst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of empty MaskStore string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        mkst = MaskStore {
            avec: vec![SomeMask {
                bts: SomeBits {
                    ints: vec![12 as u8],
                },
            }],
        };
        let n1 = mkst.formatted_string_length();
        let rs = mkst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of MaskStore len 1 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        mkst = MaskStore {
            avec: vec![
                SomeMask {
                    bts: SomeBits {
                        ints: vec![12 as u8],
                    },
                },
                SomeMask {
                    bts: SomeBits {
                        ints: vec![12 as u8],
                    },
                },
            ],
        };
        let n1 = mkst.formatted_string_length();
        let rs = mkst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of StateStore len 2 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        Ok(())
    }

    #[test]
    fn region_string_length() -> Result<(), String> {
        let dm1 = SomeDomain::new(2, "s1", "r1");

        let reg1 = dm1.region_from_string("rx1x1").unwrap();

        let n1 = reg1.formatted_string_length();
        let rs = reg1.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeRegion string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let reg2 = dm1.region_from_string("rx1x1").unwrap();

        let n1 = reg2.formatted_string_length();
        let rs = reg2.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeRegion string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let mut regt = RegionStore { avec: vec![] };

        let n1 = regt.formatted_string_length();
        let rs = regt.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of empty RegionStore string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        regt = RegionStore {
            avec: vec![dm1.region_from_string("rx1x1").unwrap()],
        };
        let n1 = regt.formatted_string_length();
        let rs = regt.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of RegionStore len 1 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        regt = RegionStore {
            avec: vec![
                dm1.region_from_string("rx1x1").unwrap(),
                dm1.region_from_string("rx1x1").unwrap(),
            ],
        };
        let n1 = regt.formatted_string_length();
        let rs = regt.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of RegionStore len 2 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        Ok(())
    }

    #[test]
    fn step_string_length() -> Result<(), String> {
        let dm1 = SomeDomain::new(1, "s1", "r1");

        let stp1 = SomeStep {
            act_num: 0,
            initial: dm1.region_from_string("r1011").unwrap(),
            result: dm1.region_from_string("r1010").unwrap(),
            group_reg: dm1.region_from_string("rx0x1").unwrap(),
            rule: SomeRule::new(
                &SomeState {
                    bts: SomeBits {
                        ints: vec![11 as u8],
                    },
                },
                &SomeState {
                    bts: SomeBits {
                        ints: vec![10 as u8],
                    },
                },
            ),
            alt_rule: false,
        };

        let n1 = stp1.formatted_string_length();
        let rs = stp1.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeStep string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let dm2 = SomeDomain::new(2, "s1", "r1");

        let stp2 = SomeStep {
            act_num: 0,
            initial: dm2.region_from_string("r1011").unwrap(),
            result: dm2.region_from_string("r1010").unwrap(),
            group_reg: dm2.region_from_string("rx0x1").unwrap(),
            rule: SomeRule::new(
                &SomeState {
                    bts: SomeBits {
                        ints: vec![0 as u8, 11 as u8],
                    },
                },
                &SomeState {
                    bts: SomeBits {
                        ints: vec![0 as u8, 10 as u8],
                    },
                },
            ),
            alt_rule: false,
        };

        let n1 = stp2.formatted_string_length();
        let rs = stp2.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of two int SomeStep string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let stpt = StepStore { avec: vec![] };

        let n1 = stpt.formatted_string_length();
        let rs = stpt.formatted_string("");
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of empty StepStore string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let stpt = StepStore {
            avec: vec![stp2.clone()],
        };
        let n1 = stpt.formatted_string_length();
        let rs = stpt.formatted_string("");
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of StepStore len 1 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let stpt = StepStore {
            avec: vec![stp2.clone(), stp2.clone()],
        };
        let n1 = stpt.formatted_string_length();
        let rs = stpt.formatted_string("");
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of StepStore len 2 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        Ok(())
    }

    #[test]
    fn rule_string_length() -> Result<(), String> {
        let rul1 = SomeRule::new(
            &SomeState {
                bts: SomeBits {
                    ints: vec![0 as u8],
                },
            },
            &SomeState {
                bts: SomeBits {
                    ints: vec![1 as u8],
                },
            },
        );

        let n1 = rul1.formatted_string_length();
        let rs = rul1.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int SomeRule string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let rul2 = SomeRule::new(
            &SomeState {
                bts: SomeBits {
                    ints: vec![0 as u8, 1 as u8],
                },
            },
            &SomeState {
                bts: SomeBits {
                    ints: vec![2 as u8, 3 as u8],
                },
            },
        );

        let n1 = rul2.formatted_string_length();
        let rs = rul2.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of two int SomeRule string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let rult = RuleStore { avec: vec![] };

        let n1 = rult.formatted_string_length();
        let rs = rult.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of empty RuleStore string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let rult = RuleStore {
            avec: vec![rul2.clone()],
        };
        let n1 = rult.formatted_string_length();
        let rs = rult.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of RuleStore len 1 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        let rult = RuleStore {
            avec: vec![rul2.clone(), rul2.clone()],
        };
        let n1 = rult.formatted_string_length();
        let rs = rult.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of RuleStore len 2 string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        Ok(())
    }

    #[test]
    fn resultstore_string_length() -> Result<(), String> {
        let rsltst = ResultStore::new(SomeState {
            bts: SomeBits {
                ints: vec![0 as u8, 1 as u8],
            },
        });
        let n1 = rsltst.formatted_string_length();
        let rs = rsltst.formatted_string();
        let n2 = rs.len();
        if n1 != n2 {
            return Err(format!(
                "calculated len of one int ResultStore string {} NEQ real len {} {}",
                n1, n2, rs
            ));
        }

        Ok(())
    }

} // end mod tests
