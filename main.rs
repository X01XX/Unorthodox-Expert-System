//#![allow(dead_code)]
/*
 * Unorthodox Expert System
 *
 * Copyright 2021 Bitflogger <earl.dukerschein@wisc.edu>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 *
 */

/*
 * For use outside of the license, contact the Wisconsin Alumni Research Foundation (WARF).
 */

use std::env;
mod action;
mod actionstore;
mod bits;
use crate::bits::SomeBits;
mod group;
mod groupstore;
mod mask;
mod need;
mod tools;
use need::SomeNeed;
mod region;
use region::SomeRegion;
mod change;
use change::SomeChange;
mod regionscorr;
mod regionstore;
mod resultstore;
mod rule;
mod rulestore;
use rulestore::RuleStore;
mod sample;
mod square;
mod squarestore;
mod state;
use sample::SomeSample;
use state::SomeState;
mod domain;
mod needstore;
mod plan;
mod pn;
mod statescorr;
mod statestore;
use pn::Pn;
mod domainstore;
mod step;
mod stepstore;
use domainstore::{DomainStore, NeedPlan};
mod actioninterface;
mod planstore;
mod selectregions;
use crate::selectregions::SelectRegions;
mod planscorr;
mod planscorrstore;
mod regionscorrstore;
mod selectregionsstore;
mod target;
use crate::target::ATarget;

extern crate unicode_segmentation;

use std::io;
use std::io::{Read, Write};
extern crate rand;
use std::fs::File;
use std::path::Path;
use std::process;
use std::time::{Duration, Instant};

/// Get and react to arguments given.
fn main() {
    // Get arguments, if any.
    let args: Vec<String> = env::args().collect();
    //println!("{:?}", args);

    if args.len() == 1 {
        run_step_by_step();
        process::exit(0);
    }

    if args.len() == 2 {
        if args[1] == "h" || args[1] == "help" {
            usage();
            process::exit(0);
        }
        if let Ok(runs) = args[1].parse::<usize>() {
            match runs {
                0 => {
                    usage();
                    eprintln!("Did not understand {:?}", args);
                    process::exit(1);
                }
                1 => {
                    let mut dmxs = domainstore_init();
                    do_session_until_no_needs(&mut dmxs);
                }
                _ => {
                    run_number_times(runs);
                    process::exit(1);
                }
            }
        } else {
            // Run with arg[1] as file path.
            run_with_file(&args[1]);
        }
        process::exit(0);
    }

    usage();
    eprintln!("Did not understand {:?}", args);
    process::exit(1);
} // end main

/// Run with user input step by step.
fn run_step_by_step() {
    usage();
    // Generate needs, get can_do and cant_do need vectors.
    let mut dmxs = domainstore_init();

    dmxs.generate_and_display_needs();

    do_interactive_session(&mut dmxs);
}

/// Load data from a file, then run with user input, step by step.
fn run_with_file(file_path: &str) {
    // Init DomainStore or read in from file.
    let mut dmxs = match load_data(file_path) {
        Ok(new_dmxs) => new_dmxs,
        Err(errstr) => {
            eprintln!("main::run_with_file: {errstr}");
            return;
        }
    };

    // Display UI info.
    usage();

    // Display current state.
    dmxs.generate_and_display_needs();

    // Run with it.
    do_interactive_session(&mut dmxs);
}

/// Run until no more needs can be done, then take user input.
fn do_session_until_no_needs(dmxs: &mut DomainStore) {
    let start = Instant::now();
    dmxs.do_session();

    let duration = start.elapsed();
    println!(
        "Steps: {} Time elapsed: {:.2?} seconds",
        dmxs.step_num - 1,
        duration
    );

    do_interactive_session(dmxs);
}

/// Run a number of times without user input, generate aggregate data.
/// Return number failures, that is the number of seessions that ended with unsatisfied needs.
fn run_number_times(num_runs: usize) -> usize {
    let mut runs_left = num_runs;
    let mut cant_do = 0;
    let mut duration_vec = Vec::<Duration>::with_capacity(num_runs);
    let mut steps_vec = Vec::<usize>::with_capacity(num_runs);
    let mut num_groups_off = 0;

    while runs_left > 0 {
        runs_left -= 1;

        let start = Instant::now();
        let (steps, groups, expected, num_cant) = do_one_session();

        let duration = start.elapsed();
        println!(
            "Steps {steps}, Time elapsed in do_session() is: {duration:.2?} groups: {groups:?}"
        );
        duration_vec.push(duration);
        steps_vec.push(steps);
        if groups != expected {
            num_groups_off += 1
        }
        if num_cant > 0 {
            cant_do += 1;
        }
    }

    println!("Number with unsatisfied needs: {cant_do} Num groups off {num_groups_off}");

    let mut duration_total = Duration::new(0, 0);
    let mut duration_high = duration_vec[0];
    let mut duration_low = duration_vec[0];
    for durx in duration_vec.iter() {
        duration_total += *durx;
        if *durx > duration_high {
            duration_high = *durx;
        }
        if *durx < duration_low {
            duration_low = *durx;
        }
    }

    let mut steps_total = 0;
    let mut steps_high = 0;
    let mut steps_low = usize::MAX;
    for stepsx in steps_vec.iter() {
        steps_total += *stepsx;
        if *stepsx > steps_high {
            steps_high = *stepsx;
        }
        if *stepsx < steps_low {
            steps_low = *stepsx;
        }
    }
    let average_time = duration_total / duration_vec.len() as u32;
    let average_steps = steps_total / steps_vec.len();
    // let duration_minutes = duration_total.as_secs() as i32 / 60 as i32;
    let duration_minutes = duration_total / 60;

    println!("\nRuns {}, Average steps: {} high: {}, low: {}, Elapsed time: {:.2?} minutes, Average time elapsed: {:.2?}, high: {:.2?}, low: {:.2?} Number with unsatisfied needs {} Num groups off {}",
         num_runs, average_steps, steps_high, steps_low, duration_minutes, average_time, duration_high, duration_low, cant_do, num_groups_off);
    cant_do
}

/// Initialize a Domain Store, with two domains and 11 actions.
fn domainstore_init() -> DomainStore {
    // Start a DomainStore
    let mut dmxs = DomainStore::new();

    dmxs.add_domain(SomeState::new(SomeBits::new_random(5)));
    dmxs.add_domain(SomeState::new(SomeBits::new_random(16)));

    // Add actions 0 through 9 to Domain 0;
    let ruls0: Vec<RuleStore> = vec![
        RuleStore::from("[XX_11/XX/00/Xx]").expect("SNH"),
        RuleStore::from("[XX_00/XX/11/Xx]").expect("SNH"),
        RuleStore::from("[XX_XX/11/XX/10]").expect("SNH"),
        RuleStore::from("[XX_11/00/10/XX, XX_11/01/11/XX]").expect("SNH"),
        RuleStore::from("[XX_11/XX/10/00, XX_11/Xx/11/00]").expect("SNH"),
        RuleStore::from("[XX_00/00/00/Xx, XX_00/00/01/XX, XX_00/01/00/XX]").expect("SNH"),
        RuleStore::from("[XX_00/XX/00/01, XX_00/XX/01/00, XX_00/Xx/00/00]").expect("SNH"),
    ];
    dmxs[0].add_action(ruls0, 5);

    let ruls1: Vec<RuleStore> = vec![
        RuleStore::from("[XX_XX/Xx/11/XX]").expect("SNH"),
        RuleStore::from("[XX_XX/XX/01/XX]").expect("SNH"),
    ];
    dmxs[0].add_action(ruls1, 5);

    let ruls2: Vec<RuleStore> = vec![
        RuleStore::from("[XX_XX/XX/10/XX]").expect("SNH"),
        RuleStore::from("[XX_XX/Xx/00/XX]").expect("SNH"),
    ];
    dmxs[0].add_action(ruls2, 5);

    let ruls3: Vec<RuleStore> = vec![
        RuleStore::from("[Xx_11/XX/XX/XX]").expect("SNH"),
        RuleStore::from("[XX_01/XX/XX/XX]").expect("SNH"),
    ];
    dmxs[0].add_action(ruls3, 5);

    let ruls4: Vec<RuleStore> = vec![
        RuleStore::from("[XX_10/XX/XX/XX]").expect("SNH"),
        RuleStore::from("[Xx_00/XX/XX/XX]").expect("SNH"),
    ];
    dmxs[0].add_action(ruls4, 5);

    let ruls5: Vec<RuleStore> = vec![RuleStore::from("[XX_XX/XX/XX/XX]").expect("SNH")];
    dmxs[0].add_action(ruls5, 5);

    // Add actions 0 through 6 to domain 1.
    let ruls0: Vec<RuleStore> =
        vec![RuleStore::from("[XX/XX/XX/XX_XX/XX/XX/Xx_XX/XX/Xx/XX_XX/XX/XX/XX]").expect("SNH")];
    dmxs[1].add_action(ruls0, 5);

    let ruls1: Vec<RuleStore> =
        vec![RuleStore::from("[XX/XX/XX/XX_XX/XX/XX/Xx_XX/Xx/XX/XX_XX/XX/XX/XX]").expect("SNH")];
    dmxs[1].add_action(ruls1, 5);

    let ruls2: Vec<RuleStore> =
        vec![RuleStore::from("[XX/XX/XX/XX_XX/XX/XX/Xx_Xx/XX/XX/XX_XX/XX/XX/XX]").expect("SNH")];
    dmxs[1].add_action(ruls2, 5);

    let ruls3: Vec<RuleStore> =
        vec![RuleStore::from("[XX/XX/XX/XX_XX/XX/XX/Xx_XX/XX/XX/XX_XX/XX/XX/XX]").expect("SNH")];
    dmxs[1].add_action(ruls3, 5);

    let ruls4: Vec<RuleStore> =
        vec![RuleStore::from("[XX/XX/XX/XX_XX/XX/Xx/XX_XX/XX/XX/XX_XX/XX/XX/XX]").expect("SNH")];
    dmxs[1].add_action(ruls4, 5);

    let ruls5: Vec<RuleStore> =
        vec![RuleStore::from("[XX/XX/XX/XX_XX/XX/Xx/XX_XX/XX/XX/XX_XX/XX/XX/XX]").expect("SNH")];
    dmxs[1].add_action(ruls5, 5);

    let ruls6: Vec<RuleStore> = vec![
        RuleStore::from("[XX/XX/XX/XX_XX/XX/XX/Xx_XX/XX/11/XX_XX/XX/XX/XX]").expect("SNH"),
        RuleStore::from("[XX/XX/XX/XX_XX/XX/Xx/XX_XX/11/00/XX_XX/XX/XX/XX]").expect("SNH"),
        RuleStore::from("[XX/XX/XX/XX_XX/Xx/XX/XX_XX/00/00/XX_XX/XX/XX/XX]").expect("SNH"),
    ];
    dmxs[1].add_action(ruls6, 5);

    // Add select regions.
    dmxs.add_select(SelectRegions::from("SR[RC[rx0x0x, rXXXX_XX1X_1XXX_XXXX], 3]").expect("SNH"));

    dmxs.add_select(SelectRegions::from("SR[RC[rx0xx1, rXXXX_XXX1_1XXX_XXXX], 2]").expect("SNH"));

    dmxs.add_select(SelectRegions::from("SR[RC[rxx1x1, rXXXX_XX00_0XXX_XXXX], 3]").expect("SNH"));

    dmxs.add_select(SelectRegions::from("SR[RC[rx1110, rXXXX_XXX0_0XXX_XXXX], 1]").expect("SNH"));

    dmxs.add_select(SelectRegions::from("SR[RC[rxXX00, rXXXX_XXx1_0xXX_XXXX], -1]").expect("SNH"));

    dmxs.add_select(SelectRegions::from("SR[RC[rxX10X, rXXXX_XX1x_x0XX_XXXX], -1]").expect("SNH"));

    dmxs.calc_select();

    dmxs
}

/// Do one session to end.
/// Return the number steps taken get to the point where
/// there are no more needs.
/// Return error if no needs that can be done are available and
/// there are needs than cannot be done.
fn do_one_session() -> (usize, usize, usize, usize) {
    let mut dmxs = domainstore_init();

    dmxs.do_session();

    (
        dmxs.step_num,
        dmxs.number_groups(),
        dmxs.number_groups_expected(),
        dmxs.cant_do.len(),
    )
}

/// Do a session, step by step, taking user commands.
pub fn do_interactive_session(dmxs: &mut DomainStore) {
    loop {
        command_loop(dmxs);

        // Generate needs, get can_do and cant_do need vectors.
        dmxs.generate_and_display_needs();
    } // end loop
}

/// Do command loop.
fn command_loop(dmxs: &mut DomainStore) {
    //println!("start command loop");
    loop {
        let mut cmd = Vec::<&str>::with_capacity(10);

        let guess = pause_for_input("\nPress Enter or type a command: ");

        for word in guess.split_whitespace() {
            cmd.push(word);
        }

        // Default command, just press Enter
        if cmd.is_empty() {
            // Process needs
            if dmxs.can_do.is_empty() {
            } else {
                do_any_need(dmxs);
            }
            return;
        }

        // Do commands
        match cmd[0] {
            "aj" => {
                if cmd.len() == 2 {
                    match display_action_anchor_info(dmxs, &cmd) {
                        Ok(()) => continue,
                        Err(error) => {
                            println!("{error}");
                        }
                    }
                } else if cmd.len() == 3 {
                    match display_group_anchor_info(dmxs, &cmd) {
                        Ok(()) => continue,
                        Err(error) => {
                            println!("{error}");
                        }
                    }
                } else {
                    println!("Too many args for the aj command.");
                }
            }
            "cd" => match do_change_domain(dmxs, &cmd) {
                Ok(()) => {
                    dmxs.print_domain();
                    dmxs.display_needs();
                }
                Err(error) => {
                    println!("{error}");
                }
            },
            "cs" => match do_change_state_command(dmxs, &cmd) {
                Ok(()) => return,
                Err(error) => {
                    println!("{error}");
                }
            },
            "dcs" => {
                dmxs.print_domain();
                dmxs.print_can_do();
            }
            "dn" => match do_chosen_need(dmxs, &cmd) {
                Ok(()) => return,
                Err(error) => {
                    println!("{error}");
                }
            },
            "exit" | "q" | "quit" => {
                println!("Done");
                process::exit(0);
            }
            "fsd" => match store_data(dmxs, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                }
            },
            "gnds" => {
                let dom_id = dmxs.current_domain;
                dmxs[dom_id].get_needs();
                dmxs.print_domain();
                continue;
            }
            "gps" => match do_print_group_defining_squares_command(dmxs, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                }
            },
            "h" => usage(),
            "help" => usage(),
            "ppd" => match do_print_plan_details(dmxs, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                }
            },
            "ps" => match do_print_squares_command(dmxs, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                }
            },
            "psr" => match do_print_select_regions(dmxs, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                }
            },
            "run" => {
                if dmxs.can_do.is_empty() {
                } else {
                    dmxs.do_session();
                }
            }
            "ss" => match do_sample_state_command(dmxs, &cmd) {
                Ok(()) => return,
                Err(error) => {
                    println!("{error}");
                }
            },
            "to" => match do_to_region_command(dmxs, &cmd) {
                Ok(()) => return,
                Err(error) => {
                    println!("{error}");
                }
            },
            _ => {
                println!("\nDid not understand command: {cmd:?}");
            }
        };
    } // end loop
} // end command_loop

/// Change the domain to a number given by user.
fn do_change_domain(dmxs: &mut DomainStore, cmd: &[&str]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one number argument is needed for the cd command.".to_string());
    }
    // Get domain number from string
    match dmxs.domain_id_from_string(cmd[1]) {
        Ok(d_id) => {
            dmxs.change_domain(d_id);
            Ok(())
        }
        Err(error) => Err(error),
    } // end match
}

/// Choose a need from a number of possibilities.
/// Attempt to satisfy the chosen need.
fn do_any_need(dmxs: &mut DomainStore) {
    let np_inx = dmxs.choose_a_need();

    if dmxs.do_a_need(dmxs.can_do[np_inx].clone()) {
        println!("Need satisfied");
    }
}

/// Print details of a given plan
fn do_print_plan_details(dmxs: &DomainStore, cmd: &[&str]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one need-number argument is needed for the ppd command.".to_string());
    }
    match cmd[1].parse::<usize>() {
        Ok(n_num) => {
            if n_num >= dmxs.can_do.len() {
                Err(format!("Invalid Need Number: {}", cmd[1]))
            } else {
                let ndx = &dmxs.needs[dmxs.can_do[n_num].inx];

                println!("\n{} Need: {}", n_num, ndx);
                match &dmxs.can_do[n_num].plans {
                    NeedPlan::AtTarget {} => println!("AT?"),
                    NeedPlan::PlanFound { plan: plnx } => dmxs.print_planscorrstore_detail(plnx),
                }
                Ok(())
            }
        }
        Err(error) => Err(error.to_string()),
    }
}

/// Try to satisfy a need chosen by the user.
fn do_chosen_need(dmxs: &mut DomainStore, cmd: &[&str]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one need-number argument is needed for the dn command.".to_string());
    }

    let dom_id = dmxs.current_domain;

    match cmd[1].parse::<usize>() {
        Ok(n_num) => {
            if n_num >= dmxs.can_do.len() {
                Err(format!("Invalid Need Number: {}", cmd[1]))
            } else {
                let nd_inx = dmxs.can_do[n_num].inx;

                println!(
                    "\nNeed chosen: {:2} {} {}",
                    n_num,
                    dmxs.needs[nd_inx],
                    match &dmxs.can_do[n_num].plans {
                        NeedPlan::AtTarget {} => "At Target".to_string(),
                        NeedPlan::PlanFound { plan: plnsx } => plnsx.str_terse(),
                    }
                );

                match dmxs.needs[nd_inx] {
                    SomeNeed::ToSelectRegion { .. } => (),
                    SomeNeed::ExitSelectRegion { .. } => (),
                    _ => {
                        if dom_id != dmxs.needs[nd_inx].dom_id().unwrap() {
                            dmxs.change_domain(dmxs.needs[nd_inx].dom_id().unwrap());
                            dmxs.print_domain();
                        }
                    }
                }

                if dmxs.do_a_need(dmxs.can_do[n_num].clone()) {
                    println!("Need satisfied");
                }

                Ok(())
            }
        }
        Err(error) => Err(format!("{error}")),
    }
}

/// Do a change-state command.
fn do_change_state_command(dmxs: &mut DomainStore, cmd: &[&str]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one state argument is needed for the cs command.".to_string());
    }
    // Get state from string
    match SomeState::from(cmd[1]) {
        Ok(a_state) => {
            println!("Changed state to {a_state}");
            dmxs.set_cur_state(a_state);
            Ok(())
        }
        Err(error) => Err(format!("\nDid not understand state, {error}")),
    } // end match
}

/// Do to-region command.
fn do_to_region_command(dmxs: &mut DomainStore, cmd: &[&str]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one region argument is needed for the to command.".to_string());
    }

    // Get region from string
    let goal_region = SomeRegion::from(cmd[1])?;

    let dom_id = dmxs.current_domain;
    let dmx = &mut dmxs[dom_id];

    if goal_region.num_bits() != dmx.num_bits() {
        return Err(format!(
            "Region does not have the same number of bits, {}, as the CCD, {}.",
            goal_region.num_bits(),
            dmx.num_bits()
        ));
    }

    let cur_state = dmx.current_state();

    let needed_change = SomeChange::new_state_to_region(cur_state, &goal_region);
    println!(
        "\nChange Current_state {cur_state}\n           to region {goal_region} num bit changes needed {}\n                 b01 {}\n                 b10 {}",
        needed_change.number_changes(),
        needed_change.b01, needed_change.b10
    );

    if goal_region.is_superset_of(cur_state) {
        println!(
            "\nCurrent_state {} is already in region {}",
            dmx.cur_state, goal_region
        );
        return Ok(());
    }

    let cur_region = SomeRegion::new(vec![cur_state.clone()]);

    for _ in 0..6 {
        println!("\nCalculating plan.");
        match dmxs.plan_using_least_negative_select_regions_for_target(
            Some(dom_id),
            &ATarget::Region {
                region: &goal_region,
            },
        ) {
            Ok(planx) => {
                match planx {
                    NeedPlan::AtTarget {} => println!("At Target"),
                    NeedPlan::PlanFound { plan: plnx } => {
                        println!("{}", plnx.str_terse());
                        println!("\nrunning plan:");
                        match dmxs.run_planscorrstore(&plnx) {
                            Ok(num) => {
                                if num == 1 {
                                    println!("{num} step run.")
                                } else {
                                    println!("{num} steps run.")
                                }
                            }
                            Err(errstr) => println!("{errstr}"),
                        }
                        break;
                    }
                };
            }
            Err(errvec) => println!("{:?}", errvec),
        }
    }
    if cur_region.is_superset_of(&dmxs[dom_id].cur_state) {
        println!("\nNo plan to get from {cur_region} to {goal_region}");
    }
    if goal_region.is_superset_of(&dmxs[dom_id].cur_state) {
        println!("\nPlan succeeded");
    } else {
        println!("\nPlan failed");
    }

    pause_for_input("\nPress Enter to continue: ");

    Ok(())
}

/// Do sample-state command.
fn do_sample_state_command(dmxs: &mut DomainStore, cmd: &Vec<&str>) -> Result<(), String> {
    if cmd.len() == 1 {
        return Err("Action number is needed for the ss command.".to_string());
    }

    let act_id = match cmd[1].parse::<usize>() {
        Ok(act_id) => act_id,
        Err(error) => {
            return Err(error.to_string());
        }
    };

    if cmd.len() == 2 {
        let dom_id = dmxs.current_domain;
        let dmx = &mut dmxs[dom_id];

        println!("Act {act_id} sample curent state.");
        dmx.take_action_arbitrary(act_id);
        return Ok(());
    }

    if cmd.len() == 3 {
        // Get state from string
        let a_state = match SomeState::from(cmd[2]) {
            Ok(a_state) => a_state,
            Err(error) => {
                return Err(error);
            }
        };

        let dom_id = dmxs.current_domain;

        let dmx = &mut dmxs[dom_id];

        if a_state.num_bits() != dmx.num_bits() {
            return Err("State does not have the same number of bits as the CCD.".to_string());
        }

        println!("Act {act_id} sample State {a_state}");
        dmx.set_cur_state(a_state);
        dmx.take_action_arbitrary(act_id);
        return Ok(());
    }

    if cmd.len() == 4 {
        // Take arbitrary sample with <action num> <initial-state> <result-state>, don't update current state
        // This tends to break things for an action, unless all samples are arbitrary.
        // Useful for testing a wholly different series of samples/results.
        // Using the command: ss  action-number  initial-state  result-state
        // e.g. ss  0  s0b1010  s0b1111

        // Get i-state from string
        let i_state = match SomeState::from(cmd[2]) {
            Ok(i_state) => i_state,
            Err(error) => {
                return Err(error);
            }
        };

        // Get r-state from string
        let r_state = match SomeState::from(cmd[3]) {
            Ok(r_state) => r_state,
            Err(error) => {
                return Err(error);
            }
        };

        if i_state.num_bits() != r_state.num_bits() {
            return Err("States do not have the same number of bits.".to_string());
        }

        let dom_id = dmxs.current_domain;
        let dmx = &mut dmxs[dom_id];

        if i_state.num_bits() != dmx.num_bits() {
            return Err("States do not have the same number of bits as the CCD.".to_string());
        }

        println!("Act {act_id} take sample {i_state} -> {r_state}");
        let smpl = SomeSample::new(i_state, r_state);
        dmx.eval_sample_arbitrary(act_id, &smpl);
        return Ok(());
    } // end command ss 4

    Err(format!("Did not understand {cmd:?}"))
}

/// Display anchors, rating, and adjacent squares, for an action.
/// For a group that has an anchor, and is limited, the number edges, that can be changed with actions,
/// should equal the sum of the first two number of the rating.
fn display_action_anchor_info(dmxs: &mut DomainStore, cmd: &[&str]) -> Result<(), String> {
    let dom_id = dmxs.current_domain;

    if cmd.len() == 1 {
        return Err("Need to supply an action number".to_string());
    }

    // Get action number
    let act_id = match cmd[1].parse::<usize>() {
        Ok(act_id) => act_id,
        Err(error) => {
            return Err(error.to_string());
        }
    };

    // Display the rates
    dmxs[dom_id].display_action_anchor_info(act_id)
}

/// Do print-squares command.
fn do_print_select_regions(dmxs: &DomainStore, cmd: &[&str]) -> Result<(), String> {
    if cmd.len() != 1 {
        return Err("No arguments needed for the psr command".to_string());
    }

    for selx in dmxs.select.iter() {
        println!("{}", selx);
    }
    Ok(())
}

/// Do print-squares command.
fn do_print_squares_command(dmxs: &DomainStore, cmd: &Vec<&str>) -> Result<(), String> {
    let dom_id = dmxs.current_domain;
    let dmx = &dmxs[dom_id];

    if cmd.len() == 1 {
        return Err("Need to supply at least an action number".to_string());
    }

    // Get action number
    let act_id = match cmd[1].parse::<usize>() {
        Ok(act_id) => act_id,
        Err(error) => {
            return Err(error.to_string());
        }
    };

    if act_id < dmx.actions.len() {
    } else {
        return Err(format!("Invalid action ID for domain {dom_id}"));
    }

    if cmd.len() == 2 {
        println!(
            "Squares of Action {} are:\n{}\n",
            act_id, dmx.actions[act_id].squares
        );
        return Ok(());
    }

    if cmd.len() == 3 {
        // Get region from command.
        let aregion = SomeRegion::from(cmd[2])?;

        if aregion.num_bits() != dmx.cur_state.num_bits() {
            return Err("Invalid number of bits in region.".to_string());
        }

        println!("Squares of Action {act_id} in region {aregion} are:\n");

        let sqrs = dmx.actions[act_id].squares.squares_in_reg(&aregion);
        if sqrs.is_empty() {
            println!("No squares in region {aregion}");
            return Ok(());
        }

        let mut max_pn = Pn::One;
        let mut min_pn = Pn::Unpredictable;
        let mut max_pn_reg: Option<SomeRegion> = None;

        for sqrx in sqrs.iter() {
            println!("    {sqrx}");

            if sqrx.pn < min_pn {
                min_pn = sqrx.pn;
            }

            if sqrx.pn > max_pn {
                max_pn = sqrx.pn;
                max_pn_reg = Some(SomeRegion::new(vec![sqrx.state.clone()]));
            } else if sqrx.pn == max_pn {
                if let Some(regx) = max_pn_reg {
                    max_pn_reg = Some(regx.union(&sqrx.state));
                } else {
                    max_pn_reg = Some(SomeRegion::new(vec![sqrx.state.clone()]));
                }
            }
        }

        // Get rule union, if any
        let mut rules: Option<RuleStore> = None;
        let mut non_pn_stas = Vec::<SomeState>::new();
        for sqrx in sqrs.iter() {
            if sqrx.pn == max_pn {
                if max_pn < Pn::Unpredictable {
                    if let Some(ruls) = rules {
                        if let Some(ruls2) = ruls.union(sqrx.rules.as_ref().expect("SNH")) {
                            rules = Some(ruls2);
                        } else {
                            rules = None;
                            break;
                        }
                    } else {
                        rules = Some(sqrx.rules.clone().expect("SNH"));
                    }
                }
            } else if let Some(ref regx) = max_pn_reg {
                if regx.is_superset_of(*sqrx) {
                    non_pn_stas.push(sqrx.state.clone());
                }
            }
        }

        // Check if max Pn squares can form a group.
        let mut form_group = true;
        let mut rules_str = String::from("None");
        if max_pn == Pn::Unpredictable {
            for stax in non_pn_stas.iter() {
                let sqrx = dmx.actions[act_id].squares.find(stax).unwrap();
                if sqrx.pnc {
                    form_group = false;
                }
            }
        } else if let Some(ruls) = rules {
            rules_str = ruls.to_string();
            for stax in non_pn_stas.iter() {
                let sqrx = dmx.actions[act_id].squares.find(stax).expect(
                    "States in the non_pn_stas StateStore should all reference existing squares",
                );
                if !sqrx.rules.as_ref().expect("SNH").is_subset_of(&ruls) {
                    form_group = false;
                }
            }
        } else {
            form_group = false;
        }

        if form_group {
            println!(
                "    Min Pn: {min_pn} Max Pn: {max_pn} Rules: {rules_str} Can form group: {form_group}");
        } else {
            println!("    Min Pn: {min_pn} Max Pn: {max_pn} Can form group: {form_group}");
        }
        return Ok(());
    }

    Err(format!("Did not understand {cmd:?}"))
}

/// Do adjacent-anchor command.
fn display_group_anchor_info(dmxs: &DomainStore, cmd: &Vec<&str>) -> Result<(), String> {
    let dom_id = dmxs.current_domain;
    let dmx = &dmxs[dom_id];

    if cmd.len() == 1 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    // Get action number
    let act_id = match cmd[1].parse::<usize>() {
        Ok(act_id) => act_id,
        Err(error) => {
            return Err(error.to_string());
        }
    };

    if cmd.len() == 2 || cmd.len() > 3 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    let aregion = SomeRegion::from(cmd[2])?;

    if aregion.num_bits() != dmx.cur_state.num_bits() {
        return Err("Invalid number of bits in region given".to_string());
    }

    dmx.display_group_anchor_info(act_id, &aregion)
}

/// Do print-group-defining-squares command.
fn do_print_group_defining_squares_command(
    dmxs: &DomainStore,
    cmd: &Vec<&str>,
) -> Result<(), String> {
    let dom_id = dmxs.current_domain;
    let dmx = &dmxs[dom_id];
    if cmd.len() == 1 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    // Get action number
    let act_id = match cmd[1].parse::<usize>() {
        Ok(act_id) => act_id,
        Err(error) => {
            return Err(error.to_string());
        }
    };

    if cmd.len() == 2 || cmd.len() > 3 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    let aregion = SomeRegion::from(cmd[2])?;

    if aregion.num_bits() != dmx.cur_state.num_bits() {
        return Err("Invalid number of bits in region given".to_string());
    }

    if let Some(grpx) = dmx.actions[act_id].groups.find(&aregion) {
        for stax in grpx.region.states.iter() {
            if let Some(sqrx) = dmx.actions[act_id].squares.find(stax) {
                println!(" {}", sqrx);
            }
        }
    } else {
        return Err(format!("Region {} not found", aregion));
    }
    Ok(())
}

/// Display usage options.
fn usage() {
    println!("\nStartup Commands: <invoke> may be the command \"ues\" or \"cargo run\"");
    println!("\n    <invoke>                 - Run interactively, press Enter for each step.");
    println!(
        "\n    <invoke> 1               - Run non-interactively, stop when no needs can be done."
    );
    println!("\n    <invoke> <number times>  - Run a number, greater than 1, times. Exit with step and duration statistics.");
    println!(
        "\n    <invoke> <file path>     - Open a file previously stored with the fsd command."
    );
    println!("\n    <invoke> [h | help]      - Show this list.\n");
    println!("\nSession Commands:");
    println!("\n    h | help                 - Help list display (this list).");
    println!(
        "\n    cd <dom num>             - Change the Curently Displayed Domain (CDD) to the given domain number."
    );
    println!("\n    Press Enter (no command) - Satisfy one need that can be done, if any.");
    println!("\n    q | exit | quit          - Quit the program.");
    println!("\n\n    aj <act num>             - For an Action in the CDD, display all groups anchor, and adJacent, info.");
    println!("    aj <act num> <region>    - For an Action in the CDD, display group anchor, and adJacent, info.");
    println!("\n    cs <state>               - Change State, an arbitrary change, for the CDD.");
    println!("\n    dn <need number>         - Do a particular Need from the can-do need list.");
    println!("\n    dcs                      - Display Current State, and domain.  After a number of commands,");
    println!("                               the current state scrolls off screen, this might be useful.");
    println!("\n    fsd <path>               - File Store Data.");
    println!(
        "\n    gps <act num> <region>   - Group Print Squares that define the group region, of a given action, of the CDD."
    );
    println!("\n    ppd <need number>        - Print the Plan Details for a given need number in the can-do list.");
    println!("\n    ps <act num>             - Print all Squares for an action, of the CDD.");
    println!(
        "    ps <act num> <region>    - Print Squares in a given action and region, of the CDD."
    );
    println!("\n    psr                      - Print Select Regions.");
    println!("\n    run                      - Run until there are no needs that can be done.");
    println!("\n    ss <act num>                        - Sample the current State, for a given action, for the CDD.");
    println!("    ss <act num> <state>                - Sample State for a given action and state, for the CDD.");
    println!(
        "    ss <act num> <state> <result-state> - Sample State, for a given action, state and arbitrary result, for the CDD."
    );
    println!("\n    to <region>              - Change the current state TO within a region, by calculating and executing a plan.");
    println!(
        "                               To find out more about why a need cannot be satisfied:"
    );
    println!("                               If needed, use the \"cd\" command, to display the need domain.");
    println!("                               Then use this command, with the need target.");
    println!("\n    A domain number is an integer, zero or greater, where such a domain exists. CDD means the Currently Displayed Domain.");
    println!("\n    An action number is an integer, zero or greater, where such an action exists.");
    println!("\n    A need number is an integer, zero or greater, where such a need exists.");
    println!("\n    A state starts with an 's0b' or 's0x', followed by one, or more, digits.");
    println!("\n    A region starts with an 'r' character, followed by one, or more, zero, one, X or x characters.");
    println!("    A region displayed with a trailing \"+\" indicates the region is formed by more than two states.");
    println!("\n    A region, or state, may contain the separator '_', which will be ignored. All bit positions must be specified.");
    println!("\n    A state can be used instead of a region, it will be translated to a region with no X-bits.");
    println!("\n    pn stands for pattern number, the number of different samples. 1 = 1 kind of result, 2 = 2 kinds of results, in order. U = upredictable.");
    println!("\n    pnc stands for pattern number confirmed, by enough extra samples.");
    println!("\n    If there is an select region for the CDD, when no more needs can be done, the program will seek to change the current state");
    println!("    to be in an select region.");
    println!("\n    If there is another select region the current state is not in, after a (3 * number-regions-in) steps, the program will get bored");
    println!("    and seek to move the current state to a different select region, or to an intersection of select regions.");
    println!(
        "\n    \"P:0[]\" means Plan: Domain 0. The current state can be used to satisfy the need."
    );
    println!("\n    \"P:1[2,3]/1/2/+5\" means Plan: Domain 1. Run action 2, then action 3, to change the current state to satisfy the need.");
    println!("    The number of desired bit changes is 1.");
    println!("    The number of bit changes in the plan that cause the desired bit changes is 2.");
    println!("    The regions the plan passes through have an aggregate rating of positive 5.");
    println!("\n    Once the current state is correct, most (but not all) needs require an additional action to get a sample.");
    println!("\n    Needs that cannot be done.  Lets say the current state is s00000000, there is a need for s10000000, and an action that changes");
    println!("    the left-most two bits.  From state s00.. the only option is state s11.. using that action.  Using the command \"cs s10<any 6 more bits>\"");
    println!("    will get things moving again.");
    println!("\n    After no more needs can be done, select region seeking logic will be used.  If there is more than one select");
    println!("    region, repeatedly pressing enter will increase the boredom duration, after the value of the select regions");
    println!("    the current state is in, a different select region will be sought.");
}

///Pause for input from user.
pub fn pause_for_input(prompt: &str) -> String {
    // Print prompt without going to a new line
    print!("{prompt}");
    io::stdout()
        .flush()
        .expect("This system call should have worked");

    // Init and read in string
    let mut in_str = String::new();
    io::stdin()
        .read_line(&mut in_str)
        .expect("Failed to read line");

    in_str.trim().to_string()
}

/// Load data from a given path string.
fn load_data(path_str: &str) -> Result<DomainStore, String> {
    let path = Path::new(path_str);
    let display = path.display();

    // Open a file, returns `io::Result<File>`
    match File::open(path) {
        Err(why) => Err(format!("Couldn't open {display}: {why}")),
        Ok(mut afile) => {
            let mut serialized = String::new();
            match afile.read_to_string(&mut serialized) {
                Err(why) => Err(format!("Couldn't read {display}: {why}")),
                Ok(_) => {
                    let deserialized_r = serde_yaml::from_str(&serialized);
                    match deserialized_r {
                        Err(why) => Err(format!("Couldn't deserialize {display}: {why}")),
                        Ok(new_dmxs) => Ok(new_dmxs),
                    } // end match deserialized_r
                }
            }
        }
    } // end match open file
}

/// Store current data to a given path string.
fn store_data(dmxs: &DomainStore, cmd: &Vec<&str>) -> Result<(), String> {
    if cmd.len() != 2 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    let path_str = &cmd[1];
    let serialized_r = serde_yaml::to_string(&dmxs);

    match serialized_r {
        Ok(serialized) => {
            let path = Path::new(&path_str);
            let display = path.display();

            // Open a file in write-only mode, returns `io::Result<File>`
            match File::create(path) {
                Err(why) => Err(format!("Couldn't create {display}: {why}")),
                Ok(mut file) => match file.write_all(serialized.as_bytes()) {
                    Err(why) => Err(format!("Couldn't write to {display}: {why}")),
                    Ok(_) => {
                        println!("Data written");
                        Ok(())
                    }
                },
            }
        }
        Err(error) => Err(format!("Couldn't serialize {path_str}: {error}")),
    } // end match serialized_r
} // end store_data
