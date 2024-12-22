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
use crate::regionscorr::RegionsCorr;
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
use crate::needstore::NeedStore;
mod plan;
mod pn;
mod statescorr;
mod statestore;
use pn::Pn;
mod sessiondata;
mod step;
mod stepstore;
use sessiondata::{SessionData, InxPlan, NeedPlan};
mod actioninterface;
mod planscorr;
mod planscorrstore;
mod planstore;
mod regionscorrstore;
mod selectregions;
mod selectregionsstore;
mod target;
use crate::target::ATarget;
mod maskscorr;
mod maskstore;
mod domainstore;

extern crate unicode_segmentation;

use std::io;
use std::io::{Read, Write};
extern crate rand;
use std::fs::File;
use std::path::Path;
use std::process;
use std::str::FromStr;
use std::time::{Duration, Instant};

/// Get and react to arguments given.
fn main() {
    // Get arguments, if any.
    let args: Vec<String> = env::args().collect();
    //println!("{:?}", args);

    // Run default DomainStore configuration.
    if args.len() == 1 {
        let rc = run_with_file("./default.kmp", 0);
        process::exit(rc);
    }

    if args.len() == 2 {
        if args[1] == "h" || args[1] == "help" {
            usage();
        }

        let rc = if let Ok(runs) = args[1].parse::<usize>() {
            // Run default DomainStore configuration a number of times.
            run_with_file("./default.kmp", runs)
        } else {
            // Run with file with user input step-by-step.
            run_with_file(&args[1], 0)
        };
        process::exit(rc);
    }

    if args.len() == 3 {
        let rc = if let Ok(runs) = args[1].parse::<usize>() {
            // Run arg[2] DomainStore configuration, arg[1] number of times.
            run_with_file(&args[2], runs)
        } else if let Ok(runs) = args[2].parse::<usize>() {
            // Run arg[1] DomainStore configuration, arg[2] number of times.
            run_with_file(&args[1], runs)
        } else {
            1
        };
        process::exit(rc);
    }

    usage();
    eprintln!("Did not understand {:?}", args);
    process::exit(1);
} // end main

/// Load data from a file, then run with user input, step by step.
fn run_with_file(file_path: &str, runs: usize) -> i32 {
    // Init DomainStore or read in from file.
    let mut dmxs = match load_data(file_path) {
        Ok(new_dmxs) => new_dmxs,
        Err(errstr) => {
            eprintln!("main::run_with_file: {errstr}");
            return 1;
        }
    };

    // Display UI info.
    usage();

    dmxs.print_select_stores_info();

    // Display current state.
    let (needs, can_do, cant_do) = generate_and_display_needs(&mut dmxs);

    // run it
    match runs {
        0 => {
            do_interactive_session(&mut dmxs, needs, can_do, cant_do);
        }
        1 => {
            do_session_until_no_needs(&mut dmxs);
        }
        _ => {
            run_number_times(&mut dmxs, runs);
        }
    }
    0
}

/// Run until no more needs can be done, then take user input.
fn do_session_until_no_needs(dmxs: &mut SessionData) {
    let start = Instant::now();
    do_session(dmxs);

    let duration = start.elapsed();
    println!(
        "Steps: {} Time elapsed: {:.2?} seconds",
        dmxs.step_num - 1,
        duration
    );

    let (needs, can_do, cant_do) = generate_and_display_needs(dmxs);

    do_interactive_session(dmxs, needs, can_do, cant_do);
}

/// Run a number of times without user input, generate aggregate data.
/// Return number failures, that is the number of seessions that ended with unsatisfied needs.
fn run_number_times(dmxs: &mut SessionData, num_runs: usize) -> usize {
    let mut runs_left = num_runs;
    let mut cant_do = 0;
    let mut duration_vec = Vec::<Duration>::with_capacity(num_runs);
    let mut steps_vec = Vec::<usize>::with_capacity(num_runs);
    let mut num_groups_off = 0;

    while runs_left > 0 {
        runs_left -= 1;

        let start = Instant::now();
        let (steps, groups, expected, num_cant) = do_one_session(dmxs);

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

/// Do a session until no needs can be done.
pub fn do_session(dmxs: &mut SessionData) -> usize {
    loop {
        // Generate needs, get can_do and cant_do need vectors.
        let (needs, can_do, cant_do) = generate_and_display_needs(dmxs);

        // Check for end.
        if can_do.is_empty() {
            return cant_do.len();
        }

        let np_inx = dmxs.choose_a_need(&can_do, &needs);

        match dmxs.do_a_need(&needs[can_do[np_inx].inx], &can_do[np_inx]) {
            Ok(()) => println!("Need satisfied"),
            Err(errstr) => println!("{errstr}"),
        }
    } // end loop
}

/// Do a session until no needs can be done,
/// then position to a desired end state,
/// return true if the desired end state is attained.
pub fn do_session_then_end_state(dmxs: &mut SessionData, end_state_within: &RegionsCorr) -> bool {
    loop {
        // Generate needs, get can_do and cant_do need vectors.
        let (needs, can_do, _cant_do) = generate_and_display_needs(dmxs);

        // Check for end.
        if can_do.is_empty() {
            match to_end_state_within(dmxs, end_state_within) {
                Ok(()) => return true,
                Err(_) => return false,
            }
        }

        let np_inx = dmxs.choose_a_need(&can_do, &needs);

        match dmxs.do_a_need(&needs[can_do[np_inx].inx], &can_do[np_inx]) {
            Ok(()) => println!("Need satisfied"),
            Err(errstr) => println!("{errstr}"),
        }
    } // end loop
}

/// Seek an end state within a given RegionsCorr instance.
fn to_end_state_within(dmxs: &mut SessionData, end_state: &RegionsCorr) -> Result<(), String> {
    let cur_regions = dmxs.all_current_regions();

    debug_assert!(cur_regions.is_congruent(end_state));

    if end_state.is_superset_of(&cur_regions) {
        return Ok(());
    }

    match dmxs.plan_using_least_negative_select_regions(&cur_regions, end_state) {
        Ok(planx) => match planx {
            NeedPlan::AtTarget {} => panic!("This condition should have been checked for earlier."),
            NeedPlan::PlanFound { plan: plnx } => match dmxs.run_planscorrstore(&plnx) {
                Ok(_) => Ok(()),
                Err(errstr) => Err(errstr),
            },
        },
        Err(errstr) => Err(format!("{:?}", errstr)),
    }
}

/// Generate and display domain and needs.
pub fn generate_and_display_needs(dmxs: &mut SessionData) -> (NeedStore, Vec<InxPlan>, Vec<usize>) {
    // Get the needs of all Domains / Actions
    dmxs.print();
    let (needs, can_do, cant_do) = dmxs.get_needs();
    display_needs(dmxs, &needs, &can_do, &cant_do);
    (needs, can_do, cant_do)
}

pub fn display_needs(dmxs: &SessionData, needs: &NeedStore, can_do: &[InxPlan], cant_do: &[usize]) {
    // Print needs.
    if needs.is_empty() {
        println!("\nNumber needs: 0");
    } else {
        // Print needs that cannot be done.
        if cant_do.is_empty() {
            // println!("\nNeeds that cannot be done: None");
        } else {
            println!("\nNeeds that cannot be done:");
            for ndplnx in cant_do.iter() {
                println!("   {}", needs[*ndplnx]);
            }
        }
    }
    // Print needs that can be done.
    print_can_do(dmxs, can_do, needs);
}

/// Print needs that can be done.
pub fn print_can_do(dmxs: &SessionData, can_do: &[InxPlan], needs: &NeedStore) {
    if can_do.is_empty() {
        if needs.is_not_empty() {
            println!("\nNeeds that can be done: None");
        }
        dmxs.print_select();
    } else {
        println!("\nNeeds that can be done:");

        for (inx, ndplnx) in can_do.iter().enumerate() {
            if ndplnx.desired_num_bits_changed != 0 {
                println!(
                    "{:2} {} {}/{}/{}/{:+}",
                    inx,
                    needs[ndplnx.inx],
                    match &ndplnx.plans {
                        NeedPlan::AtTarget {} => "At Target".to_string(),
                        NeedPlan::PlanFound { plan: plnx } => plnx.str_terse(),
                    },
                    ndplnx.desired_num_bits_changed,
                    ndplnx.process_num_bits_changed,
                    ndplnx.rate,
                );
            } else {
                println!(
                    "{:2} {} {}",
                    inx,
                    needs[ndplnx.inx],
                    match &ndplnx.plans {
                        NeedPlan::AtTarget {} => "At Target".to_string(),
                        NeedPlan::PlanFound { plan: plnx } => plnx.str_terse(),
                    }
                );
            }
        } // next ndplnx
    }
}

/// Do one session to end.
/// Return the number steps taken get to the point where
/// there are no more needs.
/// Return domainstore end-state info.
fn do_one_session(dmxs: &mut SessionData) -> (usize, usize, usize, usize) {
    let num_cant = do_session(dmxs);

    (
        dmxs.step_num,
        dmxs.number_groups(),
        dmxs.number_groups_expected(),
        num_cant,
    )
}

/// Do a session, step by step, taking user commands.
pub fn do_interactive_session(
    dmxs: &mut SessionData,
    mut needs: NeedStore,
    mut can_do: Vec<InxPlan>,
    mut cant_do: Vec<usize>,
) {
    loop {
        command_loop(dmxs, &needs, &can_do, &cant_do);

        // Generate needs, get can_do and cant_do need vectors.
        (needs, can_do, cant_do) = generate_and_display_needs(dmxs);
    } // end loop
}

/// Do command loop.
/// Some commands work without the need to return and display the session
/// state again, so the loop.  Otherwise the command returns.
fn command_loop(dmxs: &mut SessionData, needs: &NeedStore, can_do: &[InxPlan], cant_do: &[usize]) {
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
            if can_do.is_empty() {
            } else {
                do_any_need(dmxs, needs, can_do);
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
                    dmxs.print();
                    display_needs(dmxs, needs, can_do, cant_do);
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
                dmxs.print();
                print_can_do(dmxs, can_do, needs);
            }
            "dn" => match do_chosen_need(dmxs, &cmd, needs, can_do) {
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
                dmxs.print();
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
            "ppd" => match do_print_plan_details(dmxs, &cmd, needs, can_do) {
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
                if can_do.is_empty() {
                } else {
                    do_session(dmxs);
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
fn do_change_domain(dmxs: &mut SessionData, cmd: &[&str]) -> Result<(), String> {
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
fn do_any_need(dmxs: &mut SessionData, needs: &NeedStore, can_do: &[InxPlan]) {
    let np_inx = dmxs.choose_a_need(can_do, needs);

    match dmxs.do_a_need(&needs[can_do[np_inx].inx], &can_do[np_inx]) {
        Ok(()) => println!("Need satisfied"),
        Err(errstr) => println!("{errstr}"),
    }
}

/// Print details of a given plan
fn do_print_plan_details(
    dmxs: &SessionData,
    cmd: &[&str],
    needs: &NeedStore,
    can_do: &[InxPlan],
) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one need-number argument is needed for the ppd command.".to_string());
    }
    match cmd[1].parse::<usize>() {
        Ok(n_num) => {
            if n_num >= can_do.len() {
                Err(format!("Invalid Need Number: {}", cmd[1]))
            } else {
                let ndx = &needs[can_do[n_num].inx];

                println!("\n{} Need: {}", n_num, ndx);
                match &can_do[n_num].plans {
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
fn do_chosen_need(
    dmxs: &mut SessionData,
    cmd: &[&str],
    needs: &NeedStore,
    can_do: &[InxPlan],
) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one need-number argument is needed for the dn command.".to_string());
    }

    let dom_id = dmxs.current_domain;

    match cmd[1].parse::<usize>() {
        Ok(n_num) => {
            if n_num >= can_do.len() {
                Err(format!("Invalid Need Number: {}", cmd[1]))
            } else {
                let nd_inx = can_do[n_num].inx;

                println!(
                    "\nNeed chosen: {:2} {} {}",
                    n_num,
                    needs[nd_inx],
                    match &can_do[n_num].plans {
                        NeedPlan::AtTarget {} => "At Target".to_string(),
                        NeedPlan::PlanFound { plan: plnsx } => plnsx.str_terse(),
                    }
                );

                match needs[nd_inx] {
                    SomeNeed::ToSelectRegions { .. } => (),
                    SomeNeed::ExitSelectRegions { .. } => (),
                    _ => {
                        if dom_id != needs[nd_inx].dom_id().unwrap() {
                            dmxs.change_domain(needs[nd_inx].dom_id().unwrap());
                            dmxs.print();
                        }
                    }
                }

                match dmxs.do_a_need(&needs[can_do[n_num].inx], &can_do[n_num]) {
                    Ok(()) => {
                        println!("Need satisfied");
                        Ok(())
                    }
                    Err(errstr) => Err(errstr),
                }
            }
        }
        Err(error) => Err(format!("{error}")),
    }
}

/// Do a change-state command.
fn do_change_state_command(dmxs: &mut SessionData, cmd: &[&str]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one state argument is needed for the cs command.".to_string());
    }
    // Get state from string
    match SomeState::from_str(cmd[1]) {
        Ok(a_state) => {
            println!("Changed state to {a_state}");
            dmxs.set_cur_state(a_state);
            Ok(())
        }
        Err(error) => Err(format!("\nDid not understand state, {error}")),
    } // end match
}

/// Do to-region command.
fn do_to_region_command(dmxs: &mut SessionData, cmd: &[&str]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one region argument is needed for the to command.".to_string());
    }

    // Get region from string
    let goal_region = SomeRegion::from_str(cmd[1])?;

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
        "\nChange Current_state {cur_state}\n           to region {goal_region} num bit changes needed {}\n                 m01 {}\n                 b10 {}",
        needed_change.number_changes(),
        needed_change.m01, needed_change.m10
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
                region: goal_region.clone(),
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
fn do_sample_state_command(dmxs: &mut SessionData, cmd: &Vec<&str>) -> Result<(), String> {
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
        dmx.take_action(act_id);
        return Ok(());
    }

    if cmd.len() == 3 {
        // Get state from string
        let a_state = match SomeState::from_str(cmd[2]) {
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
        dmx.take_action(act_id);
        return Ok(());
    }

    if cmd.len() == 4 {
        // Take arbitrary sample with <action num> <initial-state> <result-state>, don't update current state
        // This tends to break things for an action, unless all samples are arbitrary.
        // Useful for testing a wholly different series of samples/results.
        // Using the command: ss  action-number  initial-state  result-state
        // e.g. ss  0  s1010  s1111

        // Get i-state from string
        let i_state = match SomeState::from_str(cmd[2]) {
            Ok(i_state) => i_state,
            Err(error) => {
                return Err(error);
            }
        };

        // Get r-state from string
        let r_state = match SomeState::from_str(cmd[3]) {
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
fn display_action_anchor_info(dmxs: &mut SessionData, cmd: &[&str]) -> Result<(), String> {
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
fn do_print_select_regions(dmxs: &SessionData, cmd: &[&str]) -> Result<(), String> {
    if cmd.len() != 1 {
        return Err("No arguments needed for the psr command".to_string());
    }

    for selx in dmxs.select.iter() {
        println!("{}", selx);
    }
    Ok(())
}

/// Do print-squares command.
fn do_print_squares_command(dmxs: &SessionData, cmd: &Vec<&str>) -> Result<(), String> {
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
        let aregion = SomeRegion::from_str(cmd[2])?;

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
fn display_group_anchor_info(dmxs: &SessionData, cmd: &Vec<&str>) -> Result<(), String> {
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

    let aregion = SomeRegion::from_str(cmd[2])?;

    if aregion.num_bits() != dmx.cur_state.num_bits() {
        return Err("Invalid number of bits in region given".to_string());
    }

    dmx.display_group_anchor_info(act_id, &aregion)
}

/// Do print-group-defining-squares command.
fn do_print_group_defining_squares_command(
    dmxs: &SessionData,
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

    let aregion = SomeRegion::from_str(cmd[2])?;

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
    println!("\n    <invoke>                  - Run default.kmp interactively, press Enter for each step.");
    println!(
        "\n    <invoke> 1                - Run default.kmp non-interactively, stop when no needs can be done."
    );
    println!("\n    <invoke> <number times>   - Run default.kmp a number (> 1) times. Exit with step and duration statistics.");
    println!(
        "\n    <invoke> <file path>      - Open a file previously stored with the fsd command."
    );
    println!(
        "\n    <invoke> <file path>      - Run a file containing a SessionData configuration interactively, press Enter for each step."
    );
    println!(
        "\n    <invoke> <file path> 1    - Run a file containing a SessionData configuration non-interactively, stop when no needs can be done."
    );
    println!(
        "\n    <invoke> <file path> <nt> - Run a file containing a SessionData configuration a number (> 1) times. Exit with step and duration statistics."
    );
    println!("\n    <invoke> [h | help]       - Show this list.\n");
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
    println!("\n    A need number is an integer, zero or greater, in a displayed list of needs that can be done.");
    println!(
        "\n    A state starts with an 's' character, followed by one, or more, binary digits."
    );
    println!("\n    A region starts with an 'r' character, followed by one, or more, zero, one, X or x characters.");
    println!("    A region displayed with a trailing \"+\" indicates the region is formed by more than two states.  Two states is a goal.");
    println!("\n    A region, or state, may contain the separator '_', which will be ignored. All bit positions must be specified.");
    println!("\n    A state can be used instead of a region, it will be translated to a region with no X-bits.");
    println!("\n    pn stands for pattern number, the number of different samples. 1 = 1 kind of result, 2 = 2 kinds of results, in order. U = upredictable.");
    println!("\n    pnc stands for pattern number confirmed, by enough extra samples.");
    println!("          The bar for this is fairly low.");
    println!("          Additional samples can cycle the pn and pnc values through all possible combinations.");
    println!("\n    A Select Region is an arbitrary region, across all domains, with a given value, positive or negative.");
    println!("\n        Plans, to satisfy a need, are made to avoid negative select regions, if possible.");
    println!("\n        Finding the current state within a negative select region, the program will attempt to exit the region.");
    println!("\n    A plan to satisfy a need may be shown in one of two ways.");
    println!("\n        At Target - The current state is within the need target, a sample can be taken immediately.");
    println!("\n        PCS[PC[P[0:1], P[1:3]], PC[P[0:3,2]]]/3/6/-1 - Changes need to be made to the current state to be within the need target.");
    println!("\n            PCS[ ... ]/3/6/-1 - A Plan Corresponding (per domain) Store, to change 3 bits, using plans that change 6 bits, passing through -1 valued regions.");
    println!("\n            PC[ .. ] - A Plan Corresponding (per domain). No more than one plan per domain. If more than one plan, the plans will be run in parallel.");
    println!("\n            P[ .. ] - A Plan. One, or more, actions for a single domain.");
    println!("\n            0:3,2 - For domain 0, run action 3, then action 2.");
    println!("\n    Once the current state is within the need target, most (but not all) needs require an additional action to get a sample.");
    println!("\n    Needs that cannot be done.  Lets say the current state is s0000, there is a need for s1000, and no action that changes");
    println!(
        "    the left-most bit.  Using the command \"cs s1000\" will get things moving again."
    );
    println!("\n        If there is only one square that makes a change that no other square makes, and that square has not happened to be sampled,");
    println!(
        "        the understanding of the logic will be deficient until the square is sampled."
    );
    println!(
        "\n    After no more needs can be done, positive select region seeking logic will be used."
    );
    println!("    Repeatedly pressing the Enter key will increase the boredom duration.");
    println!("    If there is more than one positive select region, when the boredom value reaches the the select region value,");
    println!("    a different select region will be sought.");
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
fn load_data(path_str: &str) -> Result<SessionData, String> {
    let path = Path::new(path_str);
    let display = path.display();

    // Open a file, returns `io::Result<File>`
    match File::open(path) {
        Ok(mut afile) => {
            let mut file_content = String::new();
            match afile.read_to_string(&mut file_content) {
                Ok(_) => {
                    match serde_yaml::from_str(&file_content) {
                        Ok(new_sdx) => Ok(new_sdx),
                        Err(_) => {
                            match SessionData::from_str(&tools::remove_comments(&file_content)) {
                                Ok(new_sdx) => Ok(new_sdx),
                                Err(why) => Err(format!("Main::load_data: {display} {why}")),
                            }
                        }
                    } // end match deserialized_r
                }
                Err(why) => Err(format!("Main::load_data: {display} {why}")),
            }
        }
        Err(why) => Err(format!("Main::load_data: {display} {why}")),
    } // end match open file
}

/// Store current data to a given path string.
fn store_data(sdx: &SessionData, cmd: &Vec<&str>) -> Result<(), String> {
    if cmd.len() != 2 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    let path_str = &cmd[1];
    let serialized_r = serde_yaml::to_string(&sdx);

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::regionscorr::RegionsCorr;
    use crate::selectregions::SelectRegions;

    /// Force a misapprehension of a rule for action 4.
    /// Develop actions 0-3 in the normal way.
    /// In action 4, testing squares adjacent to the anchor, using actions 0-3, should invalidate the first group.
    #[test]
    fn bad_start() -> Result<(), String> {
        // Create SessionData.
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]],
            ACT[[XX/XX/01/X1], [XX/XX/10/X0]]]],
        ]",
        )?;

        // Set up group XX/XX/Xx/XX for action 4.
        sdx[0].take_action_arbitrary(4, &SomeState::from_str("s0001")?);
        sdx[0].take_action_arbitrary(4, &SomeState::from_str("s1110")?);
        sdx[0].take_action_arbitrary(4, &SomeState::from_str("s0001")?);
        sdx[0].take_action_arbitrary(4, &SomeState::from_str("s1110")?);
        sdx[0].take_action_arbitrary(4, &SomeState::from_str("s0001")?);
        sdx[0].take_action_arbitrary(4, &SomeState::from_str("s1110")?);

        sdx.print();
        assert!(sdx[0].actions[4].number_groups() == 1);
        let grpx = sdx[0].actions[4]
            .groups
            .find(&SomeRegion::from_str("XXXX")?)
            .unwrap();
        println!("Group {} found", grpx.region);

        do_session(&mut sdx); // Figure other actions, and test group in action 4.
        sdx.print();

        assert!(sdx[0].actions[4].number_groups() == 2); // s/b XX/XX/10/X0, XX/XX/01/X1.
        let grpx = sdx[0].actions[4]
            .groups
            .find(&SomeRegion::from_str("XX1X")?)
            .unwrap();
        println!("Group {} found", grpx.region);
        let grpx = sdx[0].actions[4]
            .groups
            .find(&SomeRegion::from_str("XX0X")?)
            .unwrap();
        println!("Group {} found", grpx.region);

        Ok(())
    }

    /// Test the cleanup of unneeded groups.
    /// First use of running a full session from a test function.
    #[test]
    fn cleanup() -> Result<(), String> {
        // Create SessionData.
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]],
            ACT[[XX/11/01/Xx], [11/XX/10/Xx], [Xx/00/00/XX], [01/XX/11/XX]]]],
        ]",
        )?;

        sdx[0].set_cleanup(4, 500); // Effectively, turn off clean_up for action 4.

        do_session(&mut sdx);

        sdx.print();
        assert!(sdx[0].actions[0].groups.len() == 1);
        assert!(sdx[0].actions[1].groups.len() == 1);
        assert!(sdx[0].actions[2].groups.len() == 1);
        assert!(sdx[0].actions[3].groups.len() == 1);
        assert!(sdx[0].actions[4].groups.len() == 6);

        // Check action 4 primary groups.
        if let Some(grpx) = sdx[0].actions[4]
            .groups
            .find(&SomeRegion::from_str("rX10X")?)
        {
            assert!(grpx.limited);
        } else {
            return Err("Group rX10X not found?".to_string());
        }
        if let Some(grpx) = sdx[0].actions[4]
            .groups
            .find(&SomeRegion::from_str("r1X1X")?)
        {
            assert!(grpx.limited);
        } else {
            return Err("Group r1X1X not found?".to_string());
        }
        if let Some(grpx) = sdx[0].actions[4]
            .groups
            .find(&SomeRegion::from_str("rX00X")?)
        {
            assert!(grpx.limited);
        } else {
            return Err("Group rX00X not found?".to_string());
        }
        if let Some(grpx) = sdx[0].actions[4]
            .groups
            .find(&SomeRegion::from_str("r0X1X")?)
        {
            assert!(grpx.limited);
        } else {
            return Err("Group r0X1X not found?".to_string());
        }

        // Check unneeded groups.
        let subs = sdx[0].actions[4]
            .groups
            .subsets_of(&SomeRegion::from_str("r00XX")?);
        println!("subsets of r00XX {subs}");
        assert!(subs.len() == 1);
        let grpx = sdx[0].actions[4].groups.find(&subs[0]).expect("SNH");
        assert!(!grpx.limited);

        let subs = sdx[0].actions[4]
            .groups
            .subsets_of(&SomeRegion::from_str("r11XX")?);
        println!("subsets of r11XX {subs}");
        assert!(subs.len() == 1);
        let grpx = sdx[0].actions[4].groups.find(&subs[0]).expect("SNH");
        assert!(!grpx.limited);

        // Do cleanup to delete unneeded groups.
        sdx.cleanup(0, 4, &NeedStore::new(vec![]));

        sdx.print();
        assert!(sdx[0].actions[4].groups.len() == 4);
        let subs = sdx[0].actions[4]
            .groups
            .subsets_of(&SomeRegion::from_str("r11XX")?);
        assert!(subs.is_empty());

        let subs = sdx[0].actions[4]
            .groups
            .subsets_of(&SomeRegion::from_str("r00XX")?);
        assert!(subs.is_empty());

        Ok(())
    }

    /// For a single positive SelectRegion,
    /// Program develops rules, program seeks positive SelectRegion, then gets bored beyond limit.
    #[test]
    fn select1() -> Result<(), String> {
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]]]],
            SR[RC[r1000], 1]
        ]",
        )?;

        // Develop rules, position to desired end state.
        if !do_session_then_end_state(&mut sdx, &RegionsCorr::from_str("RC[r0000]")?) {
            return Err("Session to end state failed".to_string());
        }

        // Insure boredom is zero.
        let (needs, can_do, _cant_do) = generate_and_display_needs(&mut sdx);
        assert!(sdx.boredom == 0);
        assert!(needs.len() == 1);
        assert!(needs.contains_similar_need(
            "ToSelectRegions",
            &ATarget::SelectRegions {
                select: SelectRegions::from_str("SR[RC[r1000], 1]")?
            }
        ));

        sdx.print();
        assert!(sdx[0].actions[0].groups.len() == 1);
        assert!(sdx[0].actions[1].groups.len() == 1);
        assert!(sdx[0].actions[2].groups.len() == 1);
        assert!(sdx[0].actions[3].groups.len() == 1);

        // Move to positive region.
        do_any_need(&mut sdx, &needs, &can_do);

        assert!(sdx[0].cur_state == (SomeState::from_str("s1000")?));

        generate_and_display_needs(&mut sdx);
        generate_and_display_needs(&mut sdx);

        assert!(sdx.boredom > sdx.boredom_limit);

        //assert!(1 == 2);
        Ok(())
    }

    /// Test going into a net-positive SelectRegion, out of boredom, then exiting because of
    /// an additional, smaller, negative influence.
    #[test]
    fn select2() -> Result<(), String> {
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]]]],
            SR[RC[r01X1], 3],
            SR[RC[rX111], -1]
        ]",
        )?;

        // Develop rules.
        if !do_session_then_end_state(&mut sdx, &RegionsCorr::from_str("RC[r0101]")?) {
            return Err("Session to end state failed".to_string());
        }

        sdx.print();
        assert!(sdx[0].actions[0].groups.len() == 1);
        assert!(sdx[0].actions[1].groups.len() == 1);
        assert!(sdx[0].actions[2].groups.len() == 1);
        assert!(sdx[0].actions[3].groups.len() == 1);

        // Inc boredom by 1.
        let (needs, can_do, cant_do) = generate_and_display_needs(&mut sdx);
        println!("needs {needs}");
        println!("cant_do {}", cant_do.len());
        println!("can_do {}", can_do.len());
        //assert!(can_do.is_empty());
        // Inc boredom by 1.
        let (needs, can_do, cant_do) = generate_and_display_needs(&mut sdx);
        println!("needs {needs}");
        println!("cant_do {}", cant_do.len());
        println!("can_do {}", can_do.len());

        //assert!(can_do.is_empty());

        // Should want to try state 0111 now.
        let (needs, can_do, _cant_do) = generate_and_display_needs(&mut sdx);
        assert!(needs.len() == 1);
        assert!(needs.contains_similar_need(
            "ToSelectRegions",
            &ATarget::SelectRegions {
                select: SelectRegions::from_str("SR[RC[r0111], 2]")?
            }
        ));

        // Move to positive 0111..
        do_any_need(&mut sdx, &needs, &can_do);
        let (needs, _can_do, _cant_do) = generate_and_display_needs(&mut sdx);
        assert!(needs.len() == 1);
        assert!(
            needs.contains_similar_need(
                "ExitSelectRegions",
                &ATarget::DomainRegions {
                    regions: RegionsCorr::from_str("RC[rXXX0]")?
                }
            ) || needs.contains_similar_need(
                "ExitSelectRegions",
                &ATarget::DomainRegions {
                    regions: RegionsCorr::from_str("RC[rX0XX]")?
                }
            ) || needs.contains_similar_need(
                "ExitSelectRegions",
                &ATarget::DomainRegions {
                    regions: RegionsCorr::from_str("RC[rXX0X]")?
                }
            )
        );
        //assert!(1 == 2);
        Ok(())
    }

    /// Test duplicate select regions.
    #[test]
    fn select_duplicate() -> Result<(), String> {
        // Create SessionData.
        let sdx = SessionData::from_str(
            "SD[DS[DOMAIN[ACT[[XX/XX/XX/Xx]]]],
                SR[RC[r01X1], 3],
                SR[RC[r01x1], -1]
        ]",
        )?;
        assert!(sdx.select.len() == 1);
        Ok(())
    }

    /// Test no select regions.
    #[test]
    fn select_none() -> Result<(), String> {
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]]]]
        ]",
        )?;

        // Develop rules.
        if !do_session_then_end_state(&mut sdx, &RegionsCorr::from_str("RC[r0101]")?) {
            return Err("Session to end state failed".to_string());
        }

        sdx.print();
        assert!(sdx[0].actions[0].groups.len() == 1);
        assert!(sdx[0].actions[1].groups.len() == 1);
        assert!(sdx[0].actions[2].groups.len() == 1);
        assert!(sdx[0].actions[3].groups.len() == 1);

        //assert!(1 == 2);
        Ok(())
    }

    /// Test one large positive select region.
    #[test]
    fn select_one_large_positive() -> Result<(), String> {
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]]]],
            SR[RC[rXXXX], 3]
        ]",
        )?;

        // Develop rules.
        if !do_session_then_end_state(&mut sdx, &RegionsCorr::from_str("RC[r0101]")?) {
            return Err("Session to end state failed".to_string());
        }

        sdx.print();
        assert!(sdx[0].actions[0].groups.len() == 1);
        assert!(sdx[0].actions[1].groups.len() == 1);
        assert!(sdx[0].actions[2].groups.len() == 1);
        assert!(sdx[0].actions[3].groups.len() == 1);

        //assert!(1 == 2);
        Ok(())
    }

    /// Test one small positive select region.
    #[test]
    fn select_one_small_positive() -> Result<(), String> {
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]]]],
            SR[RC[r1010], 3]
        ]",
        )?;

        // Develop rules.
        if !do_session_then_end_state(&mut sdx, &RegionsCorr::from_str("RC[r0101]")?) {
            return Err("Session to end state failed".to_string());
        }

        sdx.print();
        assert!(sdx[0].actions[0].groups.len() == 1);
        assert!(sdx[0].actions[1].groups.len() == 1);
        assert!(sdx[0].actions[2].groups.len() == 1);
        assert!(sdx[0].actions[3].groups.len() == 1);

        //assert!(1 == 2);
        Ok(())
    }

    /// Test one large negative select region.
    #[test]
    fn select_one_large_negative() -> Result<(), String> {
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]]]],
            SR[RC[rXXXX], -3]
        ]",
        )?;

        // Develop rules.
        if !do_session_then_end_state(&mut sdx, &RegionsCorr::from_str("RC[r0101]")?) {
            return Err("Session to end state failed".to_string());
        }

        sdx.print();
        assert!(sdx[0].actions[0].groups.len() == 1);
        assert!(sdx[0].actions[1].groups.len() == 1);
        assert!(sdx[0].actions[2].groups.len() == 1);
        assert!(sdx[0].actions[3].groups.len() == 1);

        //assert!(1 == 2);
        Ok(())
    }

    /// Test one small negative select region.
    #[test]
    fn select_one_small_negative() -> Result<(), String> {
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]]]],
            SR[RC[r1010], -3]
        ]",
        )?;

        // Develop rules.
        if !do_session_then_end_state(&mut sdx, &RegionsCorr::from_str("RC[r0101]")?) {
            return Err("Session to end state failed".to_string());
        }

        sdx.print();
        assert!(sdx[0].actions[0].groups.len() == 1);
        assert!(sdx[0].actions[1].groups.len() == 1);
        assert!(sdx[0].actions[2].groups.len() == 1);
        assert!(sdx[0].actions[3].groups.len() == 1);

        //assert!(1 == 2);
        Ok(())
    }
}
