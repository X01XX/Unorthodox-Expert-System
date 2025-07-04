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
use std::io;
use std::io::{Read, Write};
extern crate rand;
use std::fs::File;
use std::path::Path;
use std::process;
use std::str::FromStr;
use std::time::{Duration, Instant};

mod action;
mod actionstore;
mod altrulehint;
mod bits;
mod change;
mod changescorr;
mod changestore;
mod domain;
mod domainstore;
mod group;
mod groupstore;
mod mask;
mod maskscorr;
mod maskstore;
mod need;
mod needstore;
mod plan;
mod planscorr;
mod planscorrstore;
mod planstore;
mod pn;
mod region;
mod regionscorr;
mod regionscorrstore;
mod regionstore;
mod resultstore;
mod rule;
mod rulescorr;
mod rulestore;
mod sample;
mod selectregions;
mod selectregionsstore;
mod sessiondata;
mod square;
mod squarestore;
mod state;
mod statescorr;
mod statestore;
mod step;
mod stepscorr;
mod stepstore;
mod target;
mod tools;
mod vertex;

use crate::planscorr::PlansCorr;
use crate::planscorrstore::PlansCorrStore;
use crate::regionscorr::RegionsCorr;
use crate::rulescorr::RulesCorr;
use crate::step::SomeStep;
use crate::stepscorr::StepsCorr;
use crate::stepstore::StepStore;
use crate::tools::StrLen;
use need::SomeNeed;
use pn::Pn;
use region::SomeRegion;
use rulestore::RuleStore;
use sample::SomeSample;
use sessiondata::{NeedPlan, SessionData};
use state::SomeState;

extern crate unicode_segmentation;

/// Get and react to arguments given.
fn main() {
    // Get arguments, if any.
    let args: Vec<String> = env::args().collect();
    //println!("{:?}", args);

    // Run default SessionData configuration.
    if args.len() == 1 {
        let rc = run_with_file("./default.kmp", 0);
        process::exit(rc);
    }

    if args.len() == 2 {
        if args[1] == "h" || args[1] == "help" {
            usage();
        }

        let rc = if let Ok(runs) = args[1].parse::<usize>() {
            // Run default SessionData configuration a number of times.
            run_with_file("./default.kmp", runs)
        } else {
            // Run with file with user input step-by-step.
            run_with_file(&args[1], 0)
        };
        process::exit(rc);
    }

    if args.len() == 3 {
        let rc = if let Ok(runs) = args[1].parse::<usize>() {
            // Run arg[2] SessionData configuration, arg[1] number of times.
            run_with_file(&args[2], runs)
        } else if let Ok(runs) = args[2].parse::<usize>() {
            // Run arg[1] SessionData configuration, arg[2] number of times.
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
    // Read in file.
    let file_contents = match load_data(file_path) {
        Ok(file_in) => file_in,
        Err(errstr) => {
            eprintln!("main::run_with_file: {errstr}");
            return 1;
        }
    };

    // Get SesionData struct from Serialized data or a string definition.
    let mut sdx = match serde_yaml_ng::from_str(&file_contents) {
        Ok(new_sdx) => new_sdx,
        Err(_) => match SessionData::from_str(&tools::remove_comments(&file_contents)) {
            Ok(sdx) => sdx,
            Err(errstr) => {
                eprintln!("main::run_with_file: {errstr}");
                return 1;
            }
        },
    };
    // run it
    match runs {
        0 => {
            do_interactive_session(&mut sdx, String::new());
        }
        1 => {
            do_session_until_no_needs(&mut sdx);
        }
        99 => {
            // Use file_contents repeatedly, in case random initial states are needed.
            let sdx_str = tools::remove_comments(&file_contents);

            // Run until some needs cannot be done, then drop into interactive session.
            let mut count = 0;
            loop {
                match SessionData::from_str(&sdx_str) {
                    Ok(mut sdx) => {
                        count += 1;
                        do_session(&mut sdx);
                        if sdx.cant_do.is_empty() {
                        } else {
                            do_interactive_session(&mut sdx, format!("Run count = {count}"));
                            process::exit(0);
                        }
                    }
                    Err(errstr) => {
                        eprintln!("{errstr}");
                        return 1;
                    }
                }
            }
        }
        _ => {
            let sdx_str = tools::remove_comments(&file_contents);
            run_number_times(sdx_str, runs);
        }
    }
    0
}

/// Run until no more needs can be done, then take user input.
fn do_session_until_no_needs(sdx: &mut SessionData) {
    let start = Instant::now();
    do_session(sdx);

    let duration = start.elapsed();
    let msg = format!(
        "Steps: {} Time elapsed: {:.2?} seconds",
        sdx.step_num, duration
    );

    do_interactive_session(sdx, msg);
}

/// Run a number of times without user input, generate aggregate data.
/// Return number failures, that is the number of seessions that ended with unsatisfied needs.
fn run_number_times(sdx_str: String, num_runs: usize) -> usize {
    let mut runs_left = num_runs;
    let mut cant_do = 0;
    let mut duration_vec = Vec::<Duration>::with_capacity(num_runs);
    let mut steps_vec = Vec::<usize>::with_capacity(num_runs);
    let mut num_groups_off = 0;

    while runs_left > 0 {
        // Use file_contents repeatedly, in case random initial states are needed.
        let mut sdx = match SessionData::from_str(&sdx_str) {
            Ok(sdx) => sdx,
            Err(errstr) => {
                eprintln!("{errstr}");
                return 1;
            }
        };

        runs_left -= 1;

        let start = Instant::now();
        let (steps, groups, expected, num_cant) = do_one_session(&mut sdx);

        let duration = start.elapsed();
        println!(
            "Steps {steps}, Time elapsed in do_session() is: {duration:.2?} groups: {groups:?}"
        );
        duration_vec.push(duration);
        steps_vec.push(steps);
        if groups != expected {
            num_groups_off += 1;
            println!("Num groups off");
        }
        if num_cant > 0 {
            cant_do += 1;
            println!("Some unsatisfied needs");
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
    let duration_minutes = duration_total.as_secs() as f32 / 60.0;

    println!(
        "\nRuns {}, Average steps: {} high: {}, low: {}, Elapsed time: {:.2?} minutes, Average time elapsed: {:.2?}, high: {:.2?}, low: {:.2?} Number with unsatisfied needs {} Num groups off {}",
        num_runs,
        average_steps,
        steps_high,
        steps_low,
        duration_minutes,
        average_time,
        duration_high,
        duration_low,
        cant_do,
        num_groups_off
    );
    cant_do
}

/// Do a session until no needs can be done.
pub fn do_session(sdx: &mut SessionData) -> usize {
    loop {
        // Generate needs, get can_do and cant_do need vectors.
        generate_and_display_needs(sdx);

        // Check for end.
        if sdx.can_do.is_empty() {
            return sdx.cant_do.len();
        }

        let np_inx = sdx.choose_a_need();

        match sdx.do_a_need(np_inx) {
            Ok(()) => println!("Need satisfied"),
            Err(errstr) => println!("{errstr}"),
        }
    } // end loop
}

/// Do a session until no needs can be done,
/// then position to a desired end state,
/// return true if the desired end state is attained.
pub fn do_session_then_end_state(sdx: &mut SessionData, end_state_within: &RegionsCorr) -> bool {
    loop {
        // Generate needs, get can_do and cant_do need vectors.
        generate_and_display_needs(sdx);

        // Check for end.
        if sdx.can_do.is_empty() {
            match to_end_state_within(sdx, end_state_within) {
                Ok(()) => return true,
                Err(_) => return false,
            }
        }

        let np_inx = sdx.choose_a_need();

        match sdx.do_a_need(np_inx) {
            Ok(()) => println!("Need satisfied"),
            Err(errstr) => println!("{errstr}"),
        }
    } // end loop
}

/// Seek an end state within a given RegionsCorr instance.
fn to_end_state_within(sdx: &mut SessionData, end_state: &RegionsCorr) -> Result<(), String> {
    let cur_regions = sdx.all_current_regions();

    debug_assert!(cur_regions.is_congruent(end_state));

    if end_state.is_superset_of(&cur_regions) {
        return Ok(());
    }

    match sdx.plan_using_least_negative_select_regions(&cur_regions, end_state) {
        Ok(planx) => match planx {
            NeedPlan::AtTarget {} => panic!("This condition should have been checked for earlier."),
            NeedPlan::PlanFound { plan: plnx } => match sdx.run_planscorrstore(&plnx) {
                Ok(_) => Ok(()),
                Err(errstr) => Err(errstr),
            },
        },
        Err(errstr) => Err(format!("{:?}", errstr)),
    }
}

/// Generate and display domain and needs.
pub fn generate_and_display_needs(sdx: &mut SessionData) {
    // Get the needs of all Domains / Actions
    sdx.get_needs(); // increments step_num.
    sdx.print();
    display_needs(sdx);
}

/// Get a domain number from a string.
pub fn domain_id_from_string(sdx: &SessionData, num_str: &str) -> Result<usize, String> {
    match num_str.parse() {
        Ok(d_num) => {
            if d_num >= sdx.len() {
                Err(format!("\nDomain number too large, {d_num}"))
            } else {
                Ok(d_num)
            }
        }
        Err(error) => Err(format!("Did not understand domain number, {error}")),
    } // end match
}

pub fn display_needs(sdx: &SessionData) {
    // Print needs.
    if sdx.needs.is_empty() {
        println!("\nNumber needs: 0");
    } else {
        // Print needs that cannot be done.
        if sdx.cant_do.is_empty() {
            // println!("\nNeeds that cannot be done: None");
        } else {
            println!("\nNeeds that cannot be done:");
            for ndplnx in sdx.cant_do.iter() {
                println!("   {}", sdx.needs[*ndplnx]);
            }
        }
    }
    // Print needs that can be done.
    print_can_do(sdx);
}

/// Print needs that can be done.
pub fn print_can_do(sdx: &SessionData) {
    if sdx.can_do.is_empty() {
        if sdx.needs.is_not_empty() {
            println!("\nNeeds that can be done: None");
        }
        sdx.print_select();
    } else {
        println!("\nNeeds that can be done:");

        for (inx, ndplnx) in sdx.can_do.iter().enumerate() {
            if ndplnx.desired_num_bits_changed != 0 {
                println!(
                    "{:2} {} {}/{}/{}/{:+}",
                    inx,
                    sdx.needs[ndplnx.inx],
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
                    sdx.needs[ndplnx.inx],
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
/// Return session end-state info.
fn do_one_session(sdx: &mut SessionData) -> (usize, usize, usize, usize) {
    let num_cant = do_session(sdx);

    (
        sdx.step_num,
        sdx.number_groups(),
        sdx.number_groups_defined(),
        num_cant,
    )
}

/// Do a session, step by step, taking user commands.
pub fn do_interactive_session(sdx: &mut SessionData, message: String) {
    // Display UI info.
    usage();

    sdx.print_select_stores_info();

    // Display current state.
    if sdx.step_num == 0 {
        generate_and_display_needs(sdx);
    } else {
        sdx.print();
        display_needs(sdx);
    }

    if !message.is_empty() {
        println!("{message}");
    }
    loop {
        command_loop(sdx);

        // Generate needs, get can_do and cant_do need vectors.
        generate_and_display_needs(sdx);
    } // end loop
}

/// Do command loop.
/// Some commands work without the need to return and display the session
/// state again, so the loop.  Otherwise the command returns.
fn command_loop(sdx: &mut SessionData) {
    //println!("start command loop step {}", sdx.step_num);
    loop {
        let in_str = pause_for_input("\nPress Enter or type a command: ");

        let cmd = match tools::parse_input(&in_str) {
            Ok(cmdvec) => cmdvec,
            Err(errstr) => {
                println!("{errstr}");
                vec!["error".to_string()]
            }
        };
        if cmd.len() == 1 && cmd[0] == "error" {
            continue;
        }

        // Default command, just press Enter
        if cmd.is_empty() {
            // Process needs
            if sdx.can_do.is_empty() {
            } else {
                do_any_need(sdx);
            }
            return;
        }

        // Do commands
        match &*cmd[0] {
            "aj" => {
                if cmd.len() == 2 {
                    match display_action_anchor_info(sdx, &cmd) {
                        Ok(()) => continue,
                        Err(error) => {
                            println!("{error}");
                            pause_for_input("\nPress Enter to continue: ");
                        }
                    }
                } else if cmd.len() == 3 {
                    match display_group_anchor_info(sdx, &cmd) {
                        Ok(()) => continue,
                        Err(error) => {
                            println!("{error}");
                            pause_for_input("\nPress Enter to continue: ");
                        }
                    }
                } else {
                    println!("Too many args for the aj command.");
                }
            }
            "cd" => match do_change_domain(sdx, &cmd) {
                Ok(()) => {
                    sdx.print();
                    display_needs(sdx);
                }
                Err(error) => {
                    println!("{error}");
                    pause_for_input("\nPress Enter to continue: ");
                }
            },
            "cs" => match do_change_state_command(sdx, &cmd) {
                Ok(()) => return,
                Err(error) => {
                    println!("{error}");
                    pause_for_input("\nPress Enter to continue: ");
                }
            },
            "dcs" => {
                sdx.print();
                print_can_do(sdx);
            }
            "dn" => match do_chosen_need(sdx, &cmd) {
                Ok(()) => return,
                Err(error) => {
                    println!("{error}");
                    pause_for_input("\nPress Enter to continue: ");
                }
            },
            "exit" | "q" | "quit" => {
                println!("Done");
                process::exit(0);
            }
            "fsd" => match store_data(sdx, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                    pause_for_input("\nPress Enter to continue: ");
                }
            },
            "gps" => match do_print_group_defining_squares_command(sdx, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                    pause_for_input("\nPress Enter to continue: ");
                }
            },
            "h" => usage(),
            "help" => usage(),
            "ppd" => match do_print_plan_details(sdx, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                    pause_for_input("\nPress Enter to continue: ");
                }
            },
            "ps" => match do_print_squares_command(sdx, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                    pause_for_input("\nPress Enter to continue: ");
                }
            },
            "psr" => match do_print_select_regions(sdx, &cmd) {
                Ok(()) => continue,
                Err(error) => {
                    println!("{error}");
                    pause_for_input("\nPress Enter to continue: ");
                }
            },
            "run" => {
                if sdx.can_do.is_empty() {
                } else {
                    do_session(sdx);
                }
            }
            "ss" => match do_sample_state_command(sdx, &cmd) {
                Ok(()) => return,
                Err(error) => {
                    println!("{error}");
                    pause_for_input("\nPress Enter to continue: ");
                }
            },
            "to-rc" => {
                match do_to_rc_command(sdx, &cmd) {
                    Ok(()) => (),
                    Err(error) => println!("{error}"),
                }
                pause_for_input("\nPress Enter to continue: ");
                return;
            }
            "step-rc" => {
                match do_step_rc_command(sdx, &cmd) {
                    Ok(()) => (),
                    Err(error) => println!("{error}"),
                }
                pause_for_input("\nPress Enter to continue: ");
                return;
            }
            _ => {
                println!("\nDid not understand command: {cmd:?}");
            }
        };
    } // end loop
} // end command_loop

/// Change the domain to a number given by user.
fn do_change_domain(sdx: &mut SessionData, cmd: &[String]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one number argument is needed for the cd command.".to_string());
    }
    // Get domain number from string
    match domain_id_from_string(sdx, &cmd[1]) {
        Ok(d_id) => {
            sdx.change_domain(d_id);
            Ok(())
        }
        Err(error) => Err(error),
    } // end match
}

/// Choose a need from a number of possibilities.
/// Attempt to satisfy the chosen need.
fn do_any_need(sdx: &mut SessionData) {
    let np_inx = sdx.choose_a_need();

    match sdx.do_a_need(np_inx) {
        Ok(()) => println!("Need satisfied"),
        Err(errstr) => println!("{errstr}"),
    }
}

/// Print details of a given plan
fn do_print_plan_details(sdx: &SessionData, cmd: &[String]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one need-number argument is needed for the ppd command.".to_string());
    }
    match cmd[1].parse::<usize>() {
        Ok(n_num) => {
            if n_num >= sdx.can_do.len() {
                Err(format!("Invalid Need Number: {}", cmd[1]))
            } else {
                let ndx = &sdx.needs[sdx.can_do[n_num].inx];

                println!("\n{} Need: {}", n_num, ndx);
                match &sdx.can_do[n_num].plans {
                    NeedPlan::AtTarget {} => println!("AT?"),
                    NeedPlan::PlanFound { plan: plnx } => sdx.print_planscorrstore_detail(plnx),
                }
                Ok(())
            }
        }
        Err(error) => Err(error.to_string()),
    }
}

/// Try to satisfy a need chosen by the user.
fn do_chosen_need(sdx: &mut SessionData, cmd: &[String]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one need-number argument is needed for the dn command.".to_string());
    }

    let dom_id = sdx.current_domain;

    match cmd[1].parse::<usize>() {
        Ok(n_num) => {
            if n_num >= sdx.can_do.len() {
                Err(format!("Invalid Need Number: {}", cmd[1]))
            } else {
                let nd_inx = sdx.can_do[n_num].inx;

                println!(
                    "\nNeed chosen: {:2} {} {}",
                    n_num,
                    sdx.needs[nd_inx],
                    match &sdx.can_do[n_num].plans {
                        NeedPlan::AtTarget {} => "At Target".to_string(),
                        NeedPlan::PlanFound { plan: plnsx } => plnsx.str_terse(),
                    }
                );

                match sdx.needs[nd_inx] {
                    SomeNeed::ToSelectRegions { .. } => (),
                    SomeNeed::ExitSelectRegions { .. } => (),
                    _ => {
                        if dom_id != sdx.needs[nd_inx].dom_id().unwrap() {
                            sdx.change_domain(sdx.needs[nd_inx].dom_id().unwrap());
                            sdx.print();
                        }
                    }
                }

                match sdx.do_a_need(n_num) {
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
fn do_change_state_command(sdx: &mut SessionData, cmd: &[String]) -> Result<(), String> {
    // Check number args.
    if cmd.len() != 2 {
        return Err("Exactly one state argument is needed for the cs command.".to_string());
    }
    // Get state from string
    match SomeState::from_str(&cmd[1]) {
        Ok(a_state) => {
            println!("Changed state to {a_state}");
            sdx.set_cur_state(a_state);
            Ok(())
        }
        Err(error) => Err(format!("\nDid not understand state, {error}")),
    } // end match
}

fn do_step_rc_command(sdx: &mut SessionData, cmd: &[String]) -> Result<(), String> {
    // Check number args.
    if cmd.len() < 2 || cmd.len() > 3 {
        return Err(
            "One, or two, regionscorr arguments are needed for the step command.".to_string(),
        );
    }

    // Get from-to regionscorrs.
    let (from, to) = if cmd.len() == 2 {
        let from = sdx.all_current_regions();

        // Get to RC, validate/massage it.
        let mut to = RegionsCorr::from_str(&cmd[1])?;
        to = sdx.validate_rc(to)?;
        (from, to)
    } else {
        // Get from RC, validate/massage it.
        let mut from = RegionsCorr::from_str(&cmd[1])?;
        from = sdx.validate_rc(from)?;

        // Get to RC, validate/massage it.
        let mut to = RegionsCorr::from_str(&cmd[2])?;
        to = sdx.validate_rc(to)?;
        (from, to)
    };

    if !from.is_congruent(&to) {
        return Err("Regionscorr given are not congruent".to_string());
    }

    if let Some(plans) = step_by_step_rc(sdx, &from, &to, None, None, 0) {
        println!("found plan for {from} -> {to}: {plans}");
        if plans
            .initial_regions()
            .is_superset_states(&sdx.all_current_states())
        {
            let cmd = pause_for_input("Press Enter to continue, or r to run ");
            if cmd == "r" || cmd == "R" {
                match sdx.run_planscorrstore(&plans) {
                    Ok(num) => println!("{num} steps run."),
                    Err(errstr) => println!("{errstr}"),
                }
            }
        }
    }

    Ok(())
}

/// Do to-rc command.
fn do_to_rc_command(sdx: &mut SessionData, cmd: &[String]) -> Result<(), String> {
    // Check number args.
    if cmd.len() < 2 {
        return Err("Exactly one RC, argument is needed for the to command.".to_string());
    }

    // Get region from string, validate/massage it.
    let mut goal_regions = RegionsCorr::from_str(&cmd[1])?;
    goal_regions = sdx.validate_rc(goal_regions)?;

    // Check if goal already satisfied.
    let cur_regs = sdx.all_current_regions();

    if goal_regions.is_superset_of(&cur_regs) {
        println!("\nCurrent_states {cur_regs} are already in region {goal_regions}");
        return Ok(());
    }

    match sdx.plan_using_least_negative_select_regions(&cur_regs, &goal_regions) {
        Ok(npln) => match npln {
            NeedPlan::AtTarget {} => Err("AtTarget not expected".to_string()),
            NeedPlan::PlanFound { plan: planx } => {
                println!("\nplan {planx}");
                match sdx.run_planscorrstore(&planx) {
                    Ok(num) => {
                        println!("{num} steps run.");
                        Ok(())
                    }
                    Err(errstr) => Err(errstr),
                }
            }
        },
        Err(errvec) => Err(format!("{:?}", errvec)),
    }
}

fn check_for_plan_completion_rc(
    from: &RegionsCorr,
    to: &RegionsCorr,
    forward_plan: &PlansCorrStore,
    backward_plan: &PlansCorrStore,
) -> Option<PlansCorrStore> {
    if forward_plan.is_not_empty()
        && backward_plan.is_not_empty()
        && forward_plan
            .result_regions()
            .intersects(&backward_plan.initial_regions())
    {
        match forward_plan.link(backward_plan) {
            Ok(planx) => return Some(planx),
            Err(errstr) => {
                println!("linking failed {forward_plan} to {backward_plan} {errstr}")
            }
        }
    } else if forward_plan.is_not_empty() && forward_plan.result_regions().intersects(to) {
        return forward_plan.restrict_result_regions(to);
    } else if backward_plan.is_not_empty() && backward_plan.initial_regions().is_superset_of(from) {
        return backward_plan.restrict_initial_regions(from);
    }
    None
}

/// Interactively step from an initial region to a goal region.
#[allow(unused_variables, unused_mut)]
fn step_by_step_rc(
    sdx: &SessionData,
    from: &RegionsCorr,
    to: &RegionsCorr,
    forward_asymmetric: Option<&RegionsCorr>,
    backward_asymmetric: Option<&RegionsCorr>,
    depth: usize,
) -> Option<PlansCorrStore> {
    let mut ret_plans: Option<PlansCorrStore> = None;

    let mut cur_from = from.clone();
    let mut cur_to = to.clone();

    let mut forward_plans = PlansCorrStore::new(vec![]);
    let mut backward_plans = PlansCorrStore::new(vec![]);

    let mut cur_start = 0;
    let num_to_display = 10;
    let mut first = true;

    // Command loop.
    loop {
        if first && depth == 0 {
            first = false;
            println!("Usage step-rc:");
            println!("    q = quit step-rc.");
            println!(
                "    r = Return to session, with a plan if its available (allows recursion, for asymmetric chaining)."
            );
            println!("    so = Start over, clear forward and backward plans at the current depth.");
            println!("    n = next slice of options.");
            println!("    p = previous slice of options.");
            println!("\n    fpop = Forward plans pop step at end.");
            println!("    bpop = Backward plans pop step at beginning.");
            println!(
                "\n    <step number> f = Use a step for Forward Chaining (FC), or Forward Asymmetric chaining (FA)."
            );
            println!(
                "    <step number> b = Use a step for Backward Chaining (BC), or Backward Asymmetric chaining (BA, or AB? ;)."
            );
            println!("\n    W: = Wanted change(s), going forward.");
            println!(
                "    U: = Unwanted change(s), going forward. Either in the step itself, and/or traversing to the step initial region."
            );
            println!("         Unwanted change(s) must eventually be reversed.");
            println!(
                "\n    If there are more wanted changes than unwanted changes, the current state is getting closer to the goal."
            );
            println!("\n    no input = redisplay options.");
        }

        let rules_to_goal = RulesCorr::new_rc_to_rc(&cur_from, &cur_to);
        let wanted_changes = rules_to_goal.as_changes();
        let cc_fil = " ".repeat(wanted_changes.strlen());

        let unwanted_changes = cur_to.unwanted_changes();

        let mut stepscorr = Vec::<StepStore>::with_capacity(from.len());

        // Init step options vector, with StepStores containing a no-op step.
        for regx in cur_from.iter() {
            stepscorr.push(StepStore::new(vec![SomeStep::new_no_op(regx)]));
        }

        // Get steps needed to make changes.
        // No limits within which the step must operate.
        let max_regs = sdx.maximum_regions();

        for (inx, rulx) in rules_to_goal.iter().enumerate() {
            let wanted_changes = rulx.as_change();
            if wanted_changes.is_not_low() {
                let steps = sdx.get_steps_domain(inx, &wanted_changes, &max_regs[inx]);
                if steps.is_empty() {
                } else {
                    stepscorr[inx].append(steps);
                }
            }
        }

        // Load a vector of step references.
        let mut step_refs = Vec::<Vec<&SomeStep>>::with_capacity(from.len());
        for stepsx in stepscorr.iter() {
            let mut tmp_vec = Vec::<&SomeStep>::with_capacity(stepsx.len());
            for stpx in stepsx.iter() {
                tmp_vec.push(stpx);
            }
            step_refs.push(tmp_vec);
        }

        println!(" ");

        // Get possible options .
        let mut options: Vec<Vec<&SomeStep>> = tools::any_one_of_each(&step_refs);
        options.remove(0); // Remove option of all no-op steps.

        // Convert to StepsCorr instances.
        let mut stepscorr_vec = Vec::<StepsCorr>::with_capacity(options.len());
        for optx in options.iter() {
            let mut tmp_store = StepsCorr::with_capacity(optx.len());
            for stpx in optx.iter() {
                tmp_store.push((*stpx).clone());
            }
            stepscorr_vec.push(tmp_store);
        }

        // Further massaging for forward or backward chaining.
        let mut steps_dis = Vec::<StepsCorr>::with_capacity(options.len());
        for stpstox in stepscorr_vec {
            // Check for forward chaining.
            let mut from_int = false;
            if cur_from.intersects(&stpstox.initial_regions()) {
                from_int = true;
            }
            // Check for backward chaining.
            let mut to_int = false;
            if cur_to.intersects(&stpstox.result_regions()) {
                to_int = true;
            }
            // Check for step that can do forward and backward chaining.
            if from_int && to_int {
                let stp_from = stpstox.restrict_initial_regions(&cur_from);
                let stp_to = stpstox.restrict_result_regions(&cur_to);

                if stp_from == stp_to {
                    steps_dis.push(stp_from);
                } else {
                    steps_dis.push(stp_from);
                    steps_dis.push(stp_to);
                }
            } else if from_int {
                // Forward chaining only.
                steps_dis.push(stpstox.restrict_initial_regions(&cur_from));
            } else if to_int {
                // Backward chaining only.
                steps_dis.push(stpstox.restrict_result_regions(&cur_to));
            } else {
                // Asymmetric chaining.
                let stp_f = stpstox.restrict_initial_regions(
                    &RulesCorr::new_region_to_region_min(&cur_from, &stpstox.initial_regions())
                        .result_regions(),
                );
                let stp_b = stpstox.restrict_result_regions(
                    &RulesCorr::new_region_from_region_min(&stpstox.result_regions(), &cur_to)
                        .initial_regions(),
                );

                if stp_f == stp_b {
                    steps_dis.push(stp_f);
                } else {
                    steps_dis.push(stp_f);
                    steps_dis.push(stp_b);
                }
            }
        }

        println!("-----------------------------------");

        if let Some(rcx) = forward_asymmetric {
            println!(
                "Original Forward: ({from} -> {to}) -> {rcx}, Current: from {cur_from} -> {cur_to}"
            );
        } else if let Some(rcx) = backward_asymmetric {
            println!(
                "Original Backward: {rcx} -> ({from} -> {to}), Current: from {cur_from} -> {cur_to}"
            );
        } else {
            println!("Original: ({from} -> {to}), Current: from {cur_from} -> {cur_to}");
        }
        println!(" ");
        println!("Forward plans:  {forward_plans}");

        println!(" ");
        println!("Backward plans: {}", backward_plans.formatted_str_from());

        println!(" ");
        println!("Current from: {cur_from} Current to: {cur_to} Wanted Changes: {wanted_changes}");
        println!(" ");

        let n_flag =
            steps_dis.len() > num_to_display && steps_dis.len() - cur_start >= num_to_display;
        let p_flag = cur_start != 0;

        if steps_dis.is_empty() {
            println!("options: None");
        } else {
            // Get max StepsCorr length.
            let mut max_len = 0;
            for stpcx in steps_dis.iter() {
                if stpcx.strlen() > max_len {
                    max_len = stpcx.strlen();
                }
            }

            if n_flag {
                println!("options (of {}):", steps_dis.len());
            } else {
                println!("options:");
            }

            let mut num_left = num_to_display;

            let mut cur_opt = cur_start;
            loop {
                if num_left == 0 || cur_opt >= steps_dis.len() {
                    break;
                }
                let optx = &steps_dis[cur_opt];
                print!("{cur_opt:3}");

                // Check if steps initial regions intersect from regions.
                if cur_from.intersects(&optx.initial_regions())
                    && cur_to.intersects(&optx.result_regions())
                {
                    print!(" DONE ");
                } else if cur_from.intersects(&optx.initial_regions()) {
                    print!(" FC   ");
                } else if cur_to.intersects(&optx.result_regions()) {
                    print!(" BC   ");
                } else {
                    print!(" ASYMM");
                }

                print!("  {optx}");
                if optx.strlen() < max_len {
                    print!("{}", " ".repeat(max_len - optx.strlen()));
                }
                num_left -= 1;
                cur_opt += 1;

                // Calc wanted_changes.
                let optx_changes = optx.changes();
                let mut optx_wanted_changes = optx_changes.intersection(&wanted_changes);
                print!(", W: {optx_wanted_changes}");

                // Calc unwanted changes.
                let optx_unwanted_changes =
                    RulesCorr::new_region_to_region_min(&cur_from, &optx.initial_regions())
                        .combine_sequence(&optx.rules())
                        .intersection_changes(&unwanted_changes);

                if optx_unwanted_changes.is_not_low() {
                    print!(", U: {optx_unwanted_changes}");
                } else {
                    print!("     {}", cc_fil);
                }

                // Check select regions.
                let rate = sdx.rate_results(&optx.result_regions());
                if rate.0 != 0 && rate.1 != 0 {
                    print!(" rate {:2} {:?}", rate.0 + rate.1, rate);
                } else if rate.0 != 0 {
                    print!(" rate {:2}", rate.0);
                } else if rate.1 != 0 {
                    print!(" rate {:2}", rate.1);
                }

                println!(" ");
            }
        }

        let pop_str = if forward_plans.is_not_empty() && backward_plans.is_not_empty() {
            ", fpop, bpop"
        } else if forward_plans.is_not_empty() {
            ", fpop"
        } else if backward_plans.is_not_empty() {
            ", bpop"
        } else {
            ""
        };

        let input_str = if let Some(ref planx) = ret_plans {
            println!("Plan found: {planx}");
            println!(" ");
            pause_for_input(&format!(
                "Depth: {depth} Enter q, r (plan){}, so: ",
                pop_str
            ))
        } else {
            let pn_opts = if p_flag && n_flag {
                ", p, n"
            } else if n_flag {
                ", n"
            } else if p_flag {
                ", p"
            } else {
                ""
            };
            println!(" ");
            pause_for_input(&format!(
                "Depth: {depth} Enter q, r (None), <step number> {}, so {}: ",
                pop_str, pn_opts
            ))
        };

        let cmd = match tools::parse_input(&input_str) {
            Ok(strvec) => strvec,
            Err(errstr) => {
                println!("{errstr}");
                vec![]
            }
        };

        // No input, or error.
        if cmd.is_empty() {
            continue;
        }

        // Do commands
        match &*cmd[0] {
            "q" | "Q" => {
                println!("Done");
                process::exit(0);
            }
            "n" | "N" => {
                // Show next slice of options, if any.
                if cur_start + num_to_display >= steps_dis.len() {
                } else {
                    cur_start += num_to_display;
                }
            }
            "p" | "P" => {
                // Show previous slice of options, if any.
                if cur_start >= num_to_display {
                    cur_start -= num_to_display;
                }
            }
            "r" | "R" => {
                return ret_plans;
            }
            "fpop" | "FPOP" => {
                if let Some(_top_from) = forward_plans.pop() {
                    if forward_plans.is_empty() {
                        cur_from = from.clone();
                    } else {
                        cur_from = forward_plans.result_regions();
                    }
                    ret_plans = None;
                } else {
                    println!("forward_plans is empty");
                }
            }
            "bpop" | "BPOP" => {
                if let Some(_stp_back) = backward_plans.pop_first() {
                    if backward_plans.is_empty() {
                        cur_to = to.clone();
                    } else {
                        cur_to = backward_plans.initial_regions();
                    }
                    ret_plans = None;
                } else {
                    println!("backward_plans is empty");
                }
            }
            "so" | "SO" => {
                forward_plans = PlansCorrStore::new(vec![]);
                backward_plans = PlansCorrStore::new(vec![]);
                cur_from = from.clone();
                cur_to = to.clone();
                ret_plans = None;
            }
            _ => (),
        }
        // Check for step number.
        match cmd[0].parse::<usize>() {
            Ok(num) => {
                if num >= steps_dis.len() {
                    println!("Step number is too high");
                } else if steps_dis[num].initial_regions_intersect(&cur_from)
                    && steps_dis[num].result_regions_intersect(&cur_to)
                {
                     // Do forward chaining.
                    let stp_tmp = steps_dis[num].restrict_initial_regions(&cur_from);
                    match forward_plans.push_link(PlansCorr::new_from_stepscorr(&stp_tmp)) {
                        Ok(()) => {
                            ret_plans = check_for_plan_completion_rc(
                                from,
                                to,
                                &forward_plans,
                                &backward_plans,
                            );
                            cur_from = forward_plans.result_regions();
                        }
                        Err(errstr) => println!("forward plan push failed {errstr}"),
                    }
                } else if steps_dis[num].initial_regions_intersect(&cur_from) {
                    // Do forward chaining.
                    let stp_tmp = steps_dis[num].restrict_initial_regions(&cur_from);
                    match forward_plans.push_link(PlansCorr::new_from_stepscorr(&stp_tmp)) {
                        Ok(()) => {
                            ret_plans = check_for_plan_completion_rc(
                                from,
                                to,
                                &forward_plans,
                                &backward_plans,
                            );
                            cur_from = forward_plans.result_regions();
                        }
                        Err(errstr) => println!("forward plan push failed {errstr}"),
                    }
                } else if steps_dis[num].result_regions_intersect(&cur_to) {
                    // Do backward chaining
                    let stp_tmp = steps_dis[num].restrict_result_regions(&cur_to);
                    match backward_plans.push_first_link(PlansCorr::new_from_stepscorr(&stp_tmp)) {
                        Ok(()) => {
                            ret_plans = check_for_plan_completion_rc(
                                from,
                                to,
                                &forward_plans,
                                &backward_plans,
                            );
                            cur_to = backward_plans.initial_regions();
                        }
                        Err(errstr) => println!("backward plan push_first failed {errstr}"),
                    }
                } else {
                    //TODO Asymmetric
                    println!("ASYMM not coded yet");
                }
            } // end Ok(num)
            _ => ()
        } // match cmd[0].parse::<usize>()
        // } // end cmp.len() == 2
    } // end command loop.
}

/// Do sample-state command.
fn do_sample_state_command(sdx: &mut SessionData, cmd: &Vec<String>) -> Result<(), String> {
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
        let dom_id = sdx.current_domain;
        //let domx = sdx.find(dom_id).expect("SNH");

        println!("Act {act_id} sample curent state.");
        sdx.take_action(dom_id, act_id);
        return Ok(());
    }

    if cmd.len() == 3 {
        // Get state from string
        let a_state = match SomeState::from_str(&cmd[2]) {
            Ok(a_state) => a_state,
            Err(error) => {
                return Err(error);
            }
        };

        let dom_id = sdx.current_domain;

        let domx = sdx.find_domain(dom_id).expect("SNH");

        if a_state.num_bits() != domx.num_bits() {
            return Err("State does not have the same number of bits as the CCD.".to_string());
        }

        println!("Act {act_id} sample State {a_state}");
        sdx.set_domain_state(dom_id, a_state);
        sdx.take_action(dom_id, act_id);
        return Ok(());
    }

    if cmd.len() == 4 {
        // Take arbitrary sample with <action num> <initial-state> <result-state>, don't update current state
        // This tends to break things for an action, unless all samples are arbitrary.
        // Useful for testing a wholly different series of samples/results.
        // Using the command: ss  action-number  initial-state  result-state
        // e.g. ss  0  s1010  s1111

        // Get i-state from string
        let i_state = match SomeState::from_str(&cmd[2]) {
            Ok(i_state) => i_state,
            Err(error) => {
                return Err(error);
            }
        };

        // Get r-state from string
        let r_state = match SomeState::from_str(&cmd[3]) {
            Ok(r_state) => r_state,
            Err(error) => {
                return Err(error);
            }
        };

        if i_state.num_bits() != r_state.num_bits() {
            return Err("States do not have the same number of bits.".to_string());
        }

        let dom_id = sdx.current_domain;
        let domx = sdx.find_domain(dom_id).expect("SNH");

        if i_state.num_bits() != domx.num_bits() {
            return Err("States do not have the same number of bits as the CCD.".to_string());
        }

        println!("Act {act_id} take sample {i_state} -> {r_state}");
        let smpl = SomeSample::new(i_state, r_state);
        sdx.eval_sample_arbitrary(dom_id, act_id, &smpl);
        return Ok(());
    } // end command ss 4

    Err(format!("Did not understand {cmd:?}"))
}

/// Display anchors, rating, and adjacent squares, for an action.
/// For a group that has an anchor, and is limited, the number edges, that can be changed with actions,
/// should equal the sum of the first two number of the rating.
fn display_action_anchor_info(sdx: &mut SessionData, cmd: &[String]) -> Result<(), String> {
    let dom_id = sdx.current_domain;

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
    let domx = sdx.find_domain(dom_id).expect("SNH");
    domx.display_action_anchor_info(act_id)
}

/// Do print-squares command.
fn do_print_select_regions(sdx: &SessionData, cmd: &[String]) -> Result<(), String> {
    if cmd.len() != 1 {
        return Err("No arguments needed for the psr command".to_string());
    }

    for selx in sdx.select.iter() {
        println!("{}", selx);
    }
    Ok(())
}

/// Do print-squares command.
fn do_print_squares_command(sdx: &SessionData, cmd: &Vec<String>) -> Result<(), String> {
    let dom_id = sdx.current_domain;
    let domx = sdx.find_domain(dom_id).expect("SNH");

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

    if act_id < domx.actions.len() {
    } else {
        return Err(format!("Invalid action ID for domain {dom_id}"));
    }

    if cmd.len() == 2 {
        println!(
            "Squares of Action {} are:\n{}\n",
            act_id, domx.actions[act_id].squares
        );
        return Ok(());
    }

    if cmd.len() == 3 {
        // Get region from command.
        let aregion = SomeRegion::from_str(&cmd[2])?;

        if aregion.num_bits() != domx.cur_state.num_bits() {
            return Err("Invalid number of bits in region.".to_string());
        }

        println!("Squares of Action {act_id} in region {aregion} are:\n");

        let sqrs = domx.actions[act_id].squares.squares_in_reg(&aregion);
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
                let sqrx = domx.actions[act_id].squares.find(stax).unwrap();
                if sqrx.pnc {
                    form_group = false;
                }
            }
        } else if let Some(ruls) = rules {
            rules_str = ruls.to_string();
            for stax in non_pn_stas.iter() {
                let sqrx = domx.actions[act_id].squares.find(stax).expect(
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
                "    Min Pn: {min_pn} Max Pn: {max_pn} Rules: {rules_str} Can form group: {form_group}"
            );
        } else {
            println!("    Min Pn: {min_pn} Max Pn: {max_pn} Can form group: {form_group}");
        }
        return Ok(());
    }

    Err(format!("Did not understand {cmd:?}"))
}

/// Do adjacent-anchor command.
fn display_group_anchor_info(sdx: &SessionData, cmd: &Vec<String>) -> Result<(), String> {
    let dom_id = sdx.current_domain;
    let domx = sdx.find_domain(dom_id).expect("SNH");

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

    let aregion = SomeRegion::from_str(&cmd[2])?;

    if aregion.num_bits() != domx.cur_state.num_bits() {
        return Err("Invalid number of bits in region given".to_string());
    }

    domx.display_group_anchor_info(act_id, &aregion)
}

/// Do print-group-defining-squares command.
fn do_print_group_defining_squares_command(
    sdx: &SessionData,
    cmd: &Vec<String>,
) -> Result<(), String> {
    let dom_id = sdx.current_domain;
    let domx = sdx.find_domain(dom_id).expect("SNH");
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

    let aregion = SomeRegion::from_str(&cmd[2])?;

    if aregion.num_bits() != domx.cur_state.num_bits() {
        return Err("Invalid number of bits in region given".to_string());
    }

    if let Some(grpx) = domx.actions[act_id].groups.find(&aregion) {
        for stax in grpx.region.states.iter() {
            if let Some(sqrx) = domx.actions[act_id].squares.find(stax) {
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
    println!(
        "\n    <invoke>                  - Run default.kmp interactively, press Enter for each step."
    );
    println!(
        "\n    <invoke> 1                - Run default.kmp non-interactively, stop, in interactive mode, when no needs can be done."
    );
    println!(
        "\n    <invoke> <number times>   - Run default.kmp a number (> 1) times. Exit with step and duration statistics."
    );
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
    println!(
        "\n\n    aj <act num>             - For an Action in the CDD, display all groups anchor, and adJacent, info."
    );
    println!(
        "    aj <act num> <region>      For an Action in the CDD, display group anchor, and adJacent, info."
    );
    println!("\n    cs <state>               - Change State, an arbitrary change, for the CDD.");
    println!("\n    dn <need number>         - Do a particular Need from the can-do need list.");
    println!(
        "\n    dcs                      - Display Current State, and domain.  After a number of commands,"
    );
    println!(
        "                               the current state scrolls off screen, this might be useful."
    );
    println!("\n    fsd <path>               - File Store Data.");
    println!(
        "\n    gps <act num> <region>   - Group Print Squares that define the group region, of a given action, of the CDD."
    );
    println!(
        "\n    ppd <need number>        - Print the Plan Details for a given need number in the can-do list."
    );
    println!("\n    ps <act num>             - Print all Squares for an action, of the CDD.");
    println!(
        "    ps <act num> <region>      Print Squares in a given action and region, of the CDD."
    );
    println!("\n    psr                      - Print Select Regions.");
    println!("\n    run                      - Run until there are no needs that can be done.");
    println!(
        "\n    ss <act num>             - Sample the current State, for a given action, for the CDD."
    );
    println!(
        "    ss <act num> <state>       Sample State for a given action and state, for the CDD."
    );
    println!(
        "    ss <act num> <s1> <s2>     Sample State, for a given action, state and arbitrary result, for the CDD."
    );
    //    println!("\n    to <region>              - Change the current state TO within a region, by calculating and executing a plan.");
    println!(
        "\n    to-rc <RC>               - Change the current states TO within a set of regions."
    );
    println!("\n                               Like: to-rc RC[r1010, r1_000]");
    println!(
        "                               To target less than all domains: to-rc RC[r1010, rX_XXXX]"
    );
    //    println!("\n    step <region> <region>    Interactively use rules to navigate, step by step, from an initial region to a goal region.");
    //    println!("                              This can be run anytime, but its probably more interesting to run with a fully developed set of rules.");
    //    println!("                              The fsd command can store a full session.  Later, the program can be run with the data file as an argument,");
    //    println!(
    //        "                              giving immediate access to a fully develped set of rules."
    //    );
    println!(
        "\n    step_rc <RC> [<RC>]      - Interactively use rules to navigate, step by step, from an initial set of regions to a goal set of regions."
    );
    println!("\n                               Like: step_rc RC[r0101, r111] RC[r1001, r100]");
    println!("\n                               Like: step_rc RC[r1001, r100] (means current state to given RC)");
    println!(
        "                               To target less than all domains: step_rc RC[rXXXX, s111] RC[rXXXX, s100], or step_rc RC[s111] RC[s100]"
    );
    println!(
        "\n                               This can be run anytime, but its probably more interesting to run with a fully developed set of rules."
    );
    println!(
        "                               The fsd command can store a full session.  Later, the program can be run with the data file as an argument,"
    );
    println!(
        "                               giving immediate access to a fully develped set of rules."
    );
    println!(
        "\n    A domain number is an integer, zero or greater, where such a domain exists. CDD means the Currently Displayed Domain."
    );
    println!("\n    An action number is an integer, zero or greater, where such an action exists.");
    println!(
        "\n    A need number is an integer, zero or greater, in a displayed list of needs that can be done."
    );
    println!(
        "\n    A state starts with an 's' character, followed by one, or more, binary digits."
    );
    println!(
        "\n    A region starts with an 'r' character, followed by one, or more, zero, one, X or x characters."
    );
    println!(
        "    A region displayed with a trailing \"+\" indicates the region is formed by more than two states.  Two states is a goal."
    );
    println!(
        "\n    A region, or state, may contain the separator '_', which will be ignored. All bit positions must be specified."
    );
    println!(
        "\n    A state can be used instead of a region, it will be translated to a region with no X-bits."
    );
    println!(
        "\n    An RC must have at least one region. An RC can have missing, and out of order regions, when the regions use different numbers of bits."
    );
    println!(
        "\n    pn stands for pattern number, the number of different samples. 1 = 1 kind of result, 2 = 2 kinds of results, in order. U = upredictable."
    );
    println!("\n    pnc stands for pattern number confirmed, by enough extra samples.");
    println!("          The bar for this is fairly low.");
    println!(
        "          Additional samples can cycle the pn and pnc values through all possible combinations."
    );
    println!(
        "\n    A Select Region is an arbitrary region, across all domains, with a given value, positive or negative."
    );
    println!(
        "\n        Plans, to satisfy a need, are made to avoid negative select regions, if possible."
    );
    println!(
        "\n        Finding the current state within a negative select region, the program will attempt to exit the region."
    );
    println!("\n    A plan to satisfy a need may be shown in one of two ways.");
    println!("\n        At Target - The current state is within the need target.");
    println!(
        "\n        PCS[PC[P[0:1], P[1:3]], PC[P[0:3,2]]]/3/6/-1 - Changes need to be made to the current state to be within the need target."
    );
    println!(
        "\n            PCS[ ... ]/3/6/-1 - A Plan Corresponding (per domain) Store, to change 3 bits, using plans that change 6 bits, passing through -1 valued regions."
    );
    println!(
        "\n            PC[ .. ] - A Plan Corresponding (per domain). No more than one plan per domain. If more than one plan, the plans will be run in parallel."
    );
    println!("\n            P[ .. ] - A Plan. One, or more, actions for a single domain.");
    println!("\n            0:3,2 - For domain 0, run action 3, then action 2.");
    println!(
        "\n        Once the current state is within the need target, most (but not all) needs require an additional action to get a sample."
    );
    println!(
        "\n    Needs that cannot be done.  Lets say the current state is s0000, there is a need for s1000, and no action that changes"
    );
    println!(
        "    the left-most bit.  Using the command \"cs s1000\" will get things moving again."
    );
    println!(
        "\n        If there is only one square that makes a change that no other square makes, and that square has not happened to be sampled,"
    );
    println!(
        "        the understanding of the logic will be deficient until the square is sampled."
    );
    println!(
        "\n    After no more needs can be done, positive select region seeking logic will be used."
    );
    println!("    Repeatedly pressing the Enter key will increase the boredom duration.");
    println!(
        "    If there is more than one positive select region, when the boredom value reaches the the select region value,"
    );
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
fn load_data(path_str: &str) -> Result<String, String> {
    let path = Path::new(path_str);

    // Open a file, returns `io::Result<File>`
    match File::open(path) {
        Ok(mut afile) => {
            let mut file_content = String::new();
            match afile.read_to_string(&mut file_content) {
                Ok(_) => Ok(file_content),
                Err(why) => Err(format!("main::load_data: {} {why}", path.display())),
            }
        }
        Err(why) => Err(format!("main::load_data: {} {why}", path.display())),
    } // end match open file
}

/// Store current data to a given path string.
fn store_data(sdx: &SessionData, cmd: &Vec<String>) -> Result<(), String> {
    if cmd.len() != 2 {
        return Err(format!("Did not understand {cmd:?}"));
    }

    println!("store_data: to {} step {}", cmd[1], sdx.step_num);
    let path_str = &cmd[1];
    let serialized_r = serde_yaml_ng::to_string(sdx);

    match serialized_r {
        Ok(serialized) => {
            let path = Path::new(path_str);
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
        Err(error) => Err(format!("Couldn't serialize: {error}")),
    } // end match serialized_r
} // end store_data

#[cfg(test)]
mod tests {
    use super::*;
    use crate::regionscorr::RegionsCorr;
    use crate::selectregions::SelectRegions;
    use crate::statescorr::StatesCorr;
    use crate::target::ATarget;

    #[test]
    fn do_to_rc_command1() -> Result<(), String> {
        // Create SessionData, with mutually exclusive rules.
        let mut sdx = SessionData::from_str(
            "SD[DS[
                DOMAIN[
                    ACT[[01/XX/00/XX],
                        [10/XX/01/XX]]],
                DOMAIN[
                    ACT[[Xx_XX/XX/XX/XX]]]],
                SC[s1010, s0_1010],
                ]",
        )?;
        sdx.take_action_arbitrary(0, 1, &SomeState::from_str("s0000")?);
        sdx.take_action_arbitrary(0, 1, &SomeState::from_str("s0101")?);
        sdx.take_action_arbitrary(0, 1, &SomeState::from_str("s1100")?);
        sdx.take_action_arbitrary(0, 1, &SomeState::from_str("s1001")?);

        sdx.take_action_arbitrary(1, 1, &SomeState::from_str("s0_0000")?);
        sdx.take_action_arbitrary(1, 1, &SomeState::from_str("s1_1111")?);

        sdx.print();

        // Command that should fail.
        let cmd = match tools::parse_input("to-rc RC[s1110, X_XXXX]") {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(errstr),
        };
        match do_to_rc_command(&mut sdx, &cmd) {
            Ok(()) => {
                sdx.print();
                return Err(format!("command changed region?"));
            }
            Err(errstr) => {
                if errstr == "[\"No plan found\"]" {
                } else {
                    return Err(format!("{errstr}"));
                }
            }
        }

        // Change domain 0 only.
        sdx.set_cur_states(&StatesCorr::from_str("SC[s0001, s0_1010]")?);
        let cmd = match tools::parse_input("to-rc RC[s1001, rX_XXXX]") {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(errstr),
        };

        match do_to_rc_command(&mut sdx, &cmd) {
            Ok(()) => {
                sdx.print();
                assert!(format!("{}", sdx.all_current_states()) == "SC[s1001, s0_1010]");
            }
            Err(errstr) => return Err(format!("{errstr}")),
        }

        // Change domain 1 only.
        sdx.set_cur_states(&StatesCorr::from_str("SC[s0001, s0_1010]")?);
        let cmd = match tools::parse_input("to-rc RC[rXXXX, s1_1010]") {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(errstr),
        };

        match do_to_rc_command(&mut sdx, &cmd) {
            Ok(()) => {
                sdx.print();
                assert!(format!("{}", sdx.all_current_states()) == "SC[s0001, s1_1010]");
            }
            Err(errstr) => return Err(format!("{errstr}")),
        }

        // Change both domains.
        sdx.set_cur_states(&StatesCorr::from_str("SC[s0001, s0_1010]")?);
        let cmd = match tools::parse_input("to-rc RC[s1001, s1_1010]") {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(errstr),
        };

        match do_to_rc_command(&mut sdx, &cmd) {
            Ok(()) => {
                sdx.print();
                assert!(format!("{}", sdx.all_current_states()) == "SC[s1001, s1_1010]");
                Ok(())
            }
            Err(errstr) => Err(format!("{errstr}")),
        }
    }

    /// Force a misapprehension of a rule for action 4.
    /// Develop actions 0-3 in the normal way.
    /// In action 4, testing squares adjacent to the anchor, using actions 0-3, should invalidate the first group.
    #[test]
    fn bad_start() -> Result<(), String> {
        // Create SessionData, set up group XX/XX/Xx/XX for action 4.
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]],
            ACT[[XX/XX/00/X1], [XX/XX/11/X0]]]]
        ]",
        )?;

        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s0001")?);
        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s0001")?);
        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s0001")?);

        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s1110")?);
        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s1110")?);
        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s1110")?);

        sdx.print();

        assert!(sdx.find_domain(0).expect("SNH").actions[5].number_groups() == 1);
        let grpx = sdx.find_domain(0).expect("SNH").actions[5]
            .groups
            .find(&SomeRegion::from_str("XXXX")?)
            .unwrap();
        println!("Group {} found", grpx.region);

        do_session(&mut sdx); // Figure other actions, and test group in action 4.
        sdx.print();

        assert!(sdx.find_domain(0).expect("SNH").actions[5].number_groups() == 2); // s/b XX/XX/10/X0, XX/XX/01/X1.
        let grpx = sdx.find_domain(0).expect("SNH").actions[5]
            .groups
            .find(&SomeRegion::from_str("XX1X")?)
            .unwrap();
        println!("Group {} found", grpx.region);
        let grpx = sdx.find_domain(0).expect("SNH").actions[5]
            .groups
            .find(&SomeRegion::from_str("XX0X")?)
            .unwrap();
        println!("Group {} found", grpx.region);

        Ok(())
    }

    /// Test the cleanup of unneeded groups.
    #[test]
    fn symmetric_adjacent_overlap_cleanup() -> Result<(), String> {
        // Create SessionData, 6 groups.
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]],
            ACT[[XX/11/01/Xx], [11/XX/10/Xx], [Xx/00/00/XX], [01/XX/11/XX]]]],
        ]",
        )?;
        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s0000")?);
        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s0011")?);
        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s0100")?);
        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s0110")?);
        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s1101")?);
        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s1110")?);
        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s1001")?);
        sdx.take_action_arbitrary(0, 5, &SomeState::from_str("s1011")?);

        println!("sdx {sdx}");
        assert!(sdx.find_domain(0).expect("SNH").actions[5].groups.len() == 6);

        // Limit groups, delete 00XX and 11XX.
        do_session(&mut sdx);
        sdx.get_needs();

        sdx.print();
        assert!(sdx.find_domain(0).expect("SNH").actions[0].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[1].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[2].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[3].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[4].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[5].groups.len() == 4);

        // Check action 5 primary groups.
        if let Some(grpx) = sdx.find_domain(0).expect("SNH").actions[5]
            .groups
            .find(&SomeRegion::from_str("rX10X")?)
        {
            assert!(grpx.limited);
        } else {
            return Err("Group rX10X not found?".to_string());
        }
        if let Some(grpx) = sdx.find_domain(0).expect("SNH").actions[5]
            .groups
            .find(&SomeRegion::from_str("r1X1X")?)
        {
            assert!(grpx.limited);
        } else {
            return Err("Group r1X1X not found?".to_string());
        }
        if let Some(grpx) = sdx.find_domain(0).expect("SNH").actions[5]
            .groups
            .find(&SomeRegion::from_str("rX00X")?)
        {
            assert!(grpx.limited);
        } else {
            return Err("Group rX00X not found?".to_string());
        }
        if let Some(grpx) = sdx.find_domain(0).expect("SNH").actions[5]
            .groups
            .find(&SomeRegion::from_str("r0X1X")?)
        {
            assert!(grpx.limited);
        } else {
            return Err("Group r0X1X not found?".to_string());
        }
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
        generate_and_display_needs(&mut sdx);
        assert!(sdx.boredom == 0);
        //println!("needs {}", sdx.needs);
        assert!(sdx.needs.len() == 1);
        assert!(sdx.needs.contains_similar_need(
            "ToSelectRegions",
            &ATarget::SelectRegions {
                select: SelectRegions::from_str("SR[RC[r1000], 1]")?
            }
        ));

        sdx.print();
        assert!(sdx.find_domain(0).expect("SNH").actions[0].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[1].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[2].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[3].groups.len() == 1);

        // Move to positive region.
        do_any_need(&mut sdx);

        assert!(sdx.find_domain(0).expect("SNH").cur_state == (SomeState::from_str("s1000")?));

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
        assert!(sdx.find_domain(0).expect("SNH").actions[0].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[1].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[2].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[3].groups.len() == 1);

        // Inc boredom by 1.
        generate_and_display_needs(&mut sdx);
        println!("needs {}", sdx.needs);
        println!("cant_do {}", sdx.cant_do.len());
        println!("can_do {}", sdx.can_do.len());
        //assert!(can_do.is_empty());
        // Inc boredom by 1.
        generate_and_display_needs(&mut sdx);
        println!("needs {}", sdx.needs);
        println!("cant_do {}", sdx.cant_do.len());
        println!("can_do {}", sdx.can_do.len());

        //assert!(can_do.is_empty());

        // Should want to try state 0111 now.
        generate_and_display_needs(&mut sdx);
        assert!(sdx.needs.len() == 1);
        assert!(sdx.needs.contains_similar_need(
            "ToSelectRegions",
            &ATarget::SelectRegions {
                select: SelectRegions::from_str("SR[RC[r0111], 2]")?
            }
        ));

        // Move to positive 0111..
        do_any_need(&mut sdx);
        generate_and_display_needs(&mut sdx);
        assert!(sdx.needs.len() == 1);
        assert!(
            sdx.needs.contains_similar_need(
                "ExitSelectRegions",
                &ATarget::DomainRegions {
                    regions: RegionsCorr::from_str("RC[rXXX0]")?
                }
            ) || sdx.needs.contains_similar_need(
                "ExitSelectRegions",
                &ATarget::DomainRegions {
                    regions: RegionsCorr::from_str("RC[rX0XX]")?
                }
            ) || sdx.needs.contains_similar_need(
                "ExitSelectRegions",
                &ATarget::DomainRegions {
                    regions: RegionsCorr::from_str("RC[rXX0X]")?
                }
            )
        );

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
        assert!(sdx.find_domain(0).expect("SNH").actions[0].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[1].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[2].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[3].groups.len() == 1);

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
        assert!(sdx.find_domain(0).expect("SNH").actions[0].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[1].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[2].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[3].groups.len() == 1);

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
        assert!(sdx.find_domain(0).expect("SNH").actions[0].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[1].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[2].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[3].groups.len() == 1);

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
        assert!(sdx.find_domain(0).expect("SNH").actions[0].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[1].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[2].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[3].groups.len() == 1);

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
        assert!(sdx.find_domain(0).expect("SNH").actions[0].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[1].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[2].groups.len() == 1);
        assert!(sdx.find_domain(0).expect("SNH").actions[3].groups.len() == 1);

        Ok(())
    }
}
