// Unorthodox Expert System

//#![allow(
// dead_code,
//  unused_variables,
//  unused_macros,
//  unused_imports,
//  unused_assignments,
//  unused_mut
//)]

mod bits;
//use bits::SomeBits;
mod bitsstore;
mod state;
//use state::SomeState;
mod mask;
mod maskstore;
mod region;
use region::SomeRegion;
mod action;
mod actionstore;
mod group;
mod groupstore;
mod need;
mod regionstore;
mod resultstore;
mod rule;
//use rule::SomeRule;
mod rulestore;
mod square;
//use crate::square::SomeSquare;
mod squarestore;
mod statestore;
use need::SomeNeed;
mod needstore;
use crate::needstore::NeedStore;
mod plan;
mod step;
mod stepstore;
use plan::SomePlan;
mod combinable;
mod domain;
mod pn;
use domain::SomeDomain;
mod actions;
mod tests;
use actions::{action0, action1, action2, action3, action4, action5};

use std::io;
use std::io::Write;
use std::process; // bring flush() into scope
extern crate rand;
use rand::Rng;

// Push an item into a random position in a vector
fn vec_rand_push<T>(avec: &mut Vec<T>, num: T) {
    if avec.len() > 1 {
        let inx = rand::thread_rng().gen_range(0, avec.len() + 1); // last in range is exclusive, (0,1) will always return 0.
                                                                   //println!("random inx {} len {}", inx, avec.len());
        if inx == avec.len() {
            avec.push(num);
        } else {
            avec.insert(inx, num);
        }
    } else {
        avec.push(num);
    }
}

fn init_domain(num_ints: usize, cur: &str) -> SomeDomain {
    let mut dmx = SomeDomain::new(num_ints, cur);
    dmx.add_action(action0, 0);
    dmx.add_action(action1, 0);
    dmx.add_action(action2, 0);
    dmx.add_action(action3, 0);
    dmx.add_action(action4, 0);
    dmx.add_action(action5, 0);
    dmx
}

fn main() {
    let mut dm1 = init_domain(1, "s0001"); // init state to 1 u8 integer of bits, may be higher
    let num_actions = dm1.num_actions();

    //pause_for_enter("");

    usage();
    let mut step = 0;

    loop {
        step += 1;
        println!(
            "\nStep: {} Current State: {}  Max Region: {}",
            &step, &dm1.cur_state, &dm1.max_region
        );

        print!("\nEnter command: ");
        io::stdout().flush().unwrap();

        let mut guess = String::new();
        io::stdin()
            .read_line(&mut guess)
            .expect("Failed to read line");
        //println!("The command is: {} len {} char1: {:?}", guess, guess.len(), guess.chars());

        let mut cmd = Vec::<String>::with_capacity(10);

        for word in guess.split_whitespace() {
            //println!("word: {} is {}", word_count, word);
            cmd.push(word.to_ascii_lowercase());
        }

        // Default command, just press Enter
        if cmd.len() == 0 {
            // Display new Actions and needs
            println!("\nActs: {}", dm1.actions);
            let nds: NeedStore = dm1.get_needs();

            if nds.len() > 0 {
                println!("\nAction needs: {}", nds);

                //let curst = dm1.cur_state.clone();
                if satisfy_need(&mut dm1, &nds) {
                    println!("\nActs: {}", dm1.actions);
                //println!("next_state = {}", &dm1.cur_state);
                } else {
                    println!("no need satisfied");
                }
            } else {
                println!("\nAction needs: None");
            }
            continue;
        } // end zero length commands

        // Handle one-word commands
        if cmd.len() == 1 {
            // Quit with q , exit, quit
            if cmd[0] == "q" || cmd[0] == "exit" || cmd[0] == "quit" {
                println!("Done");
                process::exit(0);
            } else if cmd[0] == "h" || cmd[0] == "help" {
                usage();
                continue;
            }

            // Print Action command
            if cmd[0] == "pa" {
                println!("\nActs: {}", dm1.actions);
                continue;
            } // end pa command
              // Print Action command

            if cmd[0] == "gn" {
                println!("\nActs: {}", dm1.get_needs());
                continue;
            } // end gn command

            if cmd[0] == "tests" {
                tests::run_tests();
                continue;
            }

            println!("\nDid not understand command: {}", guess);
            continue;
        } // end one-word commands

        // Handle two-word commands
        if cmd.len() == 2 {
            // Arbitrary change state
            if cmd[0] == "cs" {
                let state_r = dm1.state_from_string(&cmd[1]);
                match state_r {
                    Ok(a_state) => {
                        println!("Change state to {}", a_state);
                        dm1.cur_state = a_state.clone();

                        continue;
                    }
                    Err(error) => {
                        if error == 1 {
                            println!("\nDid not understand state, should start with s");
                        } else {
                            println!("\nDid not understand state, invalid character");
                        }
                    }
                } // end match
            }

            // Change current-state to a region
            if cmd[0] == "to" {
                let region_r = dm1.region_from_string(&cmd[1]);
                match region_r {
                    Ok(goal_region) => {
                        println!(
                            "Change Current_state {} to region {}",
                            dm1.cur_state, goal_region
                        );
                        if goal_region.is_superset_of_state(&dm1.cur_state) {
                            println!(
                                "current_state {} is already in region {}",
                                dm1.cur_state, goal_region
                            );
                        } else {
                            if let Some(pln) = dm1.make_plan(&goal_region) {
                                println!("Plan is {}", pln);

                                // Do the plan
                                dm1.run_plan(&pln);
                            } else {
                                println!("No plan found");
                            }
                        }
                    }
                    Err(error) => {
                        if error == 1 {
                            println!("\nDid not understand region, should start with r");
                        } else {
                            println!("\nDid not understand region, invalid character");
                        }
                    }
                } // end match region_r
                continue;
            } // end to command

            // Display InBetween needs for an action
            if cmd[0] == "ibn" {
                let act_num = cmd[1].parse().unwrap_or_else(|_err| num_actions);
                if act_num >= num_actions {
                    println!("\nInvalid action number");
                    continue;
                }

                let ndx = &dm1.actions[act_num].in_between_needs(&dm1.cur_state);

                println!("InBetweenNeeds are {}", &ndx);
                continue;
            } // end ibn <act> command

            // Act sample current state
            if cmd[0] == "ss" {
                let act_num = cmd[1].parse().unwrap_or_else(|_err| num_actions);
                if act_num >= num_actions {
                    println!("\nInvalid action number");
                    continue;
                }

                println!("Act {} sample State {}", act_num, dm1.cur_state);
                dm1.take_action_need(&SomeNeed::StateNotInGroup {
                    act_num: act_num,
                    targ_state: dm1.cur_state.clone(),
                });
                continue;
            } // end sc command

            // Print Action command
            if cmd[0] == "pa" {
                let act_num = cmd[1].parse().unwrap_or_else(|_err| num_actions);
                if act_num >= num_actions {
                    println!("\nInvalid action number");
                    continue;
                }

                println!("{}", &dm1.actions[act_num]);
                continue;
            } // end pa command

            // Act print squares only in one region
            if cmd[0] == "g1" {
                let act_num = cmd[1].parse().unwrap_or_else(|_err| num_actions);
                if act_num >= num_actions {
                    println!("\nInvalid action number");
                    continue;
                }

                let sqrs = dm1.actions[act_num]
                    .squares
                    .states_in_1_region(&dm1.actions[act_num].groups.regions());
                println!("Act {} State in one group {}", act_num, &sqrs);
                continue;
            } // end g1 command

            // Print Squares in action, ps <act num>
            if cmd[0] == "ps" {
                let act_num = cmd[1].parse().unwrap_or_else(|_err| num_actions);
                if act_num >= num_actions {
                    println!("\nInvalid action number");
                    continue;
                }

                println!(
                    "Squares in Action {} are \n{}\n",
                    &act_num, &dm1.actions[act_num].squares
                );
                continue;
            } // end ps command

            println!("\nDid not understand command: {}", guess);
            continue;
        } // end two-word commands

        // Handle three-word commands
        if cmd.len() == 3 {
            // Satisfy an InBetween need
            if cmd[0] == "ibn" {
                let act_num = cmd[1].parse().unwrap_or_else(|_err| num_actions);
                if act_num >= num_actions {
                    println!("\nInvalid action number");
                    continue;
                }

                let state_r = dm1.state_from_string(&cmd[2]);
                match state_r {
                    Ok(a_state) => {
                        println!("Act {} sample State {}", act_num, a_state);
                        dm1.cur_state = a_state.clone();

                        let ndxs = &dm1.actions[act_num].in_between_needs(&dm1.cur_state);

                        println!("InBetweenNeeds are {}", &ndxs);

                        for ndx in ndxs.iter() {
                            if ndx.target() == SomeRegion::new(&a_state, &a_state) {
                                // run the sample and need
                                dm1.take_action_need(&ndx);
                                break;
                            }
                        }

                        continue;
                    }
                    Err(error) => {
                        if error == 1 {
                            println!("\nDid not understand state, should start with s");
                        } else {
                            println!("\nDid not understand state, invalid character");
                        }
                    }
                } // end match

                continue;
            } // end ibn <act> <state> command

            // Sample State (ss) with <action num> <state>
            if cmd[0] == "ss" {
                let act_num = cmd[1].parse().unwrap_or_else(|_err| num_actions);
                if act_num >= num_actions {
                    println!("\nInvalid action number");
                    continue;
                }

                let state_r = dm1.state_from_string(&cmd[2]);
                match state_r {
                    Ok(a_state) => {
                        println!("Act {} sample State {}", act_num, a_state);
                        dm1.cur_state = a_state.clone();
                        dm1.take_action_need(&SomeNeed::StateNotInGroup {
                            act_num: act_num,
                            targ_state: a_state,
                        });
                        continue;
                    }
                    Err(error) => {
                        if error == 1 {
                            println!("\nDid not understand state, should start with s");
                        } else {
                            println!("\nDid not understand state, invalid character");
                        }
                    }
                } // end match state_r
                continue;
            } // end ss command

            // Print Squares in region, ps <act num> r<region bits>
            if cmd[0] == "ps" {
                let act_num = cmd[1].parse().unwrap_or_else(|_err| num_actions);
                if act_num >= num_actions {
                    println!("\nInvalid action number");
                    continue;
                }

                if let Ok(aregion) = dm1.region_from_string(&cmd[2]) {
                    println!(
                        "Squares of Act {} in region {} are \n{}\n",
                        &act_num,
                        &aregion,
                        &dm1.actions[act_num].squares.stas_in_reg(&aregion)
                    );
                } else {
                    println!("\nDid not understand region");
                }
                continue;
            } // end ps command

            if cmd[0] == "aj" {
                let act_num = cmd[1].parse().unwrap_or_else(|_err| num_actions);
                if act_num >= num_actions {
                    println!("\nInvalid action number");
                    continue;
                }

                if let Ok(aregion) = dm1.region_from_string(&cmd[2]) {
                    let stas = dm1.actions[act_num].squares.stas_adj_reg(&aregion);
                    println!("Squares adj to {} are {}", &aregion, &stas);
                } else {
                    println!("\nDid not understand region");
                }
                continue;
            } // end aj command

            if cmd[0] == "g1" {
                let act_num = cmd[1].parse().unwrap_or_else(|_err| num_actions);
                if act_num >= num_actions {
                    println!("\nInvalid action number");
                    continue;
                }

                if let Ok(aregion) = dm1.region_from_string(&cmd[2]) {
                    let sta_1s = dm1.actions[act_num]
                        .squares
                        .states_in_1_region(&dm1.actions[act_num].groups.regions());

                    println!(
                        "Squares in one region, in {} are {}",
                        &aregion,
                        aregion.states_in(&sta_1s)
                    );
                } else {
                    println!("\nDid not understand region");
                }
                continue;
            } // end g1 command
            println!("\nDid not understand command: {}", guess);
            continue;
        } // end 3-word commands

        // Handle four-word commands
        if cmd.len() == 4 {
            // Take Sample (ts) with <action num> <initial-state> <result-state>, don't update current state
            if cmd[0] == "ss" {
                let act_num = cmd[1].parse().unwrap_or_else(|_err| num_actions);
                if act_num >= num_actions {
                    println!("\nInvalid action number");
                    continue;
                }

                let i_state_rslt = dm1.state_from_string(&cmd[2]);
                match i_state_rslt {
                    Ok(i_state) => {
                        let r_state_rslt = dm1.state_from_string(&cmd[3]);
                        match r_state_rslt {
                            Ok(r_state) => {
                                println!(
                                    "Act {} take sample {} -> {}",
                                    act_num, &i_state, &r_state
                                );

                                dm1.take_action_arbitrary(act_num, &i_state, &r_state);
                                continue;
                            }
                            Err(error) => {
                                if error == 1 {
                                    println!("\nDid not understand state, should start with s");
                                } else {
                                    println!("\nDid not understand state, invalid character");
                                }
                            }
                        } // end match r_state_rslt
                    }
                    Err(error) => {
                        if error == 1 {
                            println!("\nDid not understand state, should start with s");
                        } else {
                            println!("\nDid not understand state, invalid character");
                        }
                    }
                } // end match i_state_rslt
                continue;
            } // end ss command

            println!("\nDid not understand command: {}", guess);
            continue;
        } // end 4-word commands
        println!("\nDid not understand command: {}", guess);
    } // end loop
} // end main

fn usage() {
    println!("\nCommands:");
    println!("    h | help                 - This list.\n");

    println!(
        "    Press Enter (no command) - Check for any Action needs, satisfy one need if possible.\n"
    );
    println!("    aj <act num> <region>    - For an Action, print squares adjacent to a region.\n");
    println!("    cs <state>               - Arbitrary change state.\n");
    println!(
        "    g1 <act num>             - For an Action, print squares that are only that region."
    );
    println!(
        "    g1 <act num> <region>    - For an Action and region, print squares that are only in one region.\n"
    );

    println!("    gn                       - Get needs\n");
    println!("    pa                       - Print all actions.");
    println!("    pa <act num>             - Print an action.\n");

    println!("\n    ps <act num>             - For an Action, print all Squares.");
    println!("    ps <act num> <region>    - For an Action, print Squares in a region.\n");

    println!("    ss <act num>             - Action to sample the current state.");
    println!("    ss <act num> <state>     - Action to sample state.");
    println!("    ss <act num> <state> <result-state> - Action to take an arbitrary sample.\n");
    println!("    tests                    - run tests and experimental code in tests.rs");

    println!("    to <region>              - Change the current state to within a region, by calculating and executing a plan.");

    println!("\n    An action number is an integer, zero or greater, where such an action exists.");
    println!("    A state starts with an 's' character, followed by zero, or more, zero and one characters.");
    println!("    A region starts with an 'r' character, followed by zero, or more, zero, one, X or x characters.");
    println!("\n    A region, or state, may contain separators of '_', '-', ',', '.' and/or '/',");
    println!("    leading consecutive zeros may be omitted ('r' is the same as 'r0', 's' is the same as 's0').");

    println!("\n    q | exit | quit          - Quit program.");
}

// Check need list to see if a need can be satisfied.
//
// Scan needs to see what need can be satisfied by the current state, sort by priority, limit to three.
// In the beginning, this avoids numerous dead-ends in trying to find a plan.
//
// Else, scan needs to see what need can be satisfied by a plan, sort by priority, limit to three.
//
// Return the new state, or None, in Option.
fn satisfy_need(dmx: &mut SomeDomain, nds: &NeedStore) -> bool {
    // Store tuples of NeedStore-index and plan, for needs that can be acheived
    // by matching the current state (empty plan) or by having a plan calculated.

    // A vector of vectors, for needs to be processed in order of priority,
    // lowest number first/highest.
    let mut pri_vec = Vec::<Vec<usize>>::with_capacity(8);

    // Scan needs to assign priority

    // For the AStateMakeGroup need, get the max num_x realized for a need that can be planned
    let mut a_state_make_group_max_x = 0;

    let mut inx = 0;
    for ndx in nds.iter() {
        if let Some(pri) = ndx.priority() {
            while pri_vec.len() <= pri {
                pri_vec.push(Vec::<usize>::new());
            }

            // Get max num_x for AStateMakeGroup needs
            match ndx {
                SomeNeed::AStateMakeGroup {
                    act_num: _,
                    targ_state: _,
                    for_reg: _,
                    far: _,
                    num_x: nx,
                } => {
                    if let Some(_) = dmx.make_plan(&ndx.target()) {
                        if *nx > a_state_make_group_max_x {
                            a_state_make_group_max_x = *nx;
                        }
                    }
                }
                _ => {}
            }

            vec_rand_push(&mut pri_vec[pri], inx);
        } // else the need is a adinistrative need that has already been delt with, so skip it.
        inx += 1;
    } // end scan of needs to assign priority

    // A vector of need-index and plan, for needs that can be met.
    let mut inx_plan = Vec::<(usize, SomePlan)>::new();

    // Scan needs to see what can be achieved with a plan
    for avec in pri_vec.iter() {
        // needs in sequence from zero on up.
        for nd_inx in avec.iter() {
            // for each need at the current priority

            let ndx = &nds[*nd_inx];

            if inx_plan.len() > 2 {
                break;
            }

            //println!("\nCheck a need {}", ndx);

            // Use only max num_x for AStateMakeGroup needs
            match ndx {
                SomeNeed::AStateMakeGroup {
                    act_num: _,
                    targ_state: _,
                    for_reg: _,
                    far: _,
                    num_x: nx,
                } => {
                    if *nx < a_state_make_group_max_x {
                        continue;
                    }
                }
                _ => {}
            }

            if let Some(plx) = dmx.make_plan(&ndx.target()) {
                inx_plan.push((*nd_inx, plx));
            }
        } // next ndx in avec

        // If at least one need of the current priority has been
        // found to be doable, do not check later priority needs
        if inx_plan.len() > 0 {
            break;
        }
    } // next avec in pri_vec

    // Print needs that can be achieved.
    println!(
        "{}",
        &String::from("\nSelected Action needs that can be done: ")
    );

    for itmx in &inx_plan {
        let pln = &itmx.1;
        println!("    {} {}", nds[itmx.0], pln);
    }
    println!("-----");

    // Return if no plans.  Includes plans of zero length for the current state.
    if inx_plan.len() == 0 {
        return false;
    }

    // Take a random choice
    let itmx = &inx_plan[rand::thread_rng().gen_range(0, inx_plan.len())];
    //    pause_for_enter("6");

    let ndx = &nds[itmx.0]; // get need using tuple index

    let pln = &itmx.1;

    println!("Need chosen: {} {}\n", ndx, &pln);

    if pln.len() > 0 {
        dmx.run_plan(&pln);
    }

    if ndx.satisfied_by(&dmx.cur_state) {
        dmx.take_action_need(&ndx);
        return true;
    }

    false
} // end satisfy_need

// Pause for input of 'c'. i.e stop consecutive Enter
pub fn pause_for_input(loc: &str) {
    loop {
        println!("{} Press c to continue: ", loc);
        io::stdout().flush().unwrap();
        let mut guess = String::new();
        io::stdin()
            .read_line(&mut guess)
            .expect("Falied to read line");

        for word in guess.split_whitespace() {
            if word == "c" {
                return;
            }
        } // end for
    } // end loop
}

pub fn pause_for_enter(loc: &str) {
    println!("{} Press Enter to continue: ", loc);
    io::stdout().flush().unwrap();
    let mut guess = String::new();
    io::stdin()
        .read_line(&mut guess)
        .expect("Falied to read line");
}
