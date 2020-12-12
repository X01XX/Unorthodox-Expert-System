// Unorthodox Expert System

#![allow(
// dead_code,
//  unused_variables,
//  unused_macros,
//  unused_imports,
//  unused_assignments,
//  unused_mut
)]

mod action;
mod actionstore;
mod bits;
mod bitsstore;
mod group;
mod groupstore;
mod mask;
mod maskstore;
mod need;
mod region;
mod regionstore;
mod resultstore;
mod rule;
mod rulestore;
mod square;
mod squarestore;
mod state;
//use crate::state::SomeState;
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

use crate::actions::{action0, action1, action2, action3, action4, action5, action6};

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
    // dmx.add_action(action0, 0);
    // dmx.add_action(action0, 2);
    dmx.add_action(action0, 6);

    dmx.add_action(action1, 0);
    dmx.add_action(action2, 0);
    dmx.add_action(action3, 0);
    dmx.add_action(action4, 0);
    dmx.add_action(action5, 0);
    dmx.add_action(action6, 0);
    dmx
}
//use std::thread;
//use std::sync::mpsc::{self, Receiver, Sender};
//use std::sync::{Arc, Mutex};
//use crate::action::SomeAction;

//use std::rc::Rc;

//fn test1(num: usize, cur_state: &SomeState) -> usize{
//	println!("a num: {} cur {}", num, cur_state);
//	num * 3
//}

fn main() {
    let mut dm1 = init_domain(1, "s0001"); // init state to 1 u8 integer of bits, may be higher
                                           //let num_actions = dm1.num_actions();

    //pause_for_enter("");

    usage();
    let mut step = 0;

    loop {
        step += 1;

        // **** new code

        // Get needs, using a thread for each action
        let nds = dm1.get_needs();
        //println!("parallel needs: {}", &nds);


        // **** end new code

        println!("\nActs: {}", dm1.actions);
        //let nds: NeedStore = dm1.get_needs();

        if nds.len() > 0 {
            println!("\nAction needs: {}", nds);
        } else {
            println!("\nAction needs: None");
        }

        println!(
            "\nStep: {} Current State: {}  Max Region: {}",
            &step, &dm1.cur_state, &dm1.max_region
        );

        print!("\nPress Enter to continue: ");
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
            // Process needs
            if nds.len() > 0 {
                //println!("\nAction needs: {}", nds);

                //let curst = dm1.cur_state.clone();
                if satisfy_need(&mut dm1, &nds) {
                } else {
                    println!("no need satisfied");
                }
            }
            continue;
        } else {
            do_command(&mut dm1, &guess);
        }
    }
} // end main

// Do a command
// if done, return true
// else print an error message and return false
fn do_command(dm1: &mut SomeDomain, guess: &String) -> bool {
    let mut cmd = Vec::<String>::with_capacity(10);

    for word in guess.split_whitespace() {
        //println!("word:is {}", word);
        cmd.push(word.to_ascii_lowercase());
    }

    // Handle one-word commands
    if cmd.len() == 1 {
        // Quit with q , exit, quit
        if cmd[0] == "q" || cmd[0] == "exit" || cmd[0] == "quit" {
            println!("Done");
            process::exit(0);
        }

        if cmd[0] == "h" || cmd[0] == "help" {
            usage();
            return true;
        }

        if cmd[0] == "pa" {
            println!("\nActs: {}", dm1.actions);
            return true;
        }

        if cmd[0] == "gn" {
            println!("\nActs: {}", dm1.get_needs());
            return true;
        }
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
                    return true;
                }
                Err(error) => {
                    println!("\nDid not understand state, {}", error);
                    return false;
                }
            } // end match
        } // end command cs

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
                        return false;
                    } else {
                        if let Some(pln) = dm1.make_plan(&goal_region) {
                            println!("Plan is {}", pln);

                            // Do the plan
                            dm1.run_plan(&pln);
                            return true;
                        } else {
                            println!("No plan found");
                            return false;
                        }
                    }
                }
                Err(error) => {
                    println!("\nDid not understand region, {}", error);
                    return false;
                }
            } // end match region_r
        } //end command to

        if cmd[0] == "ibn" {
            let act_num = cmd[1].parse().unwrap_or_else(|_err| dm1.num_actions());
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            let ndx = &dm1.actions[act_num].in_between_needs(&dm1.cur_state);

            println!("InBetweenNeeds are {}", &ndx);
            return true;
        }

        if cmd[0] == "ss" {
            let act_num = cmd[1].parse().unwrap_or_else(|_err| dm1.num_actions());
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }

            println!("Act {} sample State {}", act_num, dm1.cur_state);
            dm1.take_action_need(&SomeNeed::StateNotInGroup {
                act_num: act_num,
                targ_state: dm1.cur_state.clone(),
            });
            return true;
        }

        if cmd[0] == "pa" {
            let act_num = cmd[1].parse().unwrap_or_else(|_err| dm1.num_actions());
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            println!("{}", &dm1.actions[act_num]);
            return true;
        }

        if cmd[0] == "g1" {
            let act_num = cmd[1].parse().unwrap_or_else(|_err| dm1.num_actions());
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }

            let sqrs = dm1.actions[act_num]
                .squares
                .states_in_1_region(&dm1.actions[act_num].groups.regions());

            println!("Act {} State in one group {}", act_num, &sqrs);
            return true;
        }

        if cmd[0] == "ps" {
            let act_num = cmd[1].parse().unwrap_or_else(|_err| dm1.num_actions());
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            println!(
                "Squares of Action {} are:\n{}\n",
                &act_num, &dm1.actions[act_num].squares
            );
            return true;
        }
    } // end two-word commands

    // Handle three-word commands
    if cmd.len() == 3 {
        // Satisfy an InBetween need
        //        if cmd[0] == "ibn" {
        //            let act_num = cmd[1].parse().unwrap_or_else(|_err| dm1.num_actions());
        //            if act_num >= dm1.num_actions() {
        //                println!("\nInvalid action number");
        //                return false;
        //            }
        //            let state_r = dm1.state_from_string(&cmd[2]);
        //            match state_r {
        //                Ok(a_state) => {
        //                    println!("Act {} sample State {}", act_num, a_state);
        //                    dm1.cur_state = a_state.clone();
        //
        //                    let ndxs = &dm1.actions[act_num].in_between_needs(&dm1.cur_state);
        //
        //                    println!("InBetweenNeeds are {}", &ndxs);
        //
        //                    for ndx in ndxs.iter() {
        //                        if ndx.target() == SomeRegion::new(&a_state, &a_state) {
        //                            // run the sample and need
        //                            dm1.take_action_need(&ndx);
        //                            break;
        //                        }
        //                    }
        //                    return true;
        //                }
        //                Err(error) => {
        //                    if error == 1 {
        //                        println!("\nDid not understand state, should start with s");
        //                    } else {
        //                        println!("\nDid not understand state, invalid character");
        //                    }
        //                    return false;
        //                }
        //            } // end match
        //        }

        if cmd[0] == "ss" {
            let act_num = cmd[1].parse().unwrap_or_else(|_err| dm1.num_actions());
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
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
                    return true;
                }
                Err(error) => {
                    println!("\nDid not understand state, {}", error);
                    return false;
                }
            } // end match state_r
        }

        if cmd[0] == "ps" {
            let act_num = cmd[1].parse().unwrap_or_else(|_err| dm1.num_actions());
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            if let Ok(aregion) = dm1.region_from_string(&cmd[2]) {
                let mut psstr = String::from(format!(
                    "Squares of Action {} in region {} are:\n",
                    &act_num, &aregion
                ));
                let stas = dm1.actions[act_num].squares.stas_in_reg(&aregion);

                let mut flg = 0;

                for stax in stas.iter() {
                    if flg == 1 {
                        psstr.push_str(",\n");
                    }

                    if let Some(sqrx) = dm1.actions[act_num].squares.find(&stax) {
                        psstr.push_str(&format!("{}", sqrx));
                    } else {
                        println!("Square {} not found??", &stax);
                    }

                    flg = 1;
                }

                println!("{}", &psstr);
                return true;
            }
            println!("\nDid not understand region");
            return false;
        }

        if cmd[0] == "aj" {
            let act_num = cmd[1].parse().unwrap_or_else(|_err| dm1.num_actions());
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            if let Ok(aregion) = dm1.region_from_string(&cmd[2]) {
                let stas = dm1.actions[act_num].squares.stas_adj_reg(&aregion);
                println!("Squares adj to {} are {}", &aregion, &stas);
                return true;
            }
            println!("\nDid not understand region");
            return false;
        }

        if cmd[0] == "g1" {
            let act_num = cmd[1].parse().unwrap_or_else(|_err| dm1.num_actions());
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
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
                return true;
            }
            println!("\nDid not understand region");
            return false;
        }
    } // end 3-word commands

    // Handle four-word commands
    if cmd.len() == 4 {
        // Take Sample (ts) with <action num> <initial-state> <result-state>, don't update current state
        if cmd[0] == "ss" {
            let act_num = cmd[1].parse().unwrap_or_else(|_err| dm1.num_actions());
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            let i_state_rslt = dm1.state_from_string(&cmd[2]);
            match i_state_rslt {
                Ok(i_state) => {
                    let r_state_rslt = dm1.state_from_string(&cmd[3]);
                    match r_state_rslt {
                        Ok(r_state) => {
                            println!("Act {} take sample {} -> {}", act_num, &i_state, &r_state);

                            dm1.take_action_arbitrary(act_num, &i_state, &r_state);
                            return true;
                        }
                        Err(error) => {
                            println!("\nDid not understand state, {}", error);
                            return false;
                        }
                    } // end match r_state_rslt
                }
                Err(error) => {
                    println!("\nDid not understand state, {}", error);
                    return false;
                }
            } // end match i_state_rslt
        }
    } // end 4-word commands

    println!("\nDid not understand command: {:?}", cmd);
    false
} // end do_command

fn usage() {
    println!("\nCommands:");
    println!("    h | help                 - This list.\n");

    println!(
        "    Press Enter (no command) - Check for any Action needs, satisfy one need if possible.\n"
    );
    println!("    aj <act num> <region>    - For an Action, print squares adjacent to a region.\n");
    println!("    cs <state>               - Arbitrary change state.\n");
    println!(
        "    g1 <act num>             - For an Action, print squares that are only in one region."
    );
    println!(
        "    g1 <act num> <region>    - For an Action and region, print squares that are only in that region.\n"
    );

    println!("    gn                       - Get needs\n");
    println!("    pa                       - Print all actions.");
    println!("    pa <act num>             - Print an action.\n");

    println!("\n    ps <act num>             - For an Action, print all Squares.");
    println!("    ps <act num> <region>    - For an Action, print Squares in a region.\n");

    println!("    ss <act num>             - Action to sample the current state.");
    println!("    ss <act num> <state>     - Action to sample state.");
    println!("    ss <act num> <state> <result-state> - Action to take an arbitrary sample.\n");

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
