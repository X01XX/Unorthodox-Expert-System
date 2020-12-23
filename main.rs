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
use crate::region::region_from_string;
mod regionstore;
mod resultstore;
mod rule;
//use crate::rule::SomeRule;
mod rulestore;
mod square;
mod squarestore;
mod state;
use crate::state::state_from_string;
mod statestore;
use need::SomeNeed;
mod needstore;
//use crate::needstore::NeedStore;
mod plan;
mod step;
mod stepstore;
//use crate::stepstore::StepStore;
//use plan::SomePlan;
mod combinable;
mod domain;
mod pn;
use domain::SomeDomain;
mod actions;
mod domainstore;
mod tests;
use domainstore::DomainStore;

use crate::actions::{
    dom0_act0, dom0_act1, dom0_act2, dom0_act3, dom0_act4, dom0_act5, dom0_act6, dom1_act0,
    dom1_act1, dom1_act2, dom1_act3,
};

use std::io;
use std::io::Write;
use std::process; // bring flush() into scope
extern crate rand;
//use rand::Rng;

// Push an item into a random position in a vector
//fn vec_rand_push<T>(avec: &mut Vec<T>, num: T) {
//    if avec.len() > 1 {
//        let inx = rand::thread_rng().gen_range(0, avec.len() + 1); // last in range is exclusive, (0,1) will always return 0.
//                                                                   //println!("random inx {} len {}", inx, avec.len());
//        if inx == avec.len() {
//            avec.push(num);
//        } else {
//            avec.insert(inx, num);
//        }
//    } else {
//        avec.push(num);
//    }
//}

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
    // for tests.rs
    let mx = mask::mask_from_string(1, "m1");

    match mx {
        Ok(_) => {}
        Err(err) => {
            panic!("mask from string not OK {}", &err);
        }
    };

    // Start a DomainStore, add a Domain
    let mut dmxs = DomainStore::new();

    // Initialize a domain, with number of u8 integers, initial state, optimal region.
    // The number of u8 integers can be higher.

    let mut dm0 = SomeDomain::new(1, "s0001", "r101X");
    dm0.add_action(dom0_act0, 6);
    dm0.add_action(dom0_act1, 0);
    dm0.add_action(dom0_act2, 0);
    dm0.add_action(dom0_act3, 0);
    dm0.add_action(dom0_act4, 0);
    dm0.add_action(dom0_act5, 0);
    dm0.add_action(dom0_act6, 0);

    dmxs.push(dm0);

    let mut dm1 = SomeDomain::new(1, "s0001", "r101X");
    dm1.add_action(dom1_act0, 6);
    dm1.add_action(dom1_act1, 0);
    dm1.add_action(dom1_act2, 0);
    dm1.add_action(dom1_act3, 0);

    dmxs.push(dm1);

    usage();
    let mut step = 0;

    let mut dom_num = 0;

    loop {
        step += 1;

        // Get the needs of all Domains / Actions
        let nds = dmxs.get_needs();

        let mut dmx = &mut dmxs[dom_num];

        println!("\nDom: {} Acts: {}", dom_num, dmx.actions);

        if nds.len() > 0 {
            println!("\nAction needs: {}", nds);
        } else {
            println!("\nAction needs: None");
        }

        let mut in_opt = "Not in";

        if dmx.optimal.is_superset_of_state(&dmx.cur_state) {
            in_opt = "in";
        }

        println!(
            "\nStep: {} Dom: {} Current State: {}  Max Region: {}  {} Optimal Region: {}",
            &step, dom_num, &dmx.cur_state, &dmx.max_region, &in_opt, &dmx.optimal
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

                // TODO select highest priority needs that can be done
                if let Some(ndspln) = dmxs.choose_need(&nds) {
                    let pln = &ndspln.1;
                    let ndx = &nds[ndspln.0];
                    dom_num = ndx.dom_num();

                    println!("need {}, plan {}", &ndx, &pln);

                    //println!("need {}, plan {}", &nds[ndspln.0], ndspln.1);

                    if pln.len() > 0 {
                        //println!("doing dmx.run_plan");
                        dmxs.run_plan(dom_num, &pln);
                    } else {
                        //println!("NOT doing dmx.run_plan");
                    }

                    if ndx.satisfied_by(&dmxs.cur_state(dom_num)) {
                        // println!("doing dmx.take_action_need");
                        dmxs.take_action_need(dom_num, &ndx);
                    } else {
                        // println!("NOT doing dmx.take_action_need");
                    }
                }
            } else {
                // If no needs, change the state to an optimal state if needed
                if dmx.optimal.is_superset_of_state(&dmx.cur_state) {
                } else {
                    if let Some(pln) = dmx.make_plan(&dmx.optimal) {
                        println!("Changing state to optimal, Plan is {}", pln);

                        // Do the plan
                        dmx.run_plan(&pln);
                    } else {
                        println!("No plan found to change to the optimal state");
                    }
                }
            }
            continue;
        } else {
            if cmd.len() == 2 {
                if cmd[0] == "cd" {
                    let d_num = cmd[1].parse().unwrap_or_else(|err| {
                        println!("Invalid Domain Number: {}", err);
                        999
                    });
                    if d_num == 999 {
                        continue;
                    }
                    if d_num >= dmxs.num_domains() {
                        println!("\nInvalid Domain number");
                    } else {
                        dom_num = d_num;
                    }
                    continue;
                }
            }
            do_command(&mut dmx, &cmd);
        }
    } // end loop
} // end main

// Do a command
// if done, return true
// else print an error message and return false
fn do_command(dm1: &mut SomeDomain, cmd: &Vec<String>) -> bool {
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
        if cmd[0] == "co" {
            let region_r = region_from_string(dm1.num_ints, &cmd[1]);
            match region_r {
                Ok(goal_region) => {
                    println!(
                        "Change Optimal regionfrom {} to {}",
                        dm1.optimal, goal_region
                    );
                    dm1.optimal = goal_region;
                }
                Err(error) => {
                    println!("\nDid not understand region, {}", error);
                    return false;
                }
            } // end match region_r
        } //end command co

        // Arbitrary change state
        if cmd[0] == "cs" {
            let state_r = state_from_string(dm1.num_ints, &cmd[1]);
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
            let region_r = region_from_string(dm1.num_ints, &cmd[1]);
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
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            let ndx = &dm1.actions[act_num].in_between_needs(&dm1.cur_state);

            println!("InBetweenNeeds are {}", &ndx);
            return true;
        }

        if cmd[0] == "ss" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }

            println!("Act {} sample State {}", act_num, dm1.cur_state);
            dm1.take_action_need(&SomeNeed::StateNotInGroup {
                dom_num: dm1.num,
                act_num: act_num,
                targ_state: dm1.cur_state.clone(),
            });
            return true;
        }

        if cmd[0] == "pa" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            println!("{}", &dm1.actions[act_num]);
            return true;
        }

        if cmd[0] == "g1" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
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
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
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
        if cmd[0] == "ss" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            let state_r = state_from_string(dm1.num_ints, &cmd[2]);
            match state_r {
                Ok(a_state) => {
                    println!("Act {} sample State {}", act_num, a_state);
                    dm1.cur_state = a_state.clone();
                    dm1.take_action_need(&SomeNeed::StateNotInGroup {
                        dom_num: dm1.num,
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
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            if let Ok(aregion) = region_from_string(dm1.num_ints, &cmd[2]) {
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
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            if let Ok(aregion) = region_from_string(dm1.num_ints, &cmd[2]) {
                let stas = dm1.actions[act_num].squares.stas_adj_reg(&aregion);
                println!("Squares adj to {} are {}", &aregion, &stas);
                return true;
            }
            println!("\nDid not understand region");
            return false;
        }

        if cmd[0] == "g1" {
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            if let Ok(aregion) = region_from_string(dm1.num_ints, &cmd[2]) {
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
            let act_num = cmd[1].parse().unwrap_or_else(|err| {
                println!("Problem parsing Action number: {}", err);
                999
            });
            if act_num == 999 {
                return false;
            }
            if act_num >= dm1.num_actions() {
                println!("\nInvalid action number");
                return false;
            }
            let i_state_rslt = state_from_string(dm1.num_ints, &cmd[2]);
            match i_state_rslt {
                Ok(i_state) => {
                    let r_state_rslt = state_from_string(dm1.num_ints, &cmd[3]);
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
    println!(
        "    cd <dom num>             - Change the displayed Domain to the given Domain number.\n"
    );
    println!("    co <region>             - Change the optimal region to the given region.\n");
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

    println!(
        "\n    An action number is an integer, zero or greater, where such an action exists.\n"
    );
    println!("    A state starts with an 's' character, followed by zero, or more, zero and one characters.");
    println!("    A region starts with an 'r' character, followed by zero, or more, zero, one, X or x characters.");
    println!("\n    A region, or state, may contain the separator '_', which will be ignored.\n");
    println!("    leading consecutive zeros may be omitted ('r' is the same as 'r0', 's' is the same as 's0').");

    println!("\n    q | exit | quit          - Quit program.");
}

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
