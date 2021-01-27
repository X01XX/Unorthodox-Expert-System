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
use crate::region::SomeRegion;
mod change;
mod regionstore;
mod resultstore;
mod rule;
mod rulestore;
mod square;
mod squarestore;
mod state;
use crate::state::SomeState;
mod statestore;
use need::SomeNeed;
mod combinable;
mod domain;
mod needstore;
use crate::needstore::NeedStore;
mod plan;
mod pn;
mod step;
mod stepstore;
use domain::SomeDomain;
mod actions;
mod domainstore;
mod tests;
use domainstore::DomainStore;

use std::io;
use std::io::{Read, Write};
use std::process;
extern crate rand;
use rand::Rng;
use std::fs::File;
use std::path::Path;

fn init() -> DomainStore {
    // Start a DomainStore
    let mut dmxs = DomainStore::new();

    // Initialize a domain, with number of integers, initial state, optimal region.

    // Generate a random staring state
    let inx = rand::thread_rng().gen_range(0, 2_u8.pow(6));
    let inx_str = &format!("s{:b}", inx);

    dmxs.add_domain(1, inx_str, "r101X");
    dmxs.add_action(6);
    dmxs.add_action(0);
    dmxs.add_action(0);
    dmxs.add_action(0);
    dmxs.add_action(0);
    dmxs.add_action(0);
    dmxs.add_action(0);

    // Generate a random staring state
    let inx = rand::thread_rng().gen_range(0, 2_u8.pow(4));
    let inx_str = &format!("s{:b}000000", inx);

    dmxs.add_domain(2, inx_str, "r10_1X00_0000");
    dmxs.add_action(0);
    dmxs.add_action(0);
    dmxs.add_action(0);
    dmxs.add_action(0);

    dmxs
}

fn main() {
    // Start a DomainStore, add a Domain

    let mut dmxs = init();

    usage();

    let mut dom_num = 0;
    let mut run = 0;

    loop {
        dmxs.step += 1;

        // Get the needs of all Domains / Actions
        let nds = dmxs.get_needs();

        //let mut dmx = &mut dmxs[dom_num];

        print!("\nDom: {}", dom_num,);

        //        print!(
        //            " Predictable changes: {}",
        //            &dmxs[dom_num].x_mask,
        //        );

        println!("\nActs: {}\n\nAction needs:", &dmxs[dom_num].actions);

        if nds.len() > 0 {
            let mut inx = 0;
            for ndx in nds.iter() {
                println!("{:02} {}", inx, ndx);
                inx += 1;
            }
        //println!("\nAction needs: {}", nds);
        } else {
            println!("\nAction needs: None");
        }

        let mut in_opt = "Not in";

        if dmxs[dom_num]
            .optimal
            .is_superset_of_state(&dmxs[dom_num].cur_state)
        {
            in_opt = "in";
        }

        println!(
            "\nStep: {} Dom: {} Current State: {}  Max Region: {}  {} Optimal Region: {}",
            &dmxs.step,
            dom_num,
            &dmxs[dom_num].cur_state,
            &dmxs[dom_num].max_region,
            &in_opt,
            &dmxs[dom_num].optimal
        );

        if nds.len() == 0 {
            if run > 0 {
                run -= 1;
                if run > 0 {
                    dmxs = init();
                }
            }
        }

        let mut cmd = Vec::<String>::with_capacity(10);

        if run > 0 {
        } else {
            print!("\nPress Enter to continue: ");
            io::stdout().flush().unwrap();

            let mut guess = String::new();
            io::stdin()
                .read_line(&mut guess)
                .expect("Failed to read line");

            //println!("The command is: {} len {} char1: {:?}", guess, guess.len(), guess.chars());

            for word in guess.split_whitespace() {
                //println!("word: {} is {}", word_count, word);
                cmd.push(word.to_ascii_lowercase());
            }
        }

        // Default command, just press Enter
        if cmd.len() == 0 {
            // Process needs
            if nds.len() > 0 {
                //println!("\nAction needs: {}", nds);

                if let Some((inx, pln)) = dmxs.choose_need(&nds) {
                    let ndx = &nds[inx];
                    dom_num = ndx.dom_num();

                    println!("need {}, plan {}", &ndx, &pln);

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
                dmxs[dom_num].check_async();
                // If no needs, change the state to an optimal state if needed
                if dmxs[dom_num]
                    .optimal
                    .is_superset_of_state(&dmxs[dom_num].cur_state)
                {
                } else {
                    let optimal = dmxs[dom_num].optimal.clone();
                    if dmxs[dom_num].to_region(&optimal) {
                        println!("Change to region succeeded");
                    } else {
                        println!("Change to region failed");
                    }
                }
            }
            continue;
        } else {
            if cmd.len() == 1 {
                if cmd[0] == "run" {
                    run = 1;
                    dmxs = init();
                    continue;
                }
            } else if cmd.len() == 2 {
                if cmd[0] == "run" {
                    let r_num = cmd[1].parse().unwrap_or_else(|err| {
                        println!("Invalid Run Number: {}", err);
                        0
                    });
                    run = r_num;
                } else if cmd[0] == "cd" {
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
                } else if cmd[0] == "dn" {
                    let n_num = cmd[1].parse().unwrap_or_else(|err| {
                        println!("Invalid Need Number: {}", err);
                        999
                    });
                    if n_num == 999 {
                        continue;
                    }
                    if n_num >= nds.len() {
                        println!("Invalid Need Number: {}", cmd[1]);
                        continue;
                    }

                    let ndx = &nds[n_num];
                    let mut nds2 = NeedStore::new();
                    nds2.push(ndx.clone());

                    if let Some((inx, pln)) = dmxs.choose_need(&nds2) {
                        let ndx = &nds2[inx];
                        dom_num = ndx.dom_num();

                        println!("need {}, plan {}", &ndx, &pln);

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
                    } else {
                        println!("Path to satisfy the need was not found");
                    }
                    continue;
                } else if cmd[0] == "ld" {
                    match load_data(&cmd[1]) {
                        Err(why) => {
                            println!("couldn't read {}: {}", &cmd[1], why);
                        }
                        Ok(new_dmxs) => {
                            print!("Data loaded");
                            dmxs = new_dmxs;
                            dmxs.step -= 1;
                        }
                    } // end match load_data

                    continue;
                } else if cmd[0] == "sd" {
                    match store_data(&dmxs, &cmd[1]) {
                        Err(why) => {
                            println!("couldn't write {}: {}", &cmd[1], why);
                        }
                        Ok(_) => {
                            print!("Data written");
                        }
                    }

                    continue;
                } // end command sd
            }
            do_command(&mut dmxs[dom_num], &cmd);
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
            let region_r = SomeRegion::from_string(dm1.num_ints, &cmd[1]);
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
            let state_r = SomeState::from_string(dm1.num_ints, &cmd[1]);
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
            let region_r = SomeRegion::from_string(dm1.num_ints, &cmd[1]);
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
                        if dm1.to_region(&goal_region) {
                            println!("Change to region succeeded");
                            return true;
                        } else {
                            println!("Change to region failed");
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

            let ndx = &dm1.actions[act_num].seek_edge_needs1();

            if ndx.len() > 0 {
                println!("Seek Edge Needs are {}", &ndx);
                return true;
            }

            let ndx = &dm1.actions[act_num].seek_edge_needs2(&dm1.cur_state);

            println!("Seek Edge Needs are {}", &ndx);
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
            let state_r = SomeState::from_string(dm1.num_ints, &cmd[2]);
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
            if let Ok(aregion) = SomeRegion::from_string(dm1.num_ints, &cmd[2]) {
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
            if let Ok(aregion) = SomeRegion::from_string(dm1.num_ints, &cmd[2]) {
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
            if let Ok(aregion) = SomeRegion::from_string(dm1.num_ints, &cmd[2]) {
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
            let i_state_rslt = SomeState::from_string(dm1.num_ints, &cmd[2]);
            match i_state_rslt {
                Ok(i_state) => {
                    let r_state_rslt = SomeState::from_string(dm1.num_ints, &cmd[3]);
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
    println!("    dn <need number>        - Run a particular need from the need list");
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

    println!("\n    ld <path>                - Load data from a file");
    println!("\n    sd <path>                - Store data to a file\n");

    println!("    run                      - Run until no needs left.");
    println!("    run <number times>       - Run number of times. To elicit panics\n");

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

fn load_data(path_str: &String) -> Result<DomainStore, String> {
    let path = Path::new(path_str);
    let display = path.display();

    // Open a file, returns `io::Result<File>`
    match File::open(&path) {
        Err(why) => {
            return Err(format!("couldn't read {}: {}", display, why));
        }
        Ok(mut afile) => {
            let mut serialized = String::new();
            match afile.read_to_string(&mut serialized) {
                Err(why) => {
                    return Err(format!("couldn't read {}: {}", display, why));
                }
                Ok(_) => {
                    let deserialized_r = serde_yaml::from_str(&serialized);
                    match deserialized_r {
                        Err(why) => {
                            return Err(format!("couldn't deserialize {}: {}", display, why));
                        }
                        Ok(new_dmxs) => {
                            return Ok(new_dmxs);
                        }
                    } // end match deserialized_r
                }
            }
        }
    } // end match open file
}

fn store_data(dmxs: &DomainStore, path_str: &String) -> Result<(), String> {
    let serialized_r = serde_yaml::to_string(&dmxs);
    match serialized_r {
        Ok(serialized) => {
            let path = Path::new(&path_str);
            let display = path.display();

            // Open a file in write-only mode, returns `io::Result<File>`
            match File::create(&path) {
                Err(why) => Err(format!("couldn't create {}: {}", display, why)),
                Ok(mut file) => match file.write_all(serialized.as_bytes()) {
                    Err(why) => Err(format!("couldn't write to {}: {}", display, why)),
                    Ok(_) => Ok(()),
                },
            }
        }
        Err(error) => Err(format!("{}", error)),
    } // end match serialized_r
} // end store_data
