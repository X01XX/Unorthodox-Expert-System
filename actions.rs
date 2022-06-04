//! Actions to be taken, creating samples for the UES.
//!
//! These actions are for testing and learning, they should eventually be replaced
//! by actions that make changes outside the program.

use crate::change::SomeChange;
use crate::mask::SomeMask;
use crate::state::SomeState;
use rand::Rng;

// The number of bits is set by the line of code that creates
// the first current state, with 1, or more, integers.

/// Take an action given the domain number, action number, current_state and last change mask (if any).
pub fn take_action(
    dom_num: usize,
    act_num: usize,
    cur_state: &SomeState,
    cmask: Option<&SomeMask>,
) -> SomeState {
    if dom_num == 0 {
        if act_num == 0 {
            return dom0_act0(cur_state, cmask);
        } else if act_num == 1 {
            return dom0_act1(cur_state, cmask);
        } else if act_num == 2 {
            return dom0_act2(cur_state, cmask);
        } else if act_num == 3 {
            return dom0_act3(cur_state, cmask);
        } else if act_num == 4 {
            return dom0_act4(cur_state, cmask);
        } else if act_num == 5 {
            return dom0_act5(cur_state, cmask);
        } else if act_num == 6 {
            return dom0_act6(cur_state, cmask);
        } else if act_num == 7 {
            return dom0_act7(cur_state, cmask);
        } else if act_num == 8 {
            return dom0_act8(cur_state, cmask);
        } else {
            panic!("Dom 0, Uknown Action number {}", act_num);
        }
    } else if dom_num == 1 {
        if act_num == 0 {
            return dom1_act0(cur_state, cmask);
        } else if act_num == 1 {
            return dom1_act1(cur_state, cmask);
        } else if act_num == 2 {
            return dom1_act2(cur_state, cmask);
        } else if act_num == 3 {
            return dom1_act3(cur_state, cmask);
        } else if act_num == 4 {
            return dom1_act4(cur_state, cmask);
        } else {
            panic!("Dom 1, Uknown Action number {}", act_num);
        }
    } else {
        panic!("Unknown Domain number {}", dom_num);
    }
}

/// Domain 0, act 0, actions, given the current state.
/// The SomeMask argument is a kludge to support multiple result actions.
pub fn dom0_act0(cur: &SomeState, cmask: Option<&SomeMask>) -> SomeState {
    if cur.is_bit_set(3) && cur.is_bit_set(1) == false     // ...1X0X
        || cur.is_bit_set(3) == false && cur.is_bit_set(1) // ...0X1X
        || cur.is_bit_set(2) && cur.is_bit_set(0)
    {
        // ....X1X1

        let new_state = cur.toggle_bits(vec![0]);
        println!(
            "\nDom 0 Act 0 {} -> {} change: {}",
            cur,
            new_state,
            SomeChange::new_from_to(&cur, &new_state)
        );
        return new_state;
    }

    if cur.is_bit_set(1) {
        // ...101x, 1x10, given that the above is not true, alternate between two changes.
        let mut sample_num = rand::thread_rng().gen_range(1..3);
        if let Some(amsk) = cmask {
            if amsk.is_bit_set(1) {
                sample_num = 1;
            } else if amsk.is_bit_set(2) {
                sample_num = 2;
            }
        }

        if sample_num == 2 {
            let new_state = cur.toggle_bits(vec![1]);
            println!(
                "\nDom 0 Act 0 {} -> {} change: {}",
                cur,
                new_state,
                SomeChange::new_from_to(&cur, &new_state)
            );
            return new_state;
        } else if sample_num == 1 {
            let new_state = cur.toggle_bits(vec![2]);
            println!(
                "\nDom 0 Act 0 {} -> {} change: {}",
                cur,
                new_state,
                SomeChange::new_from_to(&cur, &new_state)
            );
            return new_state;
        } else {
            panic!("1/2 change failed!");
        }
    } else {
        // ...000x, 0x00, given that the above is not true, alternate between 3 changes.
        let mut sample_num = rand::thread_rng().gen_range(1..4);
        if let Some(amsk) = cmask {
            if amsk.is_bit_set(1) {
                sample_num = 1;
            } else if amsk.is_bit_set(2) {
                sample_num = 2;
            } else if amsk.is_bit_set(3) {
                sample_num = 3;
            }
        }

        if sample_num == 3 {
            let new_state = cur.toggle_bits(vec![1]);
            println!(
                "\nDom 0 Act 0 {} -> {} change: {}",
                cur,
                new_state,
                SomeChange::new_from_to(&cur, &new_state)
            );
            return new_state;
        } else if sample_num == 1 {
            let new_state = cur.toggle_bits(vec![2]);
            println!(
                "\nDom 0 Act 0 {} -> {} change: {}",
                cur,
                new_state,
                SomeChange::new_from_to(&cur, &new_state)
            );
            return new_state;
        } else if sample_num == 2 {
            let new_state = cur.toggle_bits(vec![3]);
            println!(
                "\nDom 0 Act 0 {} -> {} change: {}",
                cur,
                new_state,
                SomeChange::new_from_to(&cur, &new_state)
            );
            return new_state;
        } else {
            panic!("1/2/3 change failed");
        }
    }
} // end action0

/// Domain 0, act 1, actions, given the current state.
/// Toggle bit 1. ....../Xx/.
pub fn dom0_act1(cur: &SomeState, _cmask: Option<&SomeMask>) -> SomeState {
    let new_state = cur.toggle_bits(vec![1]);
    println!(
        "\nDom 0 Act 1 {} -> {} change: {}",
        cur,
        new_state,
        SomeChange::new_from_to(&cur, &new_state)
    );
    return new_state;
}

/// Domain 0, act 2, actions, given the current state.
/// Toggle bit 2. ...../Xx/..
pub fn dom0_act2(cur: &SomeState, _cmask: Option<&SomeMask>) -> SomeState {
    let new_state = cur.toggle_bits(vec![2]);
    println!(
        "\nDom 0 Act 2 {} -> {} change: {}",
        cur,
        new_state,
        SomeChange::new_from_to(&cur, &new_state)
    );
    return new_state;
}

/// Domain 0, act 3, actions, given the current state.
/// Toggle bit 3. ..../Xx/...
pub fn dom0_act3(cur: &SomeState, _cmask: Option<&SomeMask>) -> SomeState {
    let new_state = cur.toggle_bits(vec![3]);
    println!(
        "\nDom 0 Act 3 {} -> {} change: {}",
        cur,
        new_state,
        SomeChange::new_from_to(&cur, &new_state)
    );
    return new_state;
}

/// Domain 0, act 4, actions, given the current state.
/// Toggle bit 4. .../Xx/...
pub fn dom0_act4(cur: &SomeState, _cmask: Option<&SomeMask>) -> SomeState {
    let new_state = cur.toggle_bits(vec![4]);
    println!(
        "\nDom 0 Act 4 {} -> {} change: {}",
        cur,
        new_state,
        SomeChange::new_from_to(&cur, &new_state)
    );
    return new_state;
}

/// Domain 0, act 5, actions, given the current state.
/// Toggle bit 5. ../Xx/.....
pub fn dom0_act5(cur: &SomeState, _cmask: Option<&SomeMask>) -> SomeState {
    let new_state = cur.toggle_bits(vec![5]);
    println!(
        "\nDom 0 Act 5 {} -> {} change: {}",
        cur,
        new_state,
        SomeChange::new_from_to(&cur, &new_state)
    );
    return new_state;
}

/// Domain 0, act 6, actions, given the current state.
/// Toggle bit 2,3 ..../Xx/Xx/..
pub fn dom0_act6(cur: &SomeState, _cmask: Option<&SomeMask>) -> SomeState {
    let new_state = cur.toggle_bits(vec![2, 3]);
    println!(
        "\nDom 0 Act 6 {} -> {} change: {}",
        cur,
        new_state,
        SomeChange::new_from_to(&cur, &new_state)
    );
    return new_state;
}

/// Domain 0, act 7, actions, given the current state.
/// Toggle bit 6 and 7, Xx/Xx/......
pub fn dom0_act7(cur: &SomeState, _cmask: Option<&SomeMask>) -> SomeState {
    let new_state = cur.toggle_bits(vec![7, 6]);
    println!(
        "\nDom 0 Act 7 {} -> {} change: {}",
        cur,
        new_state,
        SomeChange::new_from_to(&cur, &new_state)
    );
    return new_state;
}

/// Domain 0, act 8, actions, given the current state.
/// Toggle bit 6 ./Xx/......
/// Will act 8 be used in combination with act 7 to effect a change of bit 7
/// without affecting bit 6?
/// /../Xx/, /../01/, /../10/
/// /Xx/Xx/, /Xx/Xx/, /Xx/Xx/
/// /Xx/XX/, /Xx/00/, /Xx/11/
pub fn dom0_act8(cur: &SomeState, _cmask: Option<&SomeMask>) -> SomeState {
    let new_state = cur.toggle_bits(vec![6]);
    println!(
        "\nDom 0 Act 8 {} -> {} change: {}",
        cur,
        new_state,
        SomeChange::new_from_to(&cur, &new_state)
    );
    return new_state;
}

// Domain 1 actions

/// Domain 1, act 0, actions, given the current state.
/// Toggle bit 6.
pub fn dom1_act0(cur: &SomeState, _cmask: Option<&SomeMask>) -> SomeState {
    let new_state = cur.toggle_bits(vec![6]);
    println!(
        "\nDom 1 Act 0 {} -> {} change: {}",
        cur,
        new_state,
        SomeChange::new_from_to(&cur, &new_state)
    );
    return new_state;
}

/// Domain 1, act 1, actions, given the current state.
/// Toggle bit 7.
pub fn dom1_act1(cur: &SomeState, _cmask: Option<&SomeMask>) -> SomeState {
    let new_state = cur.toggle_bits(vec![7]);
    println!(
        "\nDom 1 Act 1 {} -> {} change: {}",
        cur,
        new_state,
        SomeChange::new_from_to(&cur, &new_state)
    );
    return new_state;
}

/// Domain 1, act 2, actions, given the current state.
/// Toggle bit 8.
pub fn dom1_act2(cur: &SomeState, _cmask: Option<&SomeMask>) -> SomeState {
    let new_state = cur.toggle_bits(vec![8]);
    println!(
        "\nDom 1 Act 2 {} -> {} change: {}",
        cur,
        new_state,
        SomeChange::new_from_to(&cur, &new_state)
    );
    return new_state;
}

/// Domain 1, act 3, actions, given the current state.
/// Toggle bit 9.
pub fn dom1_act3(cur: &SomeState, _cmask: Option<&SomeMask>) -> SomeState {
    let new_state = cur.toggle_bits(vec![9]);
    println!(
        "\nDom 1 Act 3 {} -> {} change: {}",
        cur,
        new_state,
        SomeChange::new_from_to(&cur, &new_state)
    );
    return new_state;
}
/// Domain 1, act 3, actions, given the current state.
/// Toggle all bits
pub fn dom1_act4(cur: &SomeState, _cmask: Option<&SomeMask>) -> SomeState {
    let new_state = cur.toggle_bits(vec![15, 14, 13, 12, 11, 10, 5, 4, 3, 2, 1, 0]);
    println!(
        "\nDom 1 Act 4 {} -> {} change: {}",
        cur,
        new_state,
        SomeChange::new_from_to(&cur, &new_state)
    );
    return new_state;
}
