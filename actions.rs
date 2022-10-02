//! Actions to be taken, creating samples for the UES.
//!
//! These actions are for testing and learning, they should eventually be replaced
//! by actions that make changes outside the program.
//!
//! Storing a function pointer in the SomeAction runs into problems with the parallel crate
//! and the serialization crate.

use crate::mask::SomeMask;
use crate::rule::SomeRule;
use crate::state::SomeState;
use rand::Rng;

/// Take an action given the domain number, action number, current_state and last change mask (if any).
pub fn take_action(
    dom_num: usize,
    act_num: usize,
    cur_state: &SomeState,
    cmask: Option<&SomeMask>,
) -> SomeState {
    if dom_num == 0 {
        if act_num == 0 {
            dom0_act0(cur_state, cmask)
        } else if act_num == 1 {
            dom0_act1(cur_state)
        } else if act_num == 2 {
            dom0_act2(cur_state)
        } else if act_num == 3 {
            dom0_act3(cur_state)
        } else if act_num == 4 {
            dom0_act4(cur_state)
        } else if act_num == 5 {
            dom0_act5(cur_state)
        } else if act_num == 6 {
            dom0_act6(cur_state)
        } else if act_num == 7 {
            dom0_act7(cur_state)
        } else if act_num == 8 {
            dom0_act8(cur_state)
        } else {
            panic!("Dom 0, Uknown Action number {}", act_num);
        }
    } else if dom_num == 1 {
        if act_num == 0 {
            dom1_act0(cur_state)
        } else if act_num == 1 {
            dom1_act1(cur_state)
        } else if act_num == 2 {
            dom1_act2(cur_state)
        } else if act_num == 3 {
            dom1_act3(cur_state)
        } else if act_num == 4 {
            dom1_act4(cur_state)
        } else if act_num == 5 {
            dom1_act5(cur_state)
        } else if act_num == 6 {
            dom1_act6(cur_state)
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
    let new_state;

    if cur.is_bit_set(3) && !cur.is_bit_set(1)     // ...1X0X
        || !cur.is_bit_set(3) && cur.is_bit_set(1) // ...0X1X
        || cur.is_bit_set(2) && cur.is_bit_set(0)
    {
        // ....X1X1
        new_state = cur.toggle_bits("0x01");
    } else if cur.is_bit_set(1) {
        // ...101x, 1x10, alternate between two changes.
        let mut sample_num = rand::thread_rng().gen_range(1..3);
        if let Some(amsk) = cmask {
            if amsk.is_bit_set(1) {
                sample_num = 1;
            } else if amsk.is_bit_set(2) {
                sample_num = 2;
            }
        }

        if sample_num == 2 {
            new_state = cur.toggle_bits("0x02");
        } else if sample_num == 1 {
            new_state = cur.toggle_bits("0x04");
        } else {
            panic!("1/2 change failed!");
        }
    } else {
        // ...000x, 0x00, alternate between 3 changes.
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
            new_state = cur.toggle_bits("0x02");
        } else if sample_num == 1 {
            new_state = cur.toggle_bits("0x04");
        } else if sample_num == 2 {
            new_state = cur.toggle_bits("0x08");
        } else {
            panic!("1/2/3 change failed");
        }
    }

    println!(
        "\nDom 0 Act 0 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
} // end action0

/// Domain 0, act 1, actions, given the current state.
/// Toggle bit 1. ....../Xx/.
pub fn dom0_act1(cur: &SomeState) -> SomeState {
    let new_state = if cur.is_bit_set(1) {
        cur.toggle_bits("0x04")
    } else {
        cur.toggle_bits("0x02")
    };
    println!(
        "\nDom 0 Act 1 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 2, actions, given the current state.
/// Toggle bit 2. ...../Xx/..
pub fn dom0_act2(cur: &SomeState) -> SomeState {
    let new_state = if cur.is_bit_set(1) {
        cur.toggle_bits("0x02")
    } else {
        cur.toggle_bits("0x04")
    };
    println!(
        "\nDom 0 Act 2 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 3, actions, given the current state.
/// Toggle bit 3. ..../Xx/...
pub fn dom0_act3(cur: &SomeState) -> SomeState {
    let new_state = if cur.is_bit_set(3) {
        cur.toggle_bits("0x10")
    } else {
        cur.toggle_bits("0x08")
    };
    println!(
        "\nDom 0 Act 3 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 4, actions, given the current state.
/// Toggle bit 4. .../Xx/...
pub fn dom0_act4(cur: &SomeState) -> SomeState {
    let new_state = if cur.is_bit_set(3) {
        cur.toggle_bits("0x08")
    } else {
        cur.toggle_bits("0x10")
    };
    println!(
        "\nDom 0 Act 4 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 5, actions, given the current state.
/// Toggle bit 5. ../Xx/.....
pub fn dom0_act5(cur: &SomeState) -> SomeState {
    let new_state = cur.toggle_bits("0x20");
    println!(
        "\nDom 0 Act 5 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 6, actions, given the current state.
/// Toggle bit 2,3 ..../Xx/Xx/..
pub fn dom0_act6(cur: &SomeState) -> SomeState {
    let new_state = cur.toggle_bits("0x0c");
    println!(
        "\nDom 0 Act 6 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 7, actions, given the current state.
/// Toggle bit 6 and 7, Xx/Xx/......
pub fn dom0_act7(cur: &SomeState) -> SomeState {
    let new_state = cur.toggle_bits("0xc0");
    println!(
        "\nDom 0 Act 7 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 0, act 8, actions, given the current state.
/// Toggle bit 6 ./Xx/......
/// Will act 8 be used in combination with act 7 to effect a change of bit 7
/// without affecting bit 6?
/// /../Xx/, /../01/, /../10/
/// /Xx/Xx/, /Xx/Xx/, /Xx/Xx/
/// /Xx/XX/, /Xx/00/, /Xx/11/
pub fn dom0_act8(cur: &SomeState) -> SomeState {
    let new_state = cur.toggle_bits("0x40");
    println!(
        "\nDom 0 Act 8 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

// Domain 1 actions

/// Domain 1, act 0, actions, given the current state.
/// Toggle bit 6.
pub fn dom1_act0(cur: &SomeState) -> SomeState {
    let new_state = cur.toggle_bits("0x0020");
    println!(
        "\nDom 1 Act 0 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 1, act 1, actions, given the current state.
/// Toggle bit 7.
pub fn dom1_act1(cur: &SomeState) -> SomeState {
    let new_state = cur.toggle_bits("0x0040");
    println!(
        "\nDom 1 Act 1 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 1, act 2, actions, given the current state.
/// Toggle bit 8.
pub fn dom1_act2(cur: &SomeState) -> SomeState {
    let new_state = cur.toggle_bits("0x0080");
    println!(
        "\nDom 1 Act 2 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}

/// Domain 1, act 3, actions, given the current state.
/// Toggle bit 9.
pub fn dom1_act3(cur: &SomeState) -> SomeState {
    let new_state = cur.toggle_bits("0x0100");
    println!(
        "\nDom 1 Act 3 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}
/// Domain 1, act 4, actions, given the current state.
pub fn dom1_act4(cur: &SomeState) -> SomeState {
    let new_state = cur.toggle_bits("0x0200");
    println!(
        "\nDom 1 Act 4 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}
/// Domain 1, act 5, actions, given the current state.
pub fn dom1_act5(cur: &SomeState) -> SomeState {
    let new_state = cur.toggle_bits("0x0400");
    println!(
        "\nDom 1 Act 5 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}
/// Domain 1, act 6, actions, given the current state.
pub fn dom1_act6(cur: &SomeState) -> SomeState {
    let new_state = if cur.is_bit_set(5) {
        cur.toggle_bits("0x0100")
    } else if cur.is_bit_set(6) {
        cur.toggle_bits("0x0200")
    } else {
        cur.toggle_bits("0x0400")
    };
    println!(
        "\nDom 1 Act 6 {} -> {} R[{}]",
        cur,
        new_state,
        SomeRule::new(cur, &new_state)
    );
    new_state
}
