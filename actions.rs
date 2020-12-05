// Actions to be taken by the UES
//
//use crate::bits::SomeBits;
//use crate::mask::SomeMask;
//use crate::region::SomeRegion;
use crate::state::SomeState;

// The number of bits is set by the line of code that creates
// the first current state, with 1, or more, u8 integers.

pub fn action0(cur: &SomeState, hv: usize) -> SomeState {
    let num = 0;

    if cur.is_bit_set(3) && cur.is_bit_set(1) == false     // ...1X0X
        || cur.is_bit_set(3) == false && cur.is_bit_set(1) // ...0X1X
        || cur.is_bit_set(2) && cur.is_bit_set(0)
    {
        // ....X1X1

        let new_state = cur.toggle_bits(vec![0]);
        println!("\nAct {}  {} -> {}", num, cur, new_state);
        return new_state;
    }

    // When the action is created, changing the hv parm (0,1,2), in the
    // main file will have the effect of changing these groups to have
    // 1, 2 or 3 results.

    if cur.is_bit_set(1) {
        // ...101x, 1x10, given that the above is not true

        if hv % 2 == 0 {
            let new_state = cur.toggle_bits(vec![1]);
            println!("\nAct {}  {} -> {}", num, cur, new_state);
            return new_state;
        } else {
            let new_state = cur.toggle_bits(vec![2]);
            println!("\nAct {}  {} -> {}", num, cur, new_state);
            return new_state;
        }
    } else {
        // ...000x, 0x00, given that the above is not true

        if hv % 3 == 0 {
            let new_state = cur.toggle_bits(vec![1]);
            println!("\nAct {}  {} -> {}", num, cur, new_state);
            return new_state;
        } else if hv % 3 == 1 {
            let new_state = cur.toggle_bits(vec![2]);
            println!("\nAct {}  {} -> {}", num, cur, new_state);
            return new_state;
        } else {
            let new_state = cur.toggle_bits(vec![3]);
            println!("\nAct {}  {} -> {}", num, cur, new_state);
            return new_state;
        }
    }
} // end action0

pub fn action1(cur: &SomeState, _hv: usize) -> SomeState {
    let num = 1;
    let new_state = cur.toggle_bits(vec![num]);
    println!("\nAct {}  {} -> {}", num, cur, new_state);
    return new_state;
}

pub fn action2(cur: &SomeState, _hv: usize) -> SomeState {
    let num = 2;
    let new_state = cur.toggle_bits(vec![num]);
    println!("\nAct {}  {} -> {}", num, cur, new_state);
    return new_state;
}

pub fn action3(cur: &SomeState, _hv: usize) -> SomeState {
    let num = 3;
    let new_state = cur.toggle_bits(vec![num]);
    println!("\nAct {}  {} -> {}", num, cur, new_state);
    return new_state;
}

pub fn action4(cur: &SomeState, _num_seen: usize) -> SomeState {
    let num = 4;
    let new_state = cur.toggle_bits(vec![num]);
    println!("\nAct {}  {} -> {}", num, cur, new_state);
    return new_state;
}

pub fn action5(cur: &SomeState, _num_seen: usize) -> SomeState {
    let num = 5;
    let new_state = cur.toggle_bits(vec![num]);
    println!("\nAct {}  {} -> {}", num, cur, new_state);
    return new_state;
}

pub fn action6(cur: &SomeState, _num_seen: usize) -> SomeState {
    let num = 6;
    let new_state = cur.toggle_bits(vec![2,3]);
    println!("\nAct {}  {} -> {}", num, cur, new_state);
    return new_state;
}
