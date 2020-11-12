// Actions to be taken by the UES
//
//use crate::bits::SomeBits;
//use crate::mask::SomeMask;
//use crate::region::SomeRegion;
use crate::state::SomeState;

// The number of bits is set by the line of code that creates
// the first current state, with 1, or more, u8 integers.

pub fn action0(cur: &SomeState, _hv: usize) -> SomeState {
    let num = 0;

    //println!("action 0: hv {}", hv);
    //    if hv == 1 {
    //        let new_state = cur.toggle_bits(&[0]);
    //
    //        println!("\nAct {}  {} -> {}", num, cur, &new_state);
    //        return new_state;
    //    }
    //    let statex = cur.toggle_bits(&[2]);
    //    println!("\nAct {}  {} -> {}", num, cur, &statex);
    //    statex
    //cur.clone()

    if cur.is_bit_set(3) && cur.is_bit_set(1) == false     // 1X0X
        || cur.is_bit_set(3) == false && cur.is_bit_set(1) // 0X1X
        || cur.is_bit_set(2) && cur.is_bit_set(0)
    // X1X1
    {
        //    if num_seen % 2 == 1 {
        let new_state = cur.toggle_bits(&[0]);
        println!("\nAct {}  {} -> {}", num, cur, new_state);
        return new_state;
        //        }
    }

    //else if num_seen % 3 == 2 {
    //    let new_state = cur.toggle_bits(&[2]);
    //    println!("\nAct {}  {} -> {}", num, cur, new_state);
    //    return new_state;
    //	}
    println!("\nAct {}  {} -> {}", num, cur, cur);
    cur.clone()
}

pub fn action1(cur: &SomeState, _hv: usize) -> SomeState {
    let num = 1;
    let new_state = cur.toggle_bits(&[num]);
    println!("\nAct {}  {} -> {}", num, cur, new_state);
    return new_state;
}

pub fn action2(cur: &SomeState, _hv: usize) -> SomeState {
    let num = 2;
    let new_state = cur.toggle_bits(&[num]);
    println!("\nAct {}  {} -> {}", num, cur, new_state);
    return new_state;
}

pub fn action3(cur: &SomeState, _hv: usize) -> SomeState {
    let num = 3;
    let new_state = cur.toggle_bits(&[num]);
    println!("\nAct {}  {} -> {}", num, cur, new_state);
    return new_state;
}

pub fn action4(cur: &SomeState, _num_seen: usize) -> SomeState {
    let num = 4;
    let new_state = cur.toggle_bits(&[num]);
    println!("\nAct {}  {} -> {}", num, cur, new_state);
    return new_state;
}

pub fn action5(cur: &SomeState, _num_seen: usize) -> SomeState {
    let num = 5;
    let new_state = cur.toggle_bits(&[num]);
    println!("\nAct {}  {} -> {}", num, cur, new_state);
    return new_state;
}
