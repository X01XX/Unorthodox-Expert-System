// Implement a store for bits

use crate::bits::SomeBits;
use std::fmt;
use std::ops::Index;
use std::slice::Iter;

impl fmt::Display for BitsStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::from("[");

        for mskx in &self.avec {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &mskx));
            flg = 1;
        }
        rc_str.push(']');

        write!(f, "{}", rc_str)
    }
}

#[derive(Debug)]
pub struct BitsStore {
    pub avec: Vec<SomeBits>,
}

impl BitsStore {
    pub fn _new() -> Self {
        Self {
            avec: Vec::<SomeBits>::with_capacity(5),
        }
    }

    pub fn _len(&self) -> usize {
        self.avec.len()
    }

    pub fn _push(&mut self, val: SomeBits) {
        self.avec.push(val);
    }

    pub fn iter(&self) -> Iter<SomeBits> {
        self.avec.iter()
    }
}

impl Index<usize> for BitsStore {
    type Output = SomeBits;
    fn index<'a>(&'a self, i: usize) -> &'a SomeBits {
        &self.avec[i]
    }
}
