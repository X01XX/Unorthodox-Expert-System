// Implement a store for bits

use crate::bits::SomeBits;
use std::fmt;
use std::ops::Index;
//use std::slice::Iter;

impl fmt::Display for BitsStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[derive(Debug)]
pub struct BitsStore {
    pub avec: Vec<SomeBits>,
}

impl BitsStore {
    pub fn _new() -> Self {
        Self {
            avec: Vec::<SomeBits>::new(),
        }
    }

    pub fn _len(&self) -> usize {
        self.avec.len()
    }

    pub fn _push(&mut self, val: SomeBits) {
        self.avec.push(val);
    }

    //    pub fn iter(&self) -> Iter<SomeBits> {
    //        self.avec.iter()
    //    }

    pub fn formatted_string_length(&self) -> usize {
        let mut rc_len = 2;

        if self.avec.len() > 0 {
            rc_len += self.avec.len() * self.avec[0].formatted_string_length();
            if self.avec.len() > 1 {
                rc_len += (self.avec.len() - 1) * 2;
            }
        }

        rc_len
    }

    pub fn formatted_string(&self) -> String {
        let mut flg = 0;
        let mut rc_str = String::with_capacity(self.formatted_string_length());
        rc_str.push('[');

        for mskx in &self.avec {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &mskx));
            flg = 1;
        }
        rc_str.push(']');

        rc_str
    }
}

impl Index<usize> for BitsStore {
    type Output = SomeBits;
    fn index<'a>(&'a self, i: usize) -> &'a SomeBits {
        &self.avec[i]
    }
}
