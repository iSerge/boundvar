use std::collections::HashMap;
use std::io;
use std::io::Read;
use std::io::Write;

pub type Platter = u32;
pub type Array = Vec<Platter>;

enum Op {
    CondMove,
    ArrayIdx,
    ArrayAmend,
    Add,
    Mul,
    Div,
    NotAnd,
    Halt,
    Allocate,
    Abandon,
    Output,
    Input,
    LoadProg,
    Ortho,
    Invalid,
}

fn get_op(data: Platter) -> Op {
    match data >> 28 {
        0 => Op::CondMove,
        1 => Op::ArrayIdx,
        2 => Op::ArrayAmend,
        3 => Op::Add,
        4 => Op::Mul,
        5 => Op::Div,
        6 => Op::NotAnd,
        7 => Op::Halt,
        8 => Op::Allocate,
        9 => Op::Abandon,
        10 => Op::Output,
        11 => Op::Input,
        12 => Op::LoadProg,
        13 => Op::Ortho,
        _ => Op::Invalid,
    }
}

fn reg_a(data: Platter) -> usize {
    ((data >> 6) & 0b111) as usize
}

fn reg_b(data: Platter) -> usize {
    ((data >> 3) & 0b111) as usize
}

fn reg_c(data: Platter) -> usize {
    (data & 0b111) as usize
}

pub struct Machine {
    finger: Platter,
    registers: [Platter; 8],
    arrays: HashMap<Platter, Array>,
}

impl Machine {
    pub fn eval(&mut self) -> Result<String, String> {
        let Some(arr0) = self.arrays.get(&0) else {
            panic!()
        };
        if self.finger as usize >= arr0.len() {
            return Err(format!("Finger refence beyond array capacity"));
        }
        let op = arr0[self.finger as usize];
        let a = reg_a(op);
        let b = reg_b(op);
        let c = reg_c(op);

        self.finger = self.finger + 1;

        let result = match get_op(op) {
            Op::CondMove => {
                let ra = self.registers[a];
                let rb = self.registers[b];
                let rc = self.registers[c];
                let msg = format!("CondMove: {a}->{ra}, {b}->{rb}, {c}->{rc}");
                if 0 != self.registers[c] {
                    self.registers[a] = self.registers[b]
                }
                Ok(msg)
            }
            Op::ArrayIdx => {
                let ra = self.registers[a];
                let rb = self.registers[b];
                let rc = self.registers[c];
                let msg = format!("ArrayIdx: {a}->{ra}, {b}->{rb}, {c}->{rc}");
                if !self.arrays.contains_key(&rb) {
                    return Err(format!("ArrayIdx reference nonexistent array"));
                }
                if !self.arrays.get(&rb).unwrap().len() <= rc as usize {
                    return Err(format!("ArrayIdx reference beyond array capacity"));
                }
                self.registers[a] = self.arrays.get(&rb).unwrap()[rc as usize];
                Ok(msg)
            }
            Op::ArrayAmend => {
                let ra = self.registers[a];
                let rb = self.registers[b];
                let rc = self.registers[c];
                let msg = format!("ArrayAmend: {a}->{ra}, {b}->{rb}, {c}->{rc}");
                if !self.arrays.contains_key(&ra) {
                    return Err(format!("ArrayAmend reference nonexistent array"));
                }
                if !self.arrays.get(&ra).unwrap().len() <= rb as usize {
                    return Err(format!("ArrayAmend reference beyond array capacity"));
                }
                self.arrays.get_mut(&ra).unwrap()[rb as usize] = rc;
                Ok(msg)
            }
            Op::Add => {
                self.registers[a] = self.registers[b].wrapping_add(self.registers[c]);
                Ok(format!("Add"))
            }
            Op::Mul => {
                self.registers[a] = self.registers[b].wrapping_mul(self.registers[c]);
                Ok(format!("Add"))
            }
            Op::Div => {
                self.registers[a] = self.registers[b].wrapping_div(self.registers[c]);
                Ok(format!("Add"))
            }
            Op::NotAnd => {
                self.registers[a] = !(self.registers[b] & self.registers[c]);
                Ok(format!("Add"))
            }
            Op::Halt => Err(format!("Machine halted")),
            Op::Allocate => {
                let mut i: Platter = 1;
                while self.arrays.contains_key(&i) {
                    i = i + 1;
                }
                self.arrays.insert(i, vec![0; self.registers[c] as usize]);
                self.registers[b] = i;
                Ok(format!("Allocate: {i}"))
            }
            Op::Abandon => {
                if !self.arrays.contains_key(&self.registers[c]) {
                    return Err(format!("Attempt to abandon nonexistent array"));
                }
                self.arrays.remove(&self.registers[c]);
                Ok(format!("Abandon"))
            }
            Op::Output => {
                let v: [u8; 1] = [(self.registers[c] & 0xFF) as u8];
                let _ = io::stdout().write(&v);
                let vv = self.registers[c];
                Ok(format!("Output: {vv}"))
            }
            Op::Input => {
                let mut b: [u8; 1] = [0; 1];
                let mut stdin = io::stdin();
                let _ = stdin.read(&mut b);
                self.registers[c] = b[0] as Platter;
                let bb = b[0];
                Ok(format!("Input: {bb}"))
            }
            Op::LoadProg => {
                let rb = self.registers[b];
                if 0 != rb {
                    if !self.arrays.contains_key(&rb) {
                        return Err(format!("LoadProg references nonexistent array"));
                    }
                    let prog = self.arrays[&rb].clone();
                    self.arrays.insert(0, prog);
                }
                self.finger = self.registers[c];
                Ok(format!("LoadProg"))
            }
            Op::Ortho => {
                let a = ((op >> 25) & 0b111) as usize;
                let val = op & 0x01FFFFFF;
                self.registers[a] = val;
                Ok(format!("Orthography: a -> {a}, val -> {val}"))
            }
            Op::Invalid => Err(format!("Invalid operation")),
        };

        result
    }

    pub fn init(prog: Array) -> Machine {
        Machine {
            finger: 0,
            registers: [0; 8],
            arrays: HashMap::from([(0, prog)]),
        }
    }

    pub fn print_state(&self) {
        println!("Registers:");
        println!(
            "    {:08X}, {:08X}, {:08X}, {:08X},",
            self.registers[0], self.registers[1], self.registers[2], self.registers[3]
        );
        println!(
            "    {:08X}, {:08X}, {:08X}, {:08X},",
            self.registers[4], self.registers[5], self.registers[6], self.registers[7]
        );
        println!("Finger: {:08X}", self.finger);
        for (idx, arr) in &self.arrays {
            print!("({:08X}, {:08X}), ", idx, arr.len());
        }
        println!("");
    }
}
