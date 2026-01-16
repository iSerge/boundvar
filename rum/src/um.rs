use std::cell::RefCell;
#[cfg(not(feature = "stacked-arrays"))]
use std::collections::HashMap;
use std::io;
use std::io::Read;
use std::io::Write;
use std::rc::Rc;

pub type Platter = u32;
pub type Array = Vec<Platter>;

#[cfg(feature = "detailed-reports")]
pub type EvalResult = Result<String, String>;

#[cfg(not(feature = "detailed-reports"))]
pub type EvalResult = Result<(), String>;

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

#[cfg(feature = "stacked-arrays")]
pub struct Machine {
    finger: Platter,
    registers: [Platter; 8],
    free_indices: Vec<Platter>,
    arrays: Vec<Rc<RefCell<Option<Array>>>>,
}

#[cfg(not(feature = "stacked-arrays"))]
pub struct Machine {
    finger: Platter,
    registers: [Platter; 8],
    arrays: HashMap<Platter, Array>,
}

impl Machine {
    #[cfg(feature = "stacked-arrays")]
    fn check_finger(&self) -> bool {
        if self.arrays.get(0).is_none() {
            println!("There is no array 0");
            return false;
        }

        let array = self.arrays[0].borrow();
        self.finger as usize >= array.as_ref().map(|a| a.len()).unwrap_or(0)
    }

    #[cfg(not(feature = "stacked-arrays"))]
    fn check_finger(&self) -> bool {
        let Some(arr0) = self.arrays.get(&0) else {
            panic!()
        };
        self.finger as usize >= arr0.len()
    }

    #[cfg(feature = "stacked-arrays")]
    fn get_opcode(&self, idx: Platter) -> Platter {
        if self.arrays.len() == 0 {
            panic!();
        }
        self.arrays[0]
            .borrow()
            .as_ref()
            .map(|a| a[idx as usize])
            .unwrap()
    }

    #[cfg(not(feature = "stacked-arrays"))]
    fn get_opcode(&self, idx: Platter) -> Platter {
        let Some(arr0) = self.arrays.get(&0) else {
            panic!()
        };
        arr0[idx as usize]
    }

    #[cfg(feature = "stacked-arrays")]
    fn array_exists(&self, idx: Platter) -> bool {
        if idx >= self.arrays.len() as Platter {
            return false;
        }

        self.arrays[idx as usize].borrow().is_some()
    }

    #[cfg(not(feature = "stacked-arrays"))]
    fn array_exists(&self, idx: Platter) -> bool {
        self.arrays.contains_key(&idx)
    }

    #[cfg(feature = "stacked-arrays")]
    fn array_len(&self, idx: Platter) -> Platter {
        self.arrays[idx as usize]
            .borrow()
            .as_ref()
            .map(|a| a.len())
            .unwrap() as Platter
    }

    #[cfg(not(feature = "stacked-arrays"))]
    fn array_len(&self, idx: Platter) -> Platter {
        self.arrays.get(&idx).map(|arr| arr.len()).unwrap() as Platter
    }

    #[cfg(feature = "stacked-arrays")]
    fn read_array(&self, idx: Platter, offs: Platter) -> Platter {
        let array = self.arrays[idx as usize].borrow();
        array.as_ref().map(|a| a[offs as usize]).unwrap()
    }

    #[cfg(not(feature = "stacked-arrays"))]
    fn read_array(&self, idx: Platter, offs: Platter) -> Platter {
        self.arrays.get(&idx).map(|arr| arr[offs as usize]).unwrap()
    }

    #[cfg(feature = "stacked-arrays")]
    fn write_array(&mut self, idx: Platter, offs: Platter, value: Platter) {
        if Rc::strong_count(&self.arrays[idx as usize]) > 1 {
            let copy = self.arrays[idx as usize].borrow().clone();
            self.arrays[idx as usize] = Rc::new(RefCell::new(copy));
        }

        let mut array = self.arrays[idx as usize].borrow_mut();
        array.as_mut().unwrap()[offs as usize] = value;
    }

    #[cfg(not(feature = "stacked-arrays"))]
    fn write_array(&mut self, idx: Platter, offs: Platter, value: Platter) {
        self.arrays.get_mut(&idx).unwrap()[offs as usize] = value;
    }

    pub fn eval(&mut self) -> EvalResult {
        if self.check_finger() {
            return Err(format!("Finger refence beyond array capacity"));
        }
        let op = self.get_opcode(self.finger);
        let a = reg_a(op);
        let b = reg_b(op);
        let c = reg_c(op);

        self.finger = self.finger + 1;

        let result = match get_op(op) {
            Op::CondMove => {
                if 0 != self.registers[c] {
                    self.registers[a] = self.registers[b]
                }

                #[cfg(feature = "detailed-reports")]
                {
                    let ra = self.registers[a];
                    let rb = self.registers[b];
                    let rc = self.registers[c];
                    let msg = format!("CondMove: {a}->{ra}, {b}->{rb}, {c}->{rc}");
                    Ok(msg)
                }

                #[cfg(not(feature = "detailed-reports"))]
                {
                    Ok(())
                }
            }
            Op::ArrayIdx => {
                let rb = self.registers[b];
                let rc = self.registers[c];
                if !self.array_exists(rb) {
                    return Err(format!("ArrayIdx reference nonexistent array"));
                }
                if !self.array_len(rb) <= rc {
                    return Err(format!("ArrayIdx reference beyond array capacity"));
                }
                self.registers[a] = self.read_array(rb, rc);

                #[cfg(feature = "detailed-reports")]
                {
                    let ra = self.registers[a];
                    let msg = format!("ArrayIdx: {a}->{ra}, {b}->{rb}, {c}->{rc}");
                    Ok(msg)
                }

                #[cfg(not(feature = "detailed-reports"))]
                {
                    Ok(())
                }
            }
            Op::ArrayAmend => {
                let ra = self.registers[a];
                let rb = self.registers[b];
                let rc = self.registers[c];
                if !self.array_exists(ra) {
                    return Err(format!("ArrayAmend reference nonexistent array"));
                }
                if !self.array_len(ra) <= rb {
                    return Err(format!("ArrayAmend reference beyond array capacity"));
                }
                self.write_array(ra, rb, rc);

                #[cfg(feature = "detailed-reports")]
                {
                    let msg = format!("ArrayAmend: {a}->{ra}, {b}->{rb}, {c}->{rc}");
                    Ok(msg)
                }

                #[cfg(not(feature = "detailed-reports"))]
                {
                    Ok(())
                }
            }
            Op::Add => {
                self.registers[a] = self.registers[b].wrapping_add(self.registers[c]);
                Ok(())
            }
            Op::Mul => {
                self.registers[a] = self.registers[b].wrapping_mul(self.registers[c]);
                Ok(())
            }
            Op::Div => {
                self.registers[a] = self.registers[b].wrapping_div(self.registers[c]);
                Ok(())
            }
            Op::NotAnd => {
                self.registers[a] = !(self.registers[b] & self.registers[c]);
                Ok(())
            }
            Op::Halt => Err(format!("Machine halted")),
            Op::Allocate => {
                let _i;
                #[cfg(feature = "stacked-arrays")]
                {
                    if self.free_indices.is_empty() {
                        let idx = self.arrays.len() as Platter;
                        self.arrays.push(Rc::new(RefCell::new(Some(vec![
                            0;
                            self.registers[c]
                                as usize
                        ]))));
                        self.registers[b] = idx;
                        _i = idx;
                    } else {
                        let Some(idx) = self.free_indices.pop() else {
                            panic!("Trying to get idx from empty vec");
                        };
                        self.arrays[idx as usize] =
                            Rc::new(RefCell::new(Some(vec![0; self.registers[c] as usize])));
                        self.registers[b] = idx;
                        _i = idx;
                    }
                }

                #[cfg(not(feature = "stacked-arrays"))]
                {
                    let mut idx: Platter = 1;
                    while self.array_exists(i) {
                        idx = idx + 1;
                    }
                    self.arrays.insert(idx, vec![0; self.registers[c] as usize]);
                    self.registers[b] = idx;
                    _i = idx
                }

                #[cfg(feature = "detailed-reports")]
                {
                    Ok(format!("Allocate: {_i}"))
                }

                #[cfg(not(feature = "detailed-reports"))]
                {
                    Ok(())
                }
            }
            Op::Abandon => {
                if !self.array_exists(self.registers[c]) {
                    return Err(format!("Attempt to abandon nonexistent array"));
                }

                #[cfg(feature = "stacked-arrays")]
                self.free_indices.push(self.registers[c]);

                #[cfg(not(feature = "stacked-arrays"))]
                self.arrays.remove(&self.registers[c]);

                #[cfg(feature = "detailed-reports")]
                {
                    Ok(format!("Abandon"))
                }

                #[cfg(not(feature = "detailed-reports"))]
                {
                    Ok(())
                }
            }
            Op::Output => {
                let v: [u8; 1] = [(self.registers[c] & 0xFF) as u8];
                let _ = io::stdout().write(&v);
                #[cfg(feature = "detailed-reports")]
                {
                    let vv = self.registers[c];
                    Ok(format!("Output: {vv}"))
                }

                #[cfg(not(feature = "detailed-reports"))]
                {
                    Ok(())
                }
            }
            Op::Input => {
                let mut b: [u8; 1] = [0; 1];
                let mut stdin = io::stdin();
                let _ = stdin.read(&mut b);
                self.registers[c] = b[0] as Platter;

                #[cfg(feature = "detailed-reports")]
                {
                    let bb = b[0];
                    Ok(format!("Input: {bb}"))
                }

                #[cfg(not(feature = "detailed-reports"))]
                {
                    Ok(())
                }
            }
            Op::LoadProg => {
                let rb = self.registers[b];
                if 0 != rb {
                    if !self.array_exists(rb) {
                        return Err(format!("LoadProg references nonexistent array"));
                    }

                    #[cfg(feature = "stacked-arrays")]
                    {
                        let prog = self.arrays[rb as usize].clone();
                        self.arrays[0] = prog;
                    }
                    #[cfg(not(feature = "stacked-arrays"))]
                    {
                        let prog = self.arrays[&rb].clone();
                        self.arrays.insert(0, prog);
                    }
                }
                self.finger = self.registers[c];

                #[cfg(feature = "detailed-reports")]
                {
                    Ok(format!("LoadProg"))
                }

                #[cfg(not(feature = "detailed-reports"))]
                {
                    Ok(())
                }
            }
            Op::Ortho => {
                let a = ((op >> 25) & 0b111) as usize;
                let val = op & 0x01FFFFFF;
                self.registers[a] = val;

                #[cfg(feature = "detailed-reports")]
                {
                    Ok(format!("Orthography: a -> {a}, val -> {val}"))
                }

                #[cfg(not(feature = "detailed-reports"))]
                {
                    Ok(())
                }
            }
            Op::Invalid => Err(format!("Invalid operation")),
        };

        result
    }

    #[cfg(feature = "stacked-arrays")]
    pub fn init(prog: Array) -> Machine {
        Machine {
            finger: 0,
            registers: [0; 8],
            free_indices: vec![],
            arrays: vec![Rc::new(RefCell::new(Some(prog)))],
        }
    }

    #[cfg(not(feature = "stacked-arrays"))]
    pub fn init(prog: Array) -> Machine {
        Machine {
            finger: 0,
            registers: [0; 8],
            arrays: HashMap::from([(0, prog)]),
        }
    }

    #[cfg(feature = "stacked-arrays")]
    #[allow(dead_code)]
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
        for idx in 0..self.arrays.len() {
            if self.array_exists(idx as Platter) {
                print!("({:08X}, {:08X}), ", idx, self.arrays[idx].borrow().as_ref().map(|a| a.len()).unwrap());
            }
        }
        println!("");
    }

    #[cfg(not(feature = "stacked-arrays"))]
    #[allow(dead_code)]
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
