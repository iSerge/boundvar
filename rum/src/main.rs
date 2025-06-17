mod um;

use um::Machine;
use um::Platter;

use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    let mut prog: Vec<Platter> = Vec::new();

    let mut arguments = env::args();
    arguments.next();
    while let Some(arg) = arguments.next() {
        let f = File::open(arg);
        let _ = f.and_then(|mut x| {
            let mut buf: [u8; 4] = [0; 4];
            while let Ok(n) = x.read(&mut buf) {
                if 4 == n {
                    let v = u32::from(buf[0]) << 24
                        | u32::from(buf[1]) << 16
                        | u32::from(buf[2]) << 8
                        | u32::from(buf[3]);
                    prog.insert(prog.len(), v);
                    //println!("Read instruction: {v:08x}");
                } else {
                    break;
                }
            }
            Ok(())
        });
    }

    let mut machine = Machine::init(prog);

    loop {
        let r = machine.eval();
        if let Err(msg) = r {
            println!("{msg}");
            break;
        }
        // else {
        //     let Ok(msg) = r else { panic!() };
        //     println!("{msg}");
        // }
    }
}
