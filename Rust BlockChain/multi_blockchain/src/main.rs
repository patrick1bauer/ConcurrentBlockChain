use sha256::digest;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::convert::TryInto;
use std::thread::Builder;
use std::thread;
use std::time::Instant;
use std::time::Duration;
use std::sync::mpsc::{Sender, Receiver};
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use rand::Rng;

static NTHREADS: u32 = 4;

#[derive(Debug)]
struct Block {
    content: String,
    p_hash: String,
    hash: String,
    nonce: Option<i64>,
}

impl Block {
    fn mine_block(&self, nonce: i64, prefix: i64) -> i64 {
        let mut puzzle = String::new();
        let mut num = 0;
        while num != prefix {
            puzzle.push('0');
            num += 1;
        };

        let puzzle = String::from(puzzle);
        let mut s = solve_puzzle(&self.content, &self.p_hash, &self.hash, Some(nonce));
        let _check = s.split_off(prefix.try_into().unwrap());
        if s.eq(&puzzle) {
            return nonce
        };
        -1
    }

    fn get_hash(&self) -> &str {
        &(self.hash)
    }

    fn new_block(content: &str, p_hash: &str, nonce: Option<i64>) -> Block {
        // will new block (unhashed)
        Block {
            content: String::from(content),
            p_hash: String::from(p_hash),
            hash: Block::gen_hash(content, p_hash, None),
            nonce: nonce,
        }
    }

    fn gen_hash(content: &str, p_hash: &str, nonce: Option<i64>) -> String {
        let the_nonce = match nonce {
            Some(num) => num.to_string(),
            None => String::from(""),
        };
        let s = String::from(content) + &String::from(p_hash) + &the_nonce;
        
        digest(s)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        println!("You must run this program with the following command:\n\ncargo run <prefix: i64> <filename: String>\n\nPlease note that the filename should be in the form './filename' and should be present in the current directory. Run this program again with these arguments");
        return;
    }
    let prefix = &args[1];
    let filename = &args[2];

    let prefix: i64 = match prefix.trim().parse() {
        Ok(num) => num,
        Err(_) => {
            println!("You must enter a number for the prefix. The prefix is now automatically set to 2");
            2
        },
    };
 
    let mut blockchain: Vec<Block> = Vec::new();
    let mut times: Vec<f64> = Vec::new();

    let timer_total = Instant::now();

    if let Ok(lines) = read_lines(filename) {
        let data = String::from("The genesis block");
        let p_hash = String::from("00000000000000000000000000000000");
        let gen_block = Block::new_block(&data, &p_hash, None);
        blockchain.push(gen_block);
        for line in lines {
            if let Ok(content) = line {
                let timer_block = Instant::now();
                let a_block = Block::new_block(&content, Block::new_block(&(blockchain[blockchain.len()-1].content), &(blockchain[blockchain.len()-1].p_hash), None).get_hash(), None);
                let (tx, rx): (Sender<i64>, Receiver<i64>) = mpsc::channel();
                let nonce = Arc::new(Mutex::new(0));
                for i in 0..NTHREADS {
                    let (t_nonce, tx) = (Arc::clone(&nonce), tx.clone());
                    let b_block = Block::new_block(&(a_block.content), &(a_block.p_hash), None);
                    let b = Builder::new();
                    let _ = b.spawn(move || {
                        loop {
                            let mut random = rand::thread_rng();
                            let n;
                            let mut lock = t_nonce.try_lock();
                            if let Ok(ref mut nonce) = lock {
                                if **nonce < 0 {
                                    return;
                                }
                                **nonce += 1;
                                n = **nonce;
                                drop(nonce);
                            } else {
                                thread::sleep(Duration::from_millis(random.gen_range(1, 101)));
                                continue;
                            }
                            
                            let val = (&b_block).mine_block(n.into(), prefix);
                            if val > 0 {
                                tx.send(val).unwrap();
                                return;
                            }
                        }
                    }).unwrap_or_else(|e| {
                        println!("{}: {}", i, e);
                        std::process::exit(1);
                    });
                }
                let the_nonce = rx.recv();
                let val = match the_nonce {
                    Ok(num) => num,
                    Err(_) => -1,
                };

                if val > 0 {
                    blockchain.push(Block::new_block(&(a_block.content), &(a_block.p_hash), None));
                    let mut nonce = nonce.lock().unwrap();
                    *nonce = -1;
                }
                let block_time = (timer_block.elapsed().as_micros() as f64) / 1000.0;
                times.push(block_time);
            }
        }
    } else {
        println!("FILE ERROR:\n\nYou must enter a valid file name for your second commandline argument. Make sure your file is in the current directory and input it in the form ./filename\n\n");
        return;
    }
    let total_time = (timer_total.elapsed().as_micros() as f64) / 1000.0;
    let mut avg_time = 0.0;
    let div = times.len() as f64;
    for time in times {
        avg_time = avg_time + time;
    }

    avg_time = avg_time / div;

    println!("{:.6} {:.6}\n", total_time, avg_time);
}

fn solve_puzzle(content: &str, p_hash: &str, hash: &str, nonce: Option<i64>) -> String {
    let the_nonce = match nonce {
        Some(num) => num.to_string(),
        None => String::from(""),
    };
    let s = String::from(content) + &String::from(p_hash) + &String::from(hash) + &the_nonce;
    
    digest(s)
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}