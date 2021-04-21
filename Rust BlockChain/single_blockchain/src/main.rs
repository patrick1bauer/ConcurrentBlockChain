use sha256::digest;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::convert::TryInto;
use devtimer::DevTime;
use std::time::SystemTime;

#[derive(Debug)]
struct Block {
    time_stamp: String,
    content: String,
    p_hash: String,
    hash: String,
    nonce: Option<i64>,
}

impl Block {
    fn mine_block(&self, nonce: i64, puzzle: &str, prefix: i64) -> i64 {
        let mut s = solve_puzzle(&self.time_stamp, &self.content, &self.p_hash, &self.hash, Some(nonce));
        let _check = s.split_off(prefix.try_into().unwrap());
        if s.eq(puzzle) {
            return nonce
        };
        -1
    }

    fn get_hash(&self) -> &str {
        &(self.hash)
    }

    fn new_block(content: &str, p_hash: &str, nonce: Option<i64>) -> Block {
        let time_stamp = String::from(match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            Ok(n) => n.as_secs().to_string(),
            Err(_) => "Error".to_string(),
        });
        Block {
            time_stamp: String::from(&time_stamp),
            content: String::from(content),
            p_hash: String::from(p_hash),
            hash: Block::gen_hash(&time_stamp, content, p_hash, None),
            nonce: nonce,
        }
    }

    fn gen_hash(time_stamp: &str, content: &str, p_hash: &str, nonce: Option<i64>) -> String {
        let the_nonce = match nonce {
            Some(num) => num.to_string(),
            None => String::from(""),
        };
        let s = String::from(content) + &String::from(time_stamp) + &String::from(p_hash) + &the_nonce;
        
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

    let mut puzzle = String::new();
    let mut num = 0;
    while num != prefix {
        puzzle.push('0');
        num += 1;
    };

    let puzzle = String::from(puzzle);
 
    let mut blockchain: Vec<Block> = Vec::new();
    let mut times: Vec<u128> = Vec::new();

    let mut timer_total = DevTime::new_simple();
    let mut timer_blocks = DevTime::new_simple();

    timer_total.start();
    if let Ok(lines) = read_lines(filename) {
        let data = String::from("The genesis block");
        let p_hash = String::from("00000000000000000000000000000000");
        let gen_block = Block::new_block(&data, &p_hash, None);
        blockchain.push(gen_block);
        let mut index = 0;
        for line in lines {
            if let Ok(content) = line {
                timer_blocks.start();
                let mut done = false;
                while !done {
                    let mut i: i64 = 0;
                    let a_block = Block::new_block(&content, Block::new_block(&(blockchain[blockchain.len()-1].content), &(blockchain[blockchain.len()-1].p_hash), None).get_hash(), None);
                    while i < i64::MAX {
                        let val = a_block.mine_block(i, &puzzle, prefix);
                        if val > 0 {
                            let b_block = Block::new_block(&(a_block.content), &(a_block.p_hash), None);
                            blockchain.push(b_block);
                            done = true;
                            break;
                        } else {
                            i += 1;
                        }
                    }
                    if i < i64::MAX {
                        done = true;
                    }
                }
                timer_blocks.stop();
                times.push(timer_blocks.time_in_millis().unwrap());
            }
            index += 1;
        }
    } else {
        println!("FILE ERROR:\n\nYou must enter a valid file name for your second commandline argument. Make sure your file is in the current directory and input it in the form ./filename\n\n");
        return;
    }
    timer_total.stop();
    let mut avg_time = 0;
    let div: u128 = times.len().try_into().unwrap();
    for time in times {
        avg_time = avg_time + time;
    }

    avg_time = avg_time / div;

    println!("{} {}\n", timer_total.time_in_millis().unwrap(), avg_time);
}

fn solve_puzzle(time_stamp: &str, content: &str, p_hash: &str, hash: &str, nonce: Option<i64>) -> String {
    let the_nonce = match nonce {
        Some(num) => num.to_string(),
        None => String::from(""),
    };
    let s = String::from(content) + &String::from(time_stamp) + &String::from(p_hash) + &String::from(hash) + &the_nonce;
    
    digest(s)
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
