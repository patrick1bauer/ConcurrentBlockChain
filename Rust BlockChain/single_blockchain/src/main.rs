use sha256::digest;

struct Block {
    content: String,
    p_hash: String,
    hash: String,
    nonce: Option<i64>,
}

impl Block {
    fn mine_block(&self, nonce: i64) -> i64 {
        let mut s = gen_hash(&self.content, &self.p_hash, Some(nonce));
        let prefix = s.split_off(2);
        if prefix.eq("00") {
            return nonce
        };

        self.mine_block(nonce + 1)
    }

    fn new_block(content: &str, p_hash: &str) -> Block {
        // will new block (unhashed)
        Block {
            content: String::from(content),
            p_hash: String::from(p_hash),
            hash: gen_hash(&content, p_hash, None),
            nonce: None,
        }
    }

    fn genesis_block(content: &str, p_hash: &str) -> Block {
        Block {
            content: String::from(content),
            p_hash: String::from(p_hash),
            hash: gen_hash(content, p_hash, None),
            nonce: None,
        }
    }
}

fn main() {
    // This is where everything will run
}

fn validate(block: &Block) -> bool {
    // the main function (thread in multi) will run this to verify the nonce is correct
    true
}

fn gen_hash(content: &str, p_hash: &str, nonce: Option<i64>) -> String {
    let the_nonce = match nonce {
        Some(num) => num.to_string(),
        None => String::from(""),
    };
    let s = String::from(content) + &String::from(p_hash) + &the_nonce;
    
    digest(s)
}
