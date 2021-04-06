use sha256::digest;

struct mut Block {
    content: String,
    p_hash: String,
    hash: String,
    nonce: Option<i64>,
};

impl Block {
    fn mineBlock(&self, nonce: i64) -> i64 {
        // will call genHash with nonce
    }

    fn newBlock(content: String, p_hash: &str) -> Block {
        // will new block (unhashed)
    }

    fn genesisBlock(content: String) -> Block {
        let p_hash = "00000000000000000000000000000000";
        Block {
            content,
            p_hash,
            hash: genHash(content, p_hash, None),
            nonce: None,
        }
    }
}

fn main() {
    // This is where everything will run
}

fn validate(block: &Block) -> bool {
    // the main function (thread in multi) will run this to verify the nonce is correct
}

fn genHash(content: &str, p_hash: &str, nonce: Option<i64>) -> String {
    let new_content = String::from(content);
    let new_p_hash = String::from(p_hash);
    let the_nonce = match nonce {
        Some(num) => num.to_String(),
        None => String::from(""),
    }
    let s = new_content.push(new_p_hash);
    let s = s.push(the_nonce);

    digest(s)
}
