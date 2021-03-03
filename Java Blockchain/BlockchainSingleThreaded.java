import java.math.BigInteger;
import java.security.MessageDigest;
import java.util.Scanner;				// Import the Scanner class
import java.util.Date;
import java.util.*;


// blockchain class
public class BlockchainSingleThreaded {
	static List<Block> blockchain = new ArrayList<>();
    // Prefix dictates the difficulty of block mining, the higher the prefix, the more 0s required at the beginning of a block header hash
	static int prefix = 4;
	
	// main method
	public static void main(String[] args){
		
		// Print to screen creation of genesis block
		System.out.println("creating genesis block...");
		// Create the data for the genesis block
		String genesisData = new String("The Times 03/Jan/2009 Chancellor on brink of second bailout for banks");
		
		// Instantiate the genesis block
		Block genesisBlock = new Block(genesisData, null, new Date().getTime());
		
		// Add genesis block to the blockchain
		blockchain.add(genesisBlock);
		
		/* Continue until we reach the end of the input file
		while(){
			Loop through input file, reading in line by line of text
            Use block constructor and send it new line of data
            mineBlock on newly created block
            upon return, validate created block
                - Should we replicate what would logically occur should this verification fail?
                    (even though we know ours won't)
		}*/
		
		System.out.println(blockchain.get(0).getData());
	}
	
}

// Block class
class Block{
	
	// Block properties
	private String hash;
	private String previousHash;
	private String data;
	private long timeStamp;
	private int nonce;
	
	// Block constructor
	public Block(String data, String previousHash, long timeStamp){
		this.data = data;
		this.previousHash = previousHash;
		this.timeStamp = timeStamp;
		this.hash = calculateBlockHash();
	}
	
	// Standard getters
	public String getHash(){
		return this.hash;
	}
	
	public String getPreviousHash(){
		return this.previousHash;
	}
	
	public String getData(){
		return this.data;
	}
	
	public long timeStamp(){
		return this.timeStamp;
	}
	
	public int getNonce(){
		return this.nonce;
	}
	
	// Standard setters
	public void setHash(){
		this.hash = calculateBlockHash();
	}
	
	public void setPreviousHash(String previousHash){
		this.previousHash = previousHash;
	}
	
	public void setData(String data){
		this.data = data;
	}
	
	public void setTimeStamp(long timeStamp){
		this.timeStamp = timeStamp;
	}
	
	public void setNonce(int nonce){
		this.nonce = nonce;
	}


	// Method to calculate block hash
	public String calculateBlockHash(){
		// Concatenate parts of the block to generate the hash
		String dataToHash = previousHash
			+ Long.toString(timeStamp)
			+ Integer.toString(nonce)
			+ data;
		
        // Initialize the byte array
        byte[] byteRep = null;
        String hashedBlock = null;

        try {
            // Create a new MessageDigest object which implements SHA-256
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            // Generate the hash value of our input data, which is a byte array
            byteRep = md.digest(dataToHash.getBytes());
            // Convert byte array to correctly formatted string
            hashedBlock = String.format("%x", new BigInteger(1, byteRep));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return hashedBlock;
	}
	
	// Method to mine a block
	public String mineBlock(int prefix){
		// Define the prefix we want to find
		String prefixString = new String(new char[prefix]).replace('\0', '0');
		
		// Until we find a hash beginning with the correct prefix, meaning we have found a hash smaller than our necessary target
		while(!hash.substring(0, prefix).equals(prefixString)){
			// Increment the nonce
			nonce++;
			// Calculate another block hash
			hash = calculateBlockHash();
        }	
		return hash;
	}
	
	/* Method to build a block
	public void buildBlock() {
		
	}
	*/
	
	
	/* Method to validat the block
	public boolean validateBlock() {
		
	}
	*/

}
