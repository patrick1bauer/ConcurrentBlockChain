import java.security.MessageDigest;		// Import the MessageDigest Class
import java.util.Scanner;				// Import the Scanner class
import java.util.Date;
import java.util.*;


// blockchain class
public class BlockchainSingleThreaded {
	static List<Block> blockchain = new ArrayList<>();
	static int prefix = 4;
	static String prefixString = new String(new char[prefix]).replace('\0', '0');
	
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
		
		// Initialize MessageDigest class
		MessageDigest digest = null;
		
		// Initialize the byte array
		byte[] bytes = null;
		
		
		try {
			// Generate an instance of SHA-256 hash
			digest = MessageDigest.getInstance("SHA-256");
			
			// Generate the hash value of our input data, which is a byte array
			bytes = digest.digest(dataToHash.getBytes("UTF-8"));
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		
		// Instantiate the StringBuffer class
		StringBuffer buffer = new StringBuffer();
		
		// For every byte in the byte array
		for(byte b : bytes){
			// Append the byte to the buffer
			buffer.append(String.format("%02x", b));
		}
		
		// Return the buffer as a String
		return buffer.toString();
	}
	
	// Method to mine a block
	public String mineBlock(int prefix){
		// Define the prefix we want to find
		String prefixString = new String(new char[prefix]).replace('\0', '0');
		
		// Until we find a hash with the correct substring (prefix)
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
