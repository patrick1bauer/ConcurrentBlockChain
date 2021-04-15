import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Scanner; // Import the Scanner class
import java.util.Date;
import java.util.*;


// blockchain class
public class BlockchainSingleThreaded {
	static List<BlockSingle> blockchain = new ArrayList<>();
    // Prefix dictates the difficulty of block mining, the higher the prefix, the more 0s required at the beginning of a block header hash
	static int prefix;

	// main method
	public static void main(String[] args) {
		// Getting prefix
		prefix = Integer.parseInt(args[0]);
		
		// Print to screen creation of genesis block
		//System.out.println("creating genesis block...");

		Scanner in = new Scanner(System.in);
		String data = null;
		// Grab the first file line as data for the genesis block
		data = in.nextLine();

		ArrayList<Double> times = new ArrayList<Double>();
		double shortestBlock = Double.MAX_VALUE;
		double longestBlock = Double.MIN_VALUE;
		double blockTime;
		double startBlock;
		double endBlock;

		startBlock = System.nanoTime();
		// Instantiate the genesis block
		BlockSingle genesisBlock = new BlockSingle(data, "", new Date().getTime());
		// Mine the genesis block, to create the hash for our next block
		genesisBlock.mineBlock(prefix);
		// Validate our newly mined genesis block, using null as prev hash
		while(!genesisBlock.validateBlock(blockchain, prefix))
		{
			// If our validation fails retry mining after resetting values
			genesisBlock.setNonce(0);
			genesisBlock.setTimeStamp(new Date().getTime());
			genesisBlock.mineBlock(prefix);
		}
		// Add genesis block to the blockchain
		blockchain.add(genesisBlock);
		endBlock = System.nanoTime();
		//times.add(endBlock - startBlock);
		//System.out.println("Blockchain initialized");
		//System.out.println("Adding data blocks...");
		String previousHash = genesisBlock.getHash();

		// Continue until we reach the end of the input file
		while(in.hasNextLine()){
			// Read in line
			data = in.nextLine();
			startBlock = System.nanoTime();
			// Create a new block with our line of data
			BlockSingle newBlock = new BlockSingle(data, previousHash, new Date().getTime());
			newBlock.mineBlock(prefix);
			while(!newBlock.validateBlock(blockchain, prefix))
			{
				// If our validation fails retry mining after resetting values
				newBlock.setNonce(0);
				newBlock.setTimeStamp(new Date().getTime());
				newBlock.mineBlock(prefix);
			}
			// Add our newly mined and verified block and set up values for next block
			blockchain.add(newBlock);
			//System.out.println(newBlock.getData());
			endBlock = System.nanoTime();
			blockTime = endBlock - startBlock;
			times.add(blockTime);
			previousHash = newBlock.getHash();
			}
		// Grab miminum time, maximum time, and total time
		double totalTime = 0.0; 
		for(Double time : times)
		{
			totalTime += time;
			if(shortestBlock > time)
			{
				shortestBlock = time;
			}
			if(longestBlock < time)
			{
				longestBlock = time;
			}
		}
		
		String total = String.format("%.4f",totalTime/ 1000000.00000);
		String avg = String.format("%.4f", (double)(totalTime / (double)blockchain.size())/ 1000000.00000 );
		
		//System.out.println("Blockchain Complete");
		//System.out.println("Number of blocks added: " + blockchain.size());
		//System.out.println("Total execution time: " + total + " ms.");
		//System.out.println("Average block execution time: " + avg + " ms.");
		//System.out.println("Fastest block execution time: " + String.format("%.4f", shortestBlock/ 1000000.00000) + " ms.");
		//System.out.println("Slowest block execution time: " + String.format("%.4f",longestBlock/ 1000000.00000) + " ms.");
		
		System.out.println(total + " " + avg);
		in.close();
	}

}

// Block class
class BlockSingle{

	// Block properties
	private String hash;
	private String previousHash;
	private String data;
	private long timeStamp;
	private int nonce;

	// Block constructor
	public BlockSingle(String data, String previousHash, long timeStamp){
		this.data = data;
		this.previousHash = previousHash;
		this.timeStamp = timeStamp;
		this.hash = calculateBlockHash();
    this.nonce = 0;
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

        try {
            // Create a new MessageDigest object which implements SHA-256
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            // Generate the hash value of our input data, which is a byte array
            byteRep = md.digest(dataToHash.getBytes());
        } catch (Exception e) {
            e.printStackTrace();
        }
		// Convert byte array to correctly formatted string
        StringBuffer buffer = new StringBuffer();
    	for (byte b : byteRep) {
        	buffer.append(String.format("%02x", b));
    	}
    	return buffer.toString();
	}

	// Method to mine a block
	public void mineBlock(int prefix){
		// Define the prefix we want to find
		String prefixString = new String(new char[prefix]).replace('\0', '0');

		// Until we find a hash beginning with the correct prefix, meaning we have found a hash smaller than our necessary target
		while(!hash.substring(0, prefix).equals(prefixString)){
			// Increment the nonce
			nonce++;
			// Calculate another block hash
			hash = calculateBlockHash();
    }
    return;
	}

  // Method to validate the block
	public boolean validateBlock(List<BlockSingle> blockchain, int prefix) {
    // Initialize a boolean value to true
    boolean valid = true;

    // The prefix string we want to match our hash prefix to
    String prefixString = new String(new char[prefix]).replace('\0', '0');

    // Local variable to store the size of the blockchain
    int size = blockchain.size();

    // If this blockchain is NOT empty then we are NOT validating the genesis block
    if(size >= 1){
      // Validate the previousHash, hash and prefix
      valid = (previousHash.equals(blockchain.get(size - 1).hash))
            && (hash.equals(calculateBlockHash()))
            && (hash.startsWith(prefixString));
		//System.out.println("prev hash  " + previousHash + " my prv hash" + blockchain.get(size - 1).hash);
		//System.out.println("my hash  " + hash + " recalced hash" + calculateBlockHash());
		//System.out.println("starting w prefix  " + hash.startsWith(prefixString));
    }

    // Otherwise it MUST be the genesis block since the blockchain is empty
    else {
       valid = (previousHash.equals(""))
            && (hash.equals(calculateBlockHash()))
            && (hash.startsWith(prefixString));

    }

    // Return the value of valid
    return valid;
	}
}

		//times.add(endBlock - startBlock);
		System.out.println("Blockchain initialized");
		System.out.println("Adding data blocks...");
		String previousHash = genesisBlock.getHash();

		// Continue until we reach the end of the input file
		while(in.hasNextLine()){
			// Read in line
			data = in.nextLine();
			startBlock = System.nanoTime();
			// Create a new block with our line of data
			Block newBlock = new Block(data, previousHash, new Date().getTime());
			newBlock.mineBlock(prefix);
			while(!newBlock.validateBlock(blockchain, prefix))
			{
				// If our validation fails retry mining after resetting values
				newBlock.setNonce(0);
				newBlock.setTimeStamp(new Date().getTime());
				newBlock.mineBlock(prefix);
			}
			// Add our newly mined and verified block and set up values for next block
			blockchain.add(newBlock);
			//System.out.println(newBlock.getData());
			endBlock = System.nanoTime();
			blockTime = endBlock - startBlock;
			times.add(blockTime);
			previousHash = newBlock.getHash();
			}
		// Grab miminum time, maximum time, and total time
		double totalTime = 0.0; 
		for(Double time : times)
		{
			totalTime += time;
			if(shortestBlock > time)
			{
				shortestBlock = time;
			}
			if(longestBlock < time)
			{
				longestBlock = time;
			}
		}
		System.out.println("Blockchain Complete");
		System.out.println("Number of blocks added: " + blockchain.size());
		System.out.println("Total execution time: " + String.format("%.4f",totalTime/ 1000000.00000)+ " ms.");
		String avg = String.format("%.4f", (double)(totalTime / (double)blockchain.size())/ 1000000.00000 );
		System.out.println("Average block execution time: " + avg + " ms.");
		System.out.println("Fastest block execution time: " + String.format("%.4f", shortestBlock/ 1000000.00000) + " ms.");
		System.out.println("Slowest block execution time: " + String.format("%.4f",longestBlock/ 1000000.00000) + " ms.");
		in.close();
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
    this.nonce = 0;
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

        try {
            // Create a new MessageDigest object which implements SHA-256
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            // Generate the hash value of our input data, which is a byte array
            byteRep = md.digest(dataToHash.getBytes());
        } catch (Exception e) {
            e.printStackTrace();
        }
		// Convert byte array to correctly formatted string
        StringBuffer buffer = new StringBuffer();
    	for (byte b : byteRep) {
        	buffer.append(String.format("%02x", b));
    	}
    	return buffer.toString();
	}

	// Method to mine a block
	public void mineBlock(int prefix){
		// Define the prefix we want to find
		String prefixString = new String(new char[prefix]).replace('\0', '0');

		// Until we find a hash beginning with the correct prefix, meaning we have found a hash smaller than our necessary target
		while(!hash.substring(0, prefix).equals(prefixString)){
			// Increment the nonce
			nonce++;
			// Calculate another block hash
			hash = calculateBlockHash();
    }
    return;
	}

  // Method to validate the block
	public boolean validateBlock(List<Block> blockchain, int prefix) {
    // Initialize a boolean value to true
    boolean valid = true;

    // The prefix string we want to match our hash prefix to
    String prefixString = new String(new char[prefix]).replace('\0', '0');

    // Local variable to store the size of the blockchain
    int size = blockchain.size();

    // If this blockchain is NOT empty then we are NOT validating the genesis block
    if(size >= 1){
      // Validate the previousHash, hash and prefix
      valid = (previousHash.equals(blockchain.get(size - 1).hash))
            && (hash.equals(calculateBlockHash()))
            && (hash.startsWith(prefixString));
		//System.out.println("prev hash  " + previousHash + " my prv hash" + blockchain.get(size - 1).hash);
		//System.out.println("my hash  " + hash + " recalced hash" + calculateBlockHash());
		//System.out.println("starting w prefix  " + hash.startsWith(prefixString));
    }

    // Otherwise it MUST be the genesis block since the blockchain is empty
    else {
       valid = (previousHash.equals(""))
            && (hash.equals(calculateBlockHash()))
            && (hash.startsWith(prefixString));

    }

    // Return the value of valid
    return valid;
	}
}
