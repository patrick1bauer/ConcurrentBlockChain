import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Scanner; // Import the Scanner class
import java.util.concurrent.atomic.AtomicInteger;
import java.util.Date;
import java.util.List;



// blockchain class
public class BlockchainMultiThreaded{
	public List<Block> blockchain = new ArrayList<>();
   	// Prefix dictates the difficulty of block mining, the higher the prefix, the more 0s required at the beginning of a block header hash
	static int prefix = 2;
	public volatile boolean nonceFound = false;
	public int size = 0;
	public int numThreads = 0;
	public boolean checked = false;
	public volatile boolean timeToCheck = false;
	public volatile boolean timeToCalc = false;
	public volatile int threadWhoFound = -1;


	// main method
	public static void main(String[] args) {
		BlockchainMultiThreaded chain = new BlockchainMultiThreaded();

		if (args.length > 0) {
			try {
				chain.numThreads = Integer.parseInt(args[0]);
				//System.out.println("Num of threads " + chain.numThreads);
			} 
			catch (NumberFormatException e) 
			{
			}
		}
		else
		{
			chain.numThreads = 2;
		}

		// Print to screen creation of genesis block
		System.out.println("creating genesis block...");

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

		// Instantiate the genesis block
		Block genesisBlock = new Block(data, "", new Date().getTime());
				
		// Create computers and put them in a list to keep track of them
        ArrayList<Computer> computers = new ArrayList<Computer>();
		ArrayList<Thread> threads = new ArrayList<Thread>();
		Thread thread; 
        for(int i = 0; i < chain.numThreads; i++)
        {
			Computer computer = new Computer(i+1, chain, prefix);
            computers.add(computer);
			genesisBlock.setNonce(i);
			computers.get(i).setBlock(genesisBlock);
			computers.get(i).nonce = i;
			thread = new Thread(computer);
			threads.add(thread);
            thread.start();
        }
		startBlock = System.nanoTime();
		// Tell each thread to start calculating
		chain.timeToCalc = true;
		// Wait until we have possibly found the hash
		while(!chain.timeToCheck)
		{
		}

		genesisBlock.setNonce(computers.get(chain.threadWhoFound - 1).nonce);
		genesisBlock.setHash();
		
		// Validate our newly mined genesis block, using null as prev hash
		while(!genesisBlock.validateBlock(chain.blockchain, prefix))
		{
			// Let the threads know the nonce has been checked and has not been found
			chain.timeToCheck = false;
			chain.nonceFound = false;

			// If our validation fails retry mining after resetting values
			genesisBlock.setTimeStamp(new Date().getTime());
			for(int i = 0; i < chain.numThreads; i++)
			{
				// Reset all necessary values
				genesisBlock.setNonce(i);
				computers.get(i).setBlock(genesisBlock);
				computers.get(i).nonce = i;
				computers.get(i).prefix = prefix;
			}

			// Tell each thread to start calculating
			chain.timeToCalc = true;

			// Wait until we have possibly found the hash
			while(!chain.timeToCheck)
			{

			}

			genesisBlock.setNonce(computers.get(chain.threadWhoFound - 1).nonce);
			genesisBlock.setHash();

		}
		
		// Add genesis block to the blockchain
		chain.blockchain.add(genesisBlock);

		endBlock = System.nanoTime();
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
			// Give all threads our new block
			for(int i = 0; i < chain.numThreads; i++)
			{
				// Reset all necessary values
				newBlock.setNonce(i);
				computers.get(i).setBlock(newBlock);
				computers.get(i).nonce = i;
				computers.get(i).prefix = prefix;
			}
			chain.nonceFound = false;
			chain.timeToCheck = false;

			// Tell each thread to start calculating
			chain.timeToCalc = true;

			// Wait until we have possibly found the hash
			while(!chain.timeToCheck)
			{

			}

			newBlock.setNonce(computers.get(chain.threadWhoFound - 1).nonce);
			newBlock.setHash();
			
			while(!newBlock.validateBlock(chain.blockchain, prefix))
			{
				// Let the threads know the nonce has been checked and has not been found
				chain.timeToCheck = false;
				chain.nonceFound = false;

				// If our validation fails retry mining after resetting values
				newBlock.setNonce(0);
				newBlock.setTimeStamp(new Date().getTime());
				for(int i = 0; i < chain.numThreads; i++)
				{
					// Reset all necessary values
					newBlock.setNonce(i);
					computers.get(i).setBlock(newBlock);
					computers.get(i).nonce = i;
					computers.get(i).prefix = prefix;
				}

				// Tell each thread to start calculating
				chain.timeToCalc = true;

				// Wait until we have possibly found the hash
				while(!chain.timeToCheck)
				{
				}

				newBlock.setNonce(computers.get(chain.threadWhoFound - 1).nonce);
				newBlock.setHash();
			}

			// Add our newly mined and verified block and set up values for next block
			chain.blockchain.add(newBlock);
			endBlock = System.nanoTime();
			blockTime = endBlock - startBlock;
			times.add(blockTime);
			previousHash = newBlock.getHash();

		}
		chain.size = chain.blockchain.size() - 1;
		for(int i = 0; i < chain.numThreads; i++)
		{
			computers.get(i).stop();
		}

		// Grab miminum time, maximum time, and total time
		double totalTime = 0.0; 
		for(int i = 0; i < chain.size; i++)
		{
			totalTime += times.get(i);
			if(shortestBlock > times.get(i))
			{
				shortestBlock = times.get(i);
			}
			if(longestBlock < times.get(i))
			{
				longestBlock = times.get(i);
			}
		}
		System.out.println("Blockchain Complete");
		System.out.println("Number of blocks added: " + chain.blockchain.size());
		System.out.println("Total execution time: " + String.format("%.4f",totalTime/ 1000000.00000)+ " ms.");
		String avg = String.format("%.4f", (double)(totalTime / (double)chain.blockchain.size())/ 1000000.00000 );
		System.out.println("Average block execution time: " + avg + " ms.");
		System.out.println("Fastest block execution time: " + String.format("%.4f", shortestBlock/ 1000000.00000) + " ms.");
		System.out.println("Slowest block execution time: " + String.format("%.4f",longestBlock/ 1000000.00000) + " ms.");
		in.close();

	}

	public synchronized void setBlockAndStop(int ID) {
		threadWhoFound = ID;
        timeToCheck = true;
		timeToCalc = false;
		nonceFound = true;
    }

}

// Block class
class Block{

	// Block properties
	public volatile String hash;
	private String previousHash;
	private String data;
	private long timeStamp;
	public AtomicInteger nonce;

	// Block constructor
	public Block(String data, String previousHash, long timeStamp){
		this.data = data;
		this.previousHash = previousHash;
		this.timeStamp = timeStamp;
    	this.nonce = new AtomicInteger(0);
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

	public AtomicInteger getNonce(){
		return this.nonce;
	}

	// Standard setters
	public synchronized void setHash(){
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

	public synchronized void setNonce(int nonce){
		this.nonce.set(nonce);
	}

	// Method to calculate block hash
	public String calculateBlockHash(){
		// Concatenate parts of the block to generate the hash
		String dataToHash = previousHash
			+ Long.toString(timeStamp)
			+ Integer.toString(nonce.intValue())
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

class Computer implements Runnable{
    
	public boolean chainComplete = false;
	public Block currBlock;
    public int ID;
	private BlockchainMultiThreaded chain;
    public int nonce;
    

    public int prefix;

    public Computer(int ID, BlockchainMultiThreaded chain, int prefix) {
		this.ID = ID;
		this.chain = chain;
        this.prefix = prefix;
	}

    public void run()
	{
		while(!chainComplete)
		{
			// Wait until it is time to calculate a hash
			if(chain.timeToCalc)
			{
                mineBlock(prefix, chain.numThreads);
                if(!chain.nonceFound)
                {
					chain.setBlockAndStop(ID);
                }
                else
                {
                    return;
                }
			}
		}
	
    }

	public void stop() {	
		chain.size = chain.size - 2;
		chainComplete = true;
	}

    // Method to mine a block
	public void mineBlock(int prefix, int numThreads){
		// Define the prefix we want to find
		String prefixString = new String(new char[prefix]).replace('\0', '0');
		// Until we find a hash beginning with the correct prefix, meaning we have found a hash smaller than our necessary target
		while(!calculateBlockHash().substring(0, prefix).equals(prefixString)){
			if(chain.nonceFound)
			{
				return;
			}
			// Grab the next nonce
            nonce = nonce + numThreads;
			// Calculate another block hash
    	}

	}

    // Method to calculate block hash
	public String calculateBlockHash(){
		// Concatenate parts of the block to generate the hash
		String dataToHash = currBlock.getPreviousHash()
			+ Long.toString(currBlock.timeStamp())
			+ nonce
			+ currBlock.getData();

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

	public void setBlock(Block currentBlock)
	{
		this.currBlock = currentBlock;
	}
}
