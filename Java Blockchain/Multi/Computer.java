import java.security.MessageDigest;

public class Computer implements Runnable{
    
	public boolean chainComplete = false;

	public Block currBlock;
    public int ID;
	private BlockchainMultiThreaded chain;
    private int nonce;
    

    public int prefix;

    public Computer(int ID, BlockchainMultiThreaded chain, int prefix) {
		this.ID = ID;
		this.chain = chain;
        this.prefix = prefix;
	}

    public void stop() {
		chainComplete = true;
	}

    public void run()
	{
		while(!chainComplete)
		{
			// Wait until it is time to calculate a hash
			if(chain.timeToCalc)
			{
                mineBlock(prefix);
                if(!chain.nonceFound)
                {
                    if(chain.timeToCalc)
                    {
                        currBlock.setNonce(nonce);
                    }
                    if(chain.timeToCalc)
                    {
                        currBlock.setHash();
                    }
                    chain.setBlockAndStop();
                }
                else
                {
                    return;
                }
			}
            else
            {

            }
		}
	
    }

	

    // Method to mine a block
	public void mineBlock(int prefix){
		// Define the prefix we want to find
		String prefixString = new String(new char[prefix]).replace('\0', '0');
        nonce = chain.getNonce();
		// Until we find a hash beginning with the correct prefix, meaning we have found a hash smaller than our necessary target
		while(!calculateBlockHash().substring(0, prefix).equals(prefixString)){
			// Grab the next nonce
            nonce = chain.getNonce();
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
