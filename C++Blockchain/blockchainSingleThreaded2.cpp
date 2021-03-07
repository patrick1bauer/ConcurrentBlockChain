// Includes
#include <iostream>
#include <string>
#include <list>
#include <iterator>
#include <chrono>
#include <ctime>
#include "sha256.h"

// Standard namespace
using namespace std;

// Blockchain class
class BlockchainSingleThreaded
{
    // Create blockchain (list of blocks)
    static list<Block> blockchain;

    // Difficulty of block mining. Higher is harder
    int prefix = 4;

    // Main method
    int main()
    {
        // Create genesis block
        cout << "Creating genesis block" << endl;
        string genesisData = "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks";
        long currentDate = (chrono::duration_cast<chrono::milliseconds>((chrono::time_point_cast<chrono::milliseconds>(chrono::system_clock::now())).time_since_epoch())).count();
        Block genesisBlock(genesisData, NULL, currentDate);

        // Add genesis block to the blockchain
        blockchain.push_front(genesisBlock);

        // Add more blocks
        while(true)
        {
            // Gather data for new block
            // TODO
            string blockData = "sometext";
            string previousHash = blockchain.front().getHash();
            long currentDate = (chrono::duration_cast<chrono::milliseconds>((chrono::time_point_cast<chrono::milliseconds>(chrono::system_clock::now())).time_since_epoch())).count();

            // Create new block with data
            Block newBlock(blockData, previousHash, currentDate);

            // Mine block on newly created block

            // Validate created block

        }
        return 0;
    }
};

// Block class
class Block
{
    public:
        // Block constructor
        Block(string data, string previousHash, long timeStamp)
        {
            this->data = data;
            this->previousHash = previousHash;
            this->timeStamp = timeStamp;
            this->hash = calculateBlockHash();
        }

        // Getters
        string getHash()
        {
            return this->hash;
        }
        string getPreviousHash()
        {
            return this->previousHash;
        }
        string getData()
        {
            return this->data;
        }
        long getTimeStamp()
        {
            return this->timeStamp;
        }
        int getNonce()
        {
            return this->nonce;
        }

        // Setters
        void setHash(string hash)
        {
            this->hash = calculateBlockHash();
        }
        void setPreviousHash(string previousHash)
        {
            this->previousHash = previousHash;
        }
        void setData(string data)
        {
            this->data = data;
        }
        void setTimeStamp(long timeStamp)
        {
            this->timeStamp = timeStamp;
        }
        void setNonce(int nonce)
        {
            this->nonce = nonce;
        }

    private:
        // Block properties
        string data;
        string hash;
        string previousHash;
        long timeStamp;
        int nonce;

        // Calculate block hash
        string calculateBlockHash()
        {
            // Combine block data
            string blockData = this->previousHash + to_string(timeStamp) + to_string(nonce) + data;

            // Calculate hash based on block's data
            string hashedBlock = sha256(blockData);

            // Return hash
            return hashedBlock;
        }

        // Mine a block
        string mineBlock(int prefix)
        {
            // Define the prefix we want to find
            string prefixString = to_string(prefix);

            // Find a hash smaller than our necessary target
            while(hash.substr(0, prefix) == prefixString)
            {
                // Increment the nonce
                nonce++;
                
                // Calculate another block hash
                hash = calculateBlockHash();
            }
        }

        // Build a block
        void buildBlock()
        {

        }
        // Validate the block
        bool validateBlock()
        {
            
            return true;
        }
};
