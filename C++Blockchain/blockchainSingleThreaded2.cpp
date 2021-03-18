// Includes
#include <iostream>
#include <fstream>
#include <string>
// #include <list> // Lists in C++ don't allow access to elements by index.
#include <vector> // Vectors in C++ do.
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
    static vector<Block> blockchain;

    // Difficulty of block mining. Higher is harder
    int prefix = 4;

    // Main method
    int main()
    {
        // Create genesis block
        cout << "Creating genesis block" << endl;

        // Grab the first file line as data for the genesis block
        ifstream inputFile;
        inputFile.open("gorgias.txt", ios::in);
        string data;
        if (inputFile.is_open())
        {
            getline(inputFile, data);
        }
        else
        {
            cout << "[Error]: FILE IS NOT OPEN" << endl;
        }

        // Instantiate the genesis block
        long currentDate = (chrono::duration_cast<chrono::milliseconds>((chrono::time_point_cast<chrono::milliseconds>(chrono::system_clock::now())).time_since_epoch())).count();
        Block genesisBlock(data, NULL, currentDate);

        // Mine the genesis block to create the hash for the next block.
        genesisBlock.mineBlock(prefix);

        // Validate our newly mined genesis block, using null as prev hash.
        while(!genesisBlock.validateBlock(blockchain, prefix))
        {
            // If our validation fails, retry mining after resetting values.
            genesisBlock.setNonce(0);
            long currentDate = (chrono::duration_cast<chrono::milliseconds>((chrono::time_point_cast<chrono::milliseconds>(chrono::system_clock::now())).time_since_epoch())).count();
            genesisBlock.setTimeStamp(currentDate);
            genesisBlock.mineBlock(prefix);
        }

        // Add genesis block to the blockchain
        blockchain.push_back(genesisBlock);
        string previousHash = genesisBlock.getHash();

        // Continue until we reach the end of the input file
        while(inputFile.peek() != EOF)
        {
            // Read in the next line
            if (inputFile.is_open())
            {
                getline(inputFile, data);
            }
            else
            {
                cout << "[Error]: FILE IS NOT OPEN" << endl;
            }

            // Create a new block with our line of data.
            long currentDate = (chrono::duration_cast<chrono::milliseconds>((chrono::time_point_cast<chrono::milliseconds>(chrono::system_clock::now())).time_since_epoch())).count();
            Block newBlock(data, previousHash, currentDate);
            
            // Mine the new block
            newBlock.mineBlock(prefix);
            while(!newBlock.validateBlock(blockchain, prefix))
            {
                // If our validation fails, retry mining after resetting values.
                newBlock.setNonce(0);
                long currentDate = (chrono::duration_cast<chrono::milliseconds>((chrono::time_point_cast<chrono::milliseconds>(chrono::system_clock::now())).time_since_epoch())).count();
                newBlock.setTimeStamp(currentDate);
                newBlock.mineBlock(prefix);
            }

            // Add our newly mined and verified block and set up values for next block
            blockchain.push_back(newBlock);
            previousHash = newBlock.getHash();
        }
        
        // Print out genesis block data
        cout << blockchain.front().getData() << endl;

        // Close input file
        inputFile.close();

        return 0;
    }
};

// Block class
class Block
{
    public:
        // Block properties
        string data;
        string hash;
        string previousHash;
        long timeStamp;
        int nonce;

        // Block constructor
        Block(string data, string previousHash, long timeStamp)
        {

        }

        // Standard getters
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

        // Standard setters
        void setHash()
        {
            this->hash = calculateBlockHash();
        }

        void setPreviousHash(string inputHash)
        {
            this->previousHash = inputHash;
        }

        void setData(string inputData)
        {
            this->data = inputData;
        }

        void setTimeStamp(long inputTimeStamp)
        {
            this->timeStamp = inputTimeStamp;
        }

        void setNonce(int inputNonce)
        {
            this.nonce = inputNonce;
        }

        // Method to calculate block hash
        string calculateBlockHash()
        {
            // Concatenate parts of the block to generate the hash
            string dataToHash = this->previousHash + to_string(this->timeStamp) + to_string(this->nonce) + this->data;

            // Calculate hash based on block's data
            string hashedData = sha256(dataToHash);

            // Return hashed data
            return hashedData;
        }

        // Method to mine a block
        void mineBlock(int prefix)
        {
            // Define the prefix we want to find
            string prefixString = to_string(prefix);

            // Find a hash smaller than our necessary target
            while(this->hash.substr(0, prefix) == prefixString)
            {
                // Increment the nonce
                nonce++;

                // Calculate another block hash
                this->hash = calculateBlockHash();
            }

            // Finished mining the block
            return;
        }

        // Validate the block
        bool validateBlock(vector<Block> blockchain, int prefix)
        {
            // Initialize a boolean value to true
            bool valid = true;

            // The prefix string we want to match our hash prefix to
            string prefixString = to_string(prefix);

            // Local variable to store the size of the blockchain
            int size = blockchain.size();

            // If this blockchain is NOT empty then we are NOT validating the genesis block
            if(size >= 1)
            {
                // Validate the previous hash, hash and prefix
                valid = (this->previousHash == blockchain.at(size - 1).previousHash) && (this->hash == calculateBlockHash()) && (this->hash.rfind(prefixString, 0) == 0);
            }
            // Otherwise it MUST be the genesis block since the blockchain is empty
            else
            {
                valid = (this->previousHash == "") && (this->hash == calculateBlockHash()) && (this->hash.rfind(prefixString, 0) == 0);
            }

            // Return whether the block is valid or not.
            return valid;
        }

    private:

};
