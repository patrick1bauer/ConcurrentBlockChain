// Includes
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <iterator>
#include <chrono>
#include <ctime>
#include "sha256.h"

// Standard namespace
using namespace std;

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
        Block(string inputData, string inputPreviousHash, long inputTimeStamp)
        {
            this->data = inputData;
            this->previousHash = inputPreviousHash;
            this->timeStamp = inputTimeStamp;
            this->nonce = 0;
            this->hash = calculateBlockHash();
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
            this->nonce = inputNonce;
        }

        // Method to calculate block hash
        string calculateBlockHash()
        {
            // Concatenate parts of the block to generate the hash
            string dataToHash = this->previousHash + to_string(this->timeStamp) + to_string(this->nonce) + this->data;

            // Calculate hash based on block's data
            string hashedData = sha256(dataToHash);
            // cout << "hashed data: >" + hashedData + "<" << endl;

            // Return hashed data
            return hashedData;
        }

        // Method to mine a block
        void mineBlock(int prefix)
        {
            // Define the prefix we want to find
            string prefixString(prefix, '0');

            // Find a hash smaller than our necessary target
            while(this->hash.substr(0, prefix) != prefixString)
            {
                // Increment the nonce
                this->nonce = this->nonce + 1;

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
            string prefixString(prefix, '0');

            // Local variable to store the size of the blockchain
            int size = blockchain.size();

            // If this blockchain is NOT empty then we are NOT validating the genesis block
            if(size >= 1)
            {
                // Validate the previous hash, hash and prefix
                bool previousHashValid = this->previousHash == blockchain.at(size - 1).hash;
                bool hashValid = this->hash == calculateBlockHash();

                valid = previousHashValid && hashValid;
            }
            // Otherwise it MUST be the genesis block since the blockchain is empty
            else
            {
                string emptyHash = "0000000000000000000000000000000000000000000000000000000000000000";
                bool previousHashValid = this->previousHash == emptyHash;
                bool hashValid = this->hash == calculateBlockHash();

                valid = previousHashValid && hashValid;
            }

            // Return whether the block is valid or not.
            return valid;
        }
    private:
};

// Main method
int main()
{
    // Create blockchain (list of blocks)
    static vector<Block> blockchain;

    // Difficulty of block mining. Higher is harder
    int prefix = 3;

    // Create genesis block
    // Grab the first file line as data for the genesis block
    ifstream inputFile;
    inputFile.open("../gorgias.txt", ios::in);
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
    string emptyHash = "0000000000000000000000000000000000000000000000000000000000000000";
    Block genesisBlock(data, emptyHash, currentDate);

    // Mine the genesis block to create the hash for the next block.
    genesisBlock.mineBlock(prefix);

    // Validate our newly mined genesis block, using null as prev hash.
    while(!genesisBlock.validateBlock(blockchain, prefix))
    {
        // If our validation fails, retry mining after resetting values.
        genesisBlock.setNonce(0);
        long currentDate = (chrono::duration_cast<chrono::milliseconds>((chrono::time_point_cast<chrono::milliseconds>(chrono::system_clock::now())).time_since_epoch())).count();
        genesisBlock.setTimeStamp(currentDate);
        genesisBlock.calculateBlockHash();
        genesisBlock.mineBlock(prefix);
    }

    // Add genesis block to the blockchain
    blockchain.push_back(genesisBlock);

    cout << "Blockchain initialized, begining to add data blocks" << endl;
    // Continue until we reach the end of the input file
    while(inputFile.peek() != EOF)
    {
        // Read in the next line
        if (inputFile.is_open())
        {
            getline(inputFile, data);
            cout << data << endl;
        }
        else
        {
            cout << "[Error]: FILE IS NOT OPEN" << endl;
        }

        // Create a new block with our line of data.
        long currentDate = (chrono::duration_cast<chrono::milliseconds>((chrono::time_point_cast<chrono::milliseconds>(chrono::system_clock::now())).time_since_epoch())).count();
        Block newBlock(data, blockchain.at(blockchain.size() - 1).getHash(), currentDate);
        
        // Mine the new block
        newBlock.mineBlock(prefix);

        // Validate new block
        while(!newBlock.validateBlock(blockchain, prefix))
        {
            // If our validation fails, retry mining after resetting values.
            newBlock.setNonce(0);
            long currentDate = (chrono::duration_cast<chrono::milliseconds>((chrono::time_point_cast<chrono::milliseconds>(chrono::system_clock::now())).time_since_epoch())).count();
            newBlock.setTimeStamp(currentDate);
            newBlock.calculateBlockHash();
            newBlock.mineBlock(prefix);
        }

        // Add our newly mined and verified block and set up values for next block
        blockchain.push_back(newBlock);
    }
    
    // Print out genesis block data
    cout << "BLOCK CHAIN DONE!" << endl;
    // cout << blockchain.front().getData() << endl;

    // Close input file
    inputFile.close();

    return 0;
}