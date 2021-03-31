// Includes
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <iterator>
#include <chrono>
#include <ctime>
#include <iomanip>
#include "sha256.h"

// Namespaces
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
    // Create a vector of block times
    vector<float> executionTimes;

    // Start timer
    long startBlockchain = (chrono::duration_cast<chrono::microseconds>((chrono::time_point_cast<chrono::microseconds>(chrono::system_clock::now())).time_since_epoch())).count();

    // Create blockchain (list of blocks)
    vector<Block> blockchain;

    // Difficulty of block mining. Higher is harder
    int prefix = 2;

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

    cout << "Blockchain initialized" << endl << "Adding data blocks..." << endl;

    // Continue adding blocks until we reach the end of the input file
    while(inputFile.peek() != EOF)
    {
        // Start timer for this block
        long startBlock = (chrono::duration_cast<chrono::microseconds>((chrono::time_point_cast<chrono::microseconds>(chrono::system_clock::now())).time_since_epoch())).count();

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

        // End timer for this block
        long endBlock = (chrono::duration_cast<chrono::microseconds>((chrono::time_point_cast<chrono::microseconds>(chrono::system_clock::now())).time_since_epoch())).count();

        // Calculate execution time for block
        float durationBlock = (endBlock - startBlock) / 1000.f;

        // Add duration to vector
        executionTimes.push_back(durationBlock);
    }

    // Close input file
    inputFile.close();

    // Stop timer
    long stopBlockchain = (chrono::duration_cast<chrono::microseconds>((chrono::time_point_cast<chrono::microseconds>(chrono::system_clock::now())).time_since_epoch())).count();
    cout << "All blocks added!" << endl << endl;

    // Calculate total execution time
    float durationBlockchain = (stopBlockchain - startBlockchain) / 1000.f;

    // Print block execution times to txt file & calculate interesting data features
    float sum = 0.0;
    float averageBlockTime = 0;
    float fastestBlockTime = 100000000.0;
    float slowestBlockTime = 0.0;
    ofstream executionTimeFile;
    executionTimeFile.open("executionTimes.txt");
    if (executionTimeFile.is_open())
    {
        for (int dataEntryIndex = 0; dataEntryIndex < executionTimes.size(); dataEntryIndex++)
        {
            // Write execution time to file
            executionTimeFile << executionTimes.at(dataEntryIndex) << endl;

            // Add execution time to sum
            sum += executionTimes.at(dataEntryIndex);

            // Get fastest execution time
            if (executionTimes.at(dataEntryIndex) < fastestBlockTime)
            {
                fastestBlockTime = executionTimes.at(dataEntryIndex);
            }

            // Get slowest execution time
            if (executionTimes.at(dataEntryIndex) > slowestBlockTime)
            {
                slowestBlockTime = executionTimes.at(dataEntryIndex);
            }
        }

        // Calculate average
        averageBlockTime = sum / (executionTimes.size());
    }
    else
    {
        cout << "[Error]: FILE IS NOT OPEN" << endl;
    }

    // Display interesting data points.
    cout << "=================== Data ===================" << endl;
    cout << setw(32) << left << "Number of blocks added:" << right << to_string(executionTimes.size()) << endl;
    cout << setw(32) << left << "Total execution time:" << right << to_string(durationBlockchain) + " ms." << endl;
    cout << setw(32) << left << "Average block execution time:" << right << to_string(averageBlockTime) + " ms." << endl;
    cout << setw(32) << left << "Fastest block execution time:" << right << to_string(fastestBlockTime) + " ms." << endl;
    cout << setw(32) << left << "Slowest block execution time:" << right << to_string(slowestBlockTime) + " ms." << endl;

    // Print entire blockchain to txt file.
    // cout << blockchain.front().getData() << endl;

    return 0;
}
