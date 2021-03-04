/*
Disclaimer: This is my first attempt at using C++. If there's an issue, it could
very likely be due to my unfamiliarity with C++, rather than an actual logical mistake.

References:
 - https://medium.com/@vargasjonathan9517/build-a-blockchain-with-c-e164d8319557
 - https://www.baeldung.com/java-blockchain
 - https://justinmeiners.github.io/tiny-blockchain/#1:5
 */

// Importing libraries/headers
#include <iostream>
#include <string>
#include <chrono>
#include <utility>
#include <list>
#include "sha256.h"
#include "date.h"
using namespace std;
using namespace std::chrono;

// Class representing a single block in the chain
class Block
{
    private:
        string hash;         //The current block's hash
        string previousHash; //The previous block's hash
        string data;         //The data in the current block
        string timeStamp;      //The timestamp of when the current block's mining begun
        int nonce;           //An arbitrary number adjusted by the miner

    public:
        // Constructor
        Block(string data, string previousHash, string timeStamp)
        {
            this->data = move(data);
            this->previousHash = move(previousHash);
            this->timeStamp = move(timeStamp);
            this->hash = calculateBlockHash();
        }

        // Function to mine block
        string mineBlock()
        {
            /*
                I've left this blank for now while I get a better understanding of how to
                properly code the mining of a block in C++.
            */
            return "This block has been mined";
        }

        // Function to calculate the SHA256 hash for a block
        string calculateBlockHash()
        {
            /*
                I'm not too confident with this implementation, since it was done in a hurry.
                Take a look when you get a chance.
                Current leads:
                - Crypto++ library (https://stackoverflow.com/questions/31586701/generate-sha256-in-c)
                - http://www.zedwood.com/article/cpp-sha256-function
            */

            string dataToHash = previousHash
                    + timeStamp
                    + std::to_string(nonce)
                    + data;

            return sha256(dataToHash);
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

        // Setters
        void setNonce(int nonce){
            this->nonce = nonce;
        }
};

// Class represented the blockchain
class BlockchainSingleThreaded
{
    public:
        list<Block> blockchain;

        // Constructor
        BlockchainSingleThreaded(){
            // Creates genesis block
            string time = date::format("%F %T", chrono::system_clock::now());
            Block genesisBlock = *new Block("This is the Genesis Block", "None", time);
            genesisBlock.mineBlock();
            blockchain.push_front(genesisBlock);
        }

        // Adds block to blockchain
        void addBlock(string data){
            string time = date::format("%F %T", chrono::system_clock::now());
            auto *newBlock = new Block(move(data), blockchain.end()->getHash(), time);
            newBlock->mineBlock();
        }
};

// Main function for testing
int main()
{
    // Start execution timer
    auto start = high_resolution_clock::now();

    // Start blockchain
    BlockchainSingleThreaded blockchain = *new BlockchainSingleThreaded();

    // Add blocks
    string nextBlockData;
    while (getline(cin, nextBlockData))
    {
        blockchain.addBlock(nextBlockData);
    }

    // Ending execution timer
    auto stop = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(stop - start);

    // Printing test data results
    cout << "Execution time: " << duration.count() << endl;

    // Ending program
    return 0;
}
