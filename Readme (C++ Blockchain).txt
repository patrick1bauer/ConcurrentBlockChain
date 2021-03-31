  _____                 ____  _            _        _           _       
 / ____|    _     _    |  _ \| |          | |      | |         (_)      
| |       _| |_ _| |_  | |_) | | ___   ___| | _____| |__   __ _ _ _ __  
| |      |_   _|_   _| |  _ <| |/ _ \ / __| |/ / __| '_ \ / _` | | '_ \ 
| |____    |_|   |_|   | |_) | | (_) | (__|   < (__| | | | (_| | | | | |
 \_____|               |____/|_|\___/ \___|_|\_\___|_| |_|\__,_|_|_| |_|                                                                                                                       

## TABLE OF CONTENTS
	1. INTRODUCTION ------------------- Includes program purpose and project notes
	2. USAGE -------------------------- Includes compilation and execution instructions, and output format
	3. EVALUATION --------------------- Includes statements and proof of correctness/efficiency/etc.
	4. PROJECT STATEMENT -------------- Includes the description of the project
	5. [Blank space for later additions]

--------------------------------------------------------------------------------------------

## 1. INTRODUCTION

COP 4520, Spring 2021, Team 3:
- Shelby Menown
- Stacey Dale
- Patrick Bauer
- Robert Pomichter
- Hannah Moss

COP 4520.001: Concepts of Parallel and Distributed Processing with Professor Damian Dechev (Spring 2021)
Department of Computer Science, College of College of Engineering and Computer Science at UCF

This program implements a simple blockchain written in C++. There are two versions included:
1.) blockchainSingleThreaded.cpp implements a simple blockchain*.
2.) blockchainMultiThreaded.cpp takes the simple blockchain, and implements a concurrent algorithm.

*When using the term "simple blockchain", we refer to a program that represents a blockchain in theory,
but excludes the vast network of nodes typically involved in validatin the blockchain. 
The blockchain will instead run entirely on your computer, with the purpose of researching blockchain
efficiency in a closed environment.

--------------------------------------------------------------------------------------------

## 2. USAGE

Two programs are included:
blockchainSingleThreaded.cpp --- A basic, single-threaded implementation of a blockchain
blockchainMultiThreaded.cpp ---- The previously single-threaded blockchain, upgraded with concurrency
sha256.cpp --------------------- An outsourced C++ program to compute the SHA256 hash
sha256.h ----------------------- The header file for the above program
LICENSE.txt -------------------- The license accompanying the use of the SHA256 program

Navigate the console to the directory containing the program.

In Command Prompt, compile and run the blockchainSingleThreaded program with the commands:
	g++ blockchainSingleThreaded.cpp sha256.cpp
	./a.out
	
In Command Prompt, compile and run the blockchainMultiThreaded program with the commands:
	g++ blockchainMultiThreaded.cpp sha256.cpp
	./a.out
	
ADDITIONAL ARGUMENTS:
- Add "-p" to the end of the first line to have the blockchain printed out to a TXT file. (NOTE: This will dramatically increase execution times)
	- This output will be saved to CPPSingleThreadedBlockchain.txt or CPPMultiThreadedBlockchain.txt, depending on which version was executed.
	- Example: g++ blockchainSingleThreaded.cpp sha256.cpp -p

The output of the single-threaded version will be written to a file "CPPSingleThreadedSummary.txt" in 
the same file location as the program.
The multi-threaded version's output is written to "CPPMultiThreadedSummary.txt", also in the same file
location as the program.

Output format:
[Example of variables that get printed, and in what format. I included an example below, 
from the first Programming Assignment. The goal here is that the user can understand what the output means.]

<execution time in seconds>  <total number of primes found>  <sum of all primes found>
<top ten maximum primes, listed in order from lowest to highest>

NOTE: The execution time is calculated in nanoseconds, but printed out in seconds.

--------------------------------------------------------------------------------------------

## 3. EVALUATION

1. Proof of Correctness
[Proof the blockchain functions as intended. This can be by comparing results to another,
already validated blockchain you found, or through discussing the algorithms in depth, and
how they achieve the desired effect.]

2. Efficiency
[Discussion of efficiency between the single and multithreaded versions.]

3. Experimental Evaluation
Each version of the program was tested against the following provided input files:
blockchain_V1.in
blockchain_V2.in
blockchain_V3.in
...
[These don't exist yet, but we're going to need something to compare across versions, and languages.]

Both produce the same ledger, and their execution times over the test files are compared below:
	Test Case:      			[blockchain_V1.in, blockchain_V2.in, blockchain_V3.in, ...]
	Single-Threaded Execution Times: 	[0.0285768s, 0.1294918s, 1.0925716s, ...]
	Multi-Threaded Execution Times: 	[0.0013407s, 0.0078569s, 0.0249165s, ...]

[Include further discussion about these results.]

--------------------------------------------------------------------------------------------

## 4. PROJECT STATEMENT

Our Goals:
1. To research the efficiency of a multi-threaded blockchain algorithm VS. single-threaded blockchain algorithm
2. To research the change in efficiency of a blockchain across multiple languages.

Our Methodology:
1. Implement a simple, single-threaded blockchain in Java, C++, and Haskell.
2. Take each language's implementation, and convert it to a multi-threaded version.
3. Compare the efficiency of the single and multi-threaded versions of a language.
4. Compare the effiency of the multi-threaded versions across all languages.
5. Compare the different between a language's single

--------------------------------------------------------------------------------------------

## 5. [Blank space for later additions]
