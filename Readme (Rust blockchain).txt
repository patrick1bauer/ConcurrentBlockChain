 ____                     ____  _            _        _           _       
|  _ \               _   |  _ \| |          | |      | |         (_)      
| | | | _   _ ___ __| |__| |_) | | ___   ___| | _____| |__   __ _ _ _ __  
| | / /| | | / __|__   __|  _ <| |/ _ \ / __| |/ / __| '_ \ / _` | | '_ \ 
| | \ \| |_| \__ \  | |  | |_) | | (_) | (__|   < (__| | | | (_| | | | | |
|_|  \_\_ _ _/___/  |_|  |____/|_|\___/ \___|_|\_\___|_| |_|\__,_|_|_| |_|                                                                                                   

## TABLE OF CONTENTS
	1. INTRODUCTION ------------------- Includes program purpose and project notes
	2. USAGE -------------------------- Includes compilation and execution instructions
	3. PROJECT STATEMENT -------------- Includes the description of the project

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

This program implements a simple blockchain written in Rust. There are 2 implementations included:
1.) single_blockchain implements a simple blockchain*.
2.) multi_blockchain takes the simple blockchain, and implements a concurrent algorithm.


*When using the term "simple blockchain", we refer to a program that represents a blockchain in theory,
but excludes the vast network of nodes typically involved in validatin the blockchain. 
The blockchain will instead run entirely on your computer, with the purpose of researching blockchain
efficiency in a closed environment.

--------------------------------------------------------------------------------------------

## 2. USAGE

Two Projects are included:
single_blockchain --- A basic, single-threaded implementation of a blockchain
multi_blockchain ---- The previously single-threaded blockchain, upgraded with concurrency


Navigate to the directory containing the program. All main code is in the /src/main.rs. However, to run the project, you only need to be in the /<Project name>/ folder.

You will need to install curl, rust, and cargo.

For curl:

sudo apt-get install curl -y

For Rust:

curl https://sh.rustup.rs -sSf | sh

Manually add the bin directory for Cargo:

source $HOME/.cargo/env
source ~/.profile

for the build-essentials:

sudo apt-get install build-essential -y


In Command Prompt, navigate to the single_blockchain project, and build/run with the following commands:
	cargo build
  	cargo run <prefix> <file name>
	
In Command Prompt, navigate to the multi_blockchain project, and build/run with the following commands:
	cargo build
  	cargo run <prefix> <file name>

--------------------------------------------------------------------------------------------


## 3. PROJECT STATEMENT

Our Goals:
1. To research the efficiency of a multi-threaded blockchain algorithm VS. single-threaded blockchain algorithm
2. To research the change in efficiency of a blockchain across multiple languages.

Our Methodology:
1. Implement a simple, single-threaded blockchain in Java, C++, and Haskell (Rust)
2. Take each language's implementation, and convert it to a multi-threaded version.
3. Compare the efficiency of the single and multi-threaded versions of a language.
4. Compare the effiency of the multi-threaded versions across all languages.

--------------------------------------------------------------------------------------------
