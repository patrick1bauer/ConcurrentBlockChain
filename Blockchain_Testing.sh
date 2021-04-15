#!/bin/bash

# COP4520 Course Project
# Group 3 - Concurrent Blockchains

# ============================
# Blockchain Testing: Blockchain_Testing.sh
# ============================
# You can run this script at the command line like so:
#   bash Blockchain_Testing.sh


################################################################################
# Shell check.
################################################################################

# These checks ensure the script is not being run through the
# Bourne shell (or any shell other than bash).

if [ "$BASH" != "/bin/bash" ]; then
  echo ""
  echo "Please use bash to run this script, like so: bash JavaBlockchainTesting.sh"
  echo ""
  exit
fi

if [ -z "$BASH_VERSION" ]; then
  echo ""
  echo "Please use bash to run this script, like so: bash JavaBlockchainTesting.sh"
  echo ""
  exit
fi

################################################################################
# Check that all required files are present.
################################################################################

if [ ! -f BlockchainMultiThreaded.java ]; then
	echo ""
	echo " Error: You must place BlockchainMultiThreaded.java in this directory before we can"
	echo "        proceed. Aborting test script."
	echo ""
	exit
elif [ ! -f BlockchainSingleThreaded.java ]; then
	echo ""
	echo " Error: You must place BlockchainSingleThreaded.java in this directory before we can"
	echo "        proceed. Aborting test script."
	echo ""
	exit
elif [ ! -f BlockchainMultiThreaded.cpp ]; then
	echo ""
	echo " Error: You must place BlockchainMultiThreaded.cpp in this directory before we can"
	echo "        proceed. Aborting test script."
	echo ""
	exit
elif [ ! -f BlockchainSingleThreaded.cpp ]; then
	echo ""
	echo " Error: You must place BlockchainSingleThreaded.cpp in this directory before we can"
	echo "        proceed. Aborting test script."
	echo ""
	exit
elif [ ! -f sha256.cpp ]; then
	echo ""
	echo " Error: You must place BlockchainSingleThreaded.cpp in this directory before we can"
	echo "        proceed. Aborting test script."
	echo ""
	exit
elif [ ! -f sha256.h ]; then
	echo ""
	echo " Error: You must place BlockchainSingleThreaded.h in this directory before we can"
	echo "        proceed. Aborting test script."
	echo ""
	exit
#elif [ ! -f BlockchainMultiThreaded.rs ]; then
#	echo ""
#	echo " Error: You must place BlockchainMultiThreaded.rs in this directory before we can"
#	echo "        proceed. Aborting test script."
#	echo ""
#	exit
#elif [ ! -f BlockchainSingleThreaded.rs ]; then
#	echo ""
#	echo " Error: You must place BlockchainSingleThreaded.rs in this directory before we can"
#	echo "        proceed. Aborting test script."
#	echo ""
#	exit
elif [ ! -d Data_Output ]; then
	echo ""
	echo " Error: You must place the Data_Output folder in this directory"
	echo "        before we can proceed. Aborting test script."
	echo ""
	exit
elif [ ! -d Data_Input ]; then
	echo ""
	echo " Error: You must place the Data_Input folder in this directory"
	echo "        before we can proceed. Aborting test script."
	echo ""
	exit
elif [ ! -f Data_Output/JAVA_SingleThreaded_Data.txt ]; then
	echo ""
	echo " Error: You must place JAVA_SingleThreaded_Data.txt in the Data_Output directory"
	echo "        before we can proceed. Aborting test script."
	echo ""
	exit
elif [ ! -f Data_Output/JAVA_MultiThreaded_Data.txt ]; then
	echo ""
	echo " Error: You must place JAVA_MultiThreaded_Data.txt in the Data_Output directory"
	echo "        before we can proceed. Aborting test script."
	echo ""
	exit
elif [ ! -f Data_Output/CPP_SingleThreaded_Data.txt ]; then
	echo ""
	echo " Error: You must place CPP_SingleThreaded_Data.txt in the Data_Output directory"
	echo "        before we can proceed. Aborting test script."
	echo ""
	exit
elif [ ! -f Data_Output/CPP_MultiThreaded_Data.txt ]; then
	echo ""
	echo " Error: You must place CPP_MultiThreaded_Data.txt in the Data_Output directory"
	echo "        before we can proceed. Aborting test script."
	echo ""
	exit
elif [ ! -f Data_Output/RUST_SingleThreaded_Data.txt ]; then
	echo ""
	echo " Error: You must place RUST_SingleThreaded_Data.txt the Data_Output directory"
	echo "        before we can proceed. Aborting test script."
	echo ""
	exit
elif [ ! -f Data_Output/RUST_MultiThreaded_Data.txt ]; then
	echo ""
	echo " Error: You must place RUST_MultiThreaded_Data.txt in the Data_Output directory"
	echo "        before we can proceed. Aborting test script."
	echo ""
	exit
fi

for i in 1 2 3 4 5;
do
	if [ ! -f Data_Input/TestCase$i.txt ]; then
		echo ""
		echo " Error: You must place TestCase$i.txt in this directory before we"
		echo "        can proceed. Aborting test script."
		echo ""
		exit
	fi
done

################################################################################
# Run test cases with input specified at command line (standard test cases).
################################################################################

echo ""
echo "================================================================"
echo "Running test cases against Java Blockchains..."
echo "================================================================"
echo ""

# Make sure any pre-compiled classes get re-compiled.
rm -rf *.class

# Attempt to compile single-threaded blockchain.
javac BlockchainSingleThreaded.java 2> /dev/null
compile_val=$?
if [[ $compile_val != 0 ]]; then
	echo "fail single-threaded (failed to compile)"
fi
#Attempt to compile multi-threaded blockchain.
javac BlockchainMultiThreaded.java 2> /dev/null
compile_val=$?
if [[ $compile_val != 0 ]]; then
	echo "fail multi-threaded (failed to compile)"
fi
		
# Run single-threaded against data
for i in 1 2 3 4 5;
do
	echo "  [Test Case] TestCase $i ... "

	# Run program over each prefix 
	for j in 0 1;
	do
		echo -n "    [Prefix] Prefix $j ... "
	
		# Run single-threaded program. Capture return value to check whether it crashes.
		java BlockchainSingleThreaded <Data_Input/TestCase$i.txt $j 2> Data_Output/JAVA_SingleThreaded_Data.txt> /dev/null
		execution_val=$?
		if [[ $execution_val != 0 ]]; then
			echo "fail single-threaded (program crashed)"
			continue
		fi
		
		# Run multi-threaded program. Capture return value to check whether it crashes.
		java BlockchainMultiThreaded <Data_Input/TestCase$i.txt $j 2> Data_Output/JAVA_MultiThreaded_Data.txt> /dev/null
		execution_val=$?
		if [[ $execution_val != 0 ]]; then
			echo "fail multi-threaded (program crashed)"
			continue
		fi
		
		echo "" 
	done
done

echo ""
echo "================================================================"
echo "Running test cases against C++ Blockchains..."
echo "================================================================"
echo ""

# Make sure any pre-compiled classes get re-compiled.
rm -rf *.class

# Attempt to compile single-threaded blockchain.
g++ -o blockchainSingleThreaded blockchainSingleThreaded.cpp sha256.cpp 2> /dev/null
compile_val=$?
if [[ $compile_val != 0 ]]; then
	echo "fail single-threaded (failed to compile)"
fi
#Attempt to compile multi-threaded blockchain.
g++ -o blockchainMultiThreaded blockchainMultiThreaded.cpp sha256.cpp 2> /dev/null
compile_val=$?
if [[ $compile_val != 0 ]]; then
	echo "fail multi-threaded (failed to compile)"
fi
		
# Run single-threaded against data
for i in 1 2 3 4 5;
do
	echo "  [Test Case] TestCase $i ... "

	# Run program over each prefix
	for j in 0 1;
	do
		echo -n "    [Prefix] Prefix $j ... "
	
		# Run single-threaded program. Capture return value to check whether it crashes.
		.blockchainSingleThreaded <Data_Input/TestCase$i.txt $j 2> Data_Output/CPP_SingleThreaded_Data.txt> /dev/null
		execution_val=$?
		if [[ $execution_val != 0 ]]; then
			echo "fail single-threaded (program crashed)"
			continue
		fi
		
		# Run multi-threaded program. Capture return value to check whether it crashes.
		.blockchainMultiThreaded <Data_Input/TestCase$i.txt $j 2> Data_Output/CPP_MultiThreaded_Data.txt> /dev/null
		execution_val=$?
		if [[ $execution_val != 0 ]]; then
			echo "fail multi-threaded (program crashed)"
			continue
		fi
		
		echo ""
	done
done

echo ""
echo "================================================================"
echo "Running test cases against Rust Blockchains..."
echo "================================================================"
echo "Not implemented yet"

# Make sure any pre-compiled classes get re-compiled.
rm -rf *.class

################################################################################
# Cleanup phase.
################################################################################

rm -f *.class

echo ""
echo "================================================================"
echo "All Tests Complete."
echo "================================================================"
echo ""