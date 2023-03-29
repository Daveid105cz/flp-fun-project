#!/bin/bash

# first build the project with make, output executable is in this folder
make

# and then run the tests by running the output executable with all the .in files in the test folder
# the first test uses the -i flag and file testInfo.in and its output needs to be exactly the same as input file. if not, print error
./flp22-fun -i test/curve.in > test/testInfo.out
diff test/curve.in test/testInfo.out
if [ $? -eq 0 ]; then
    echo "Test for -i passed"
else
    echo "Test for -i failed"
fi

#another test uses the -s flag with testSign.in and its output is put as input into the program with -v flag 
# and shoudl return a text whose content is a string true, not in a file
./flp22-fun -s test/testSign.in > test/testSign.out
./flp22-fun -v test/testSign.out > test/testVerify.out
grep -q "true" test/testVerify.out
if [ $? -eq 0 ]; then
    echo "Test for -s and -v passed"
else
    echo "Test for -s and -v failed"
fi

#clear all output files
rm test/*.out