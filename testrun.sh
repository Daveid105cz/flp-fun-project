#!/bin/bash

# first build the project with make, output executable is in this folder
make

./flp22-fun -i test/test01.in > test/test01.out
diff test/test01.in test/test01.out
if [ $? -eq 0 ]; then
    echo "Test for -i passed"
else
    echo "Test for -i failed"
fi


#clear all output files
rm test/*.out
