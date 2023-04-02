#!/bin/bash

# This script is used to pack the files into a zip file named flp-fun-xpodes05.zip
# The zip file will be created in the current directory

# The zip file will contain the following files:
# Makefile
# all files in the src directory
# all files in the test directory ending with .in
# README

#create the doc directory if it does not exist
mkdir -p doc
cp README ./doc/README
cp test-description.txt ./doc/test-description.txt

# Remove the zip file if it exists
rm -f flp-fun-xpodes05.zip
# Create the zip file
zip flp-fun-xpodes05.zip Makefile src/*.hs doc/README doc/test-description.txt test/*.in

#remove the doc directory
rm -rf doc
# End of file pack.sh