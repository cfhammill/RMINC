#!/bin/bash

cd ..
git clone --recursive https://github.com/BIC-MNI/minc-toolkit
cd minc-toolkit
mkdir build 
cd build
echo building minc-toolkit
cmake .. #&>/dev/null
echo "  Making:"
make > /dev/null
echo "  Installing:"
make install