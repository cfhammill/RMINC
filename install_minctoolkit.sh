#!/bin/bash

cd ..
git clone --recursive https://github.com/BIC-MNI/minc-toolkit
cd minc-toolkit
mkdir build 
cd build
cmake ..
make
make install