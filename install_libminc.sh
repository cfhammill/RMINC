#!/bin/bash

cd ..
git clone https://github.com/BIC-MNI/libminc
cd libminc
./configure
make
sudo make install