#!/bin/bash

cd ..
git clone https://github.com/BIC-MNI/libminc
cd libminc
cmake -DLIBMINC_BUILD_SHARED_LIBS=ON -DLIBMINC_MINC1_SUPPORT=ON . 
make
sudo make install

cd ..
git clone https://github.com/BIC-MNI/minc-tools
mkdir minc-tools/build
cd minc-tools/build
cmake .. -DLIBMINC_DIR=/usr/local/lib
make
sudo make install
