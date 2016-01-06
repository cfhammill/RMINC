#!/bin/bash

cd ..
git clone https://github.com/BIC-MNI/libminc
cd libminc
cmake cmake -DLIBMINC_BUILD_SHARED_LIBS=ON -DLIBMINC_MINC1_SUPPORT=ON . 
make
sudo make install