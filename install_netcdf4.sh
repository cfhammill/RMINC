#!/bin/bash

cd ..
wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-4.3.3.1.tar.gz
tar xzf netcdf-4.3.3.1.tar.gz
cd netcdf-4.3.3.1
echo installing netcdf4
./configure > /dev/null
make > /dev/null
sudo make install > /dev/null
