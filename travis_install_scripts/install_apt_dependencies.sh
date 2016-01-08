# remove special apt repositories to install hdf5
sudo add-apt-repository -ry "deb http://cran.rstudio.com//bin/linux/ubuntu $(lsb_release -cs)/"
sudo add-apt-repository -ry "ppa:marutter/c2d4u"
 
sudo apt-get update -qq
sudo apt-get install -y libvtk5-dev \
                        python-dev \
                        python-vtk \
                        libssh2-1 \
                        libssh2-1-dev \
                        libcurl4-openssl-dev \
                        libhdf5-serial-dev \
                        bison \
                        flex

 # Add them back for downstream stuff
 sudo add-apt-repository -y "deb http://cran.rstudio.com//bin/linux/ubuntu $(lsb_release -cs)/"
 sudo add-apt-repository -y "ppa:marutter/c2d4u"
 sudo apt-get update -qq