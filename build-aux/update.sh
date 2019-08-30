#!/bin/sh

# These files are usually out of date on distros, largely because many
# of the Autotools components are nearly abandoned. The updated
# version are found in these locations.

# config.rpath
rm config.rpath
wget https://git.savannah.gnu.org/cgit/gnulib.git/plain/build-aux/config.rpath

# depcomp
rm depcomp
wget https://git.savannah.gnu.org/cgit/gnulib.git/plain/build-aux/depcomp
chmod 755 depcomp

# install-sh
rm install-sh
wget https://git.savannah.gnu.org/cgit/gnulib.git/plain/build-aux/install-sh
chmod 755 install-sh

# missing
rm missing
wget https://git.savannah.gnu.org/cgit/automake.git/plain/lib/missing
chmod 755 missing

# test-driver
rm test-driver
wget https://git.savannah.gnu.org/cgit/automake.git/plain/lib/test-driver
chmod 755 test-driver

# texinfo.tex
rm texinfo.tex
wget http://ftp.gnu.org/gnu/texinfo/texinfo.tex
chmod 644 texinfo.tex
