#!/bin/bash
# -------------------------------------------------------------------
# A script to build tomboy-ng Deb packages, gtk2 only. Download
# this script, run it from your home dir. It depends on a suitable
# FPC and Lazarus installed in root space. 
# If using your own build FPC or Lazarus, you will have to use the
# the prepare scripts. 
# If you have had a bad build and need to inc the -1 after the ver
# then you will have to resort to using the prepare.debian manually.
# copyright David Bannon 2021, License unlimited.
# ------------------------------------------------------

# VER="33e"
VER="$1"
DebVer="Debv""$VER"

if [ "$1" == "" ]; then
	echo " ERROR, must provide a ver numb, eg 33e or 34"
	exit 1
fi

cd
rm -Rf "Build""$DebVer" "Test""$DebVer" 
mkdir "Build""$DebVer"; cd "Build""$DebVer"
wget https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/prepare.debian
bash ./prepare.debian -D unstable -n
cd "tomboy-ng_0.""$VER""-1" 
debuild -S
cd ..
if [ ! -f "tomboy-ng_0.""$VER""-1.dsc" ]; then
	echo "======== Failed to make dsc file, exiting ======"
	exit 1
fi
mkdir ../Test"$DebVer"
cp *.xz *.gz *.dsc ../Test"$DebVer" 
cd ../Test"$DebVer"
dpkg-source -x *.dsc
echo "================"
pwd
ls -l

cd "tomboy-ng-0.""$VER"               # note '-' at start of ver number, not underscore
echo "================"
pwd
ls -l
dpkg-buildpackage -us  -uc 
cd ..
if [ ! -f "tomboy-ng_0.""$VER""-1_amd64.deb" ]; then
	echo "======== Failed to make Deb file, exiting ========"
	exit 1
fi
lintian -IiE --pedantic *.changes
