#!/bin/bash
set -e
# -------------------------------------------------------------------
#   bash ./test-ppa.bash 0.34d
#
# A script to build tomboy-ng PPA packages, gtk2 and Qt5. Download
# this script, run it from your home dir. It depends on a suitable
# FPC and Lazarus installed in root space. 
# If using your own build FPC or Lazarus, you will have to use the
# the prepare scripts. Easy for Debian because it builds only GTK2
# but if you want to build a QT5 PPA, better look at code below.
# Similarly, if you have had a bad build, and need to inc the 
# -1 after the tomboy-ng version number, its manual.
# copyright David Bannon 2021, License unlimited.
# ------------------------------------------------------
# HISTORTY
# 2022-10-03 Build for focal
# 

# VER="33e"
VER="$1"
DebVer="PPA""$VER"

STARTDIR="$PWD"/

if [ "$1" == "" ]; then
	echo " ERROR, must provide a ver numb, eg 33e or 34"
	exit 1
fi

# cd ..

rm -Rf "$STARTDIR""Build""$DebVer" "Test""$DebVer" 
mkdir "$STARTDIR""Build""$DebVer"; cd "$STARTDIR""Build""$DebVer"
wget https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/scripts/prepare.ppa
wget https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/scripts/mkcontrol.bash
chmod u+x mkcontrol.bash
#cp ../prepare.ppa .
bash ./prepare.ppa -D focal       # was Bionic for GTK2
cd "tomboy-ng_0.""$VER""-1" 
debuild -S
cd ..
if [ ! -f "tomboy-ng_0.""$VER""-1.dsc" ]; then
	echo "======== Failed to make tomboy-ng_0.""$VER""-1.dsc  exiting ======"
	exit 1
fi

cd "$STARTDIR"
#DebVer="$DebVer""QT"
rm -Rf "$STARTDIR""Build""$DebVer"QT "$STARTDIR""Test""$DebVer"QT 
mkdir "$STARTDIR""Build""$DebVer"QT; cd "$STARTDIR""Build""$DebVer"QT
wget https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/scripts/prepare.ppa
#cp ../prepare.ppa .
bash ./prepare.ppa -D focal -Q
cd "tomboy-ng-qt5_0.""$VER""-1" 
debuild -S
cd ..
if [ ! -f "tomboy-ng-qt5_0.""$VER""-1.dsc" ]; then
	echo "======== Failed to make dsc file, exiting ======"
	exit 1
fi

# exit 1

cd ..
cd "$STARTDIR""Build""$DebVer"
mkdir ../Test"$DebVer"
cp *.xz *.gz *.dsc "$STARTDIR"Test"$DebVer" 
cd "$STARTDIR"Test"$DebVer"
dpkg-source -x *.dsc
cd "tomboy-ng-0.""$VER"               # note '-' at start of ver number, not underscore
dpkg-buildpackage -us  -uc 
cd ..
if [ ! -f "tomboy-ng_0.""$VER""-1_amd64.deb" ]; then
	echo "======== Failed to make Deb file, exiting ========"
	exit 1
fi
lintian -IiE --pedantic *.changes

echo "--------- OK, if it looks OK, go back to each build directoy and run -"
echo "          dput ppa:d-bannon/ppa-tomboy-ng *.changes"
