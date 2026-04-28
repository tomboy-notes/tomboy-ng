#!/usr/bin/bash

# Oct 2025, brief and careless script to build packman packages for tomboy-ng
# It should be passed a ver, eg 0.42b, it will pull down the necessary PKGBUILD
# files and call the pacman tool, makepkg which wil, in turn pull down the 
# tomboy-ng source and build away.
# Depends on finding BOTH FPC and Lazarus in PATH, github needs to be tagged with VER
#
# 

if [ "$1" == "" ]; then
	echo "must provide version on command line, eg 0.42b"
	exit
fi


export VER="$1"
PREFILENAME="tomboy-ng-""$VER""-1-x86_64"
POSTFILENAME=".pkg.tar.zst"

function BuildPac () {    # passed Qt6 or gtk2
	cd
	WIDGET="$1"
	rm -Rf "$VER"
	mkdir "$VER"
	cd "$VER"
        wget https://github.com/tomboy-notes/tomboy-ng/raw/refs/heads/master/scripts/PKGBUILD."$WIDGET"
	mv PKGBUILD."$WIDGET" PKGBUILD
	makepkg --skipinteg -f
	mv "$PREFILENAME""$POSTFILENAME"  ../"$PREFILENAME"-"$WIDGET""$POSTFILENAME"
}

BuildPac "Qt6"
BuildPac "gtk2"
ls -l ../*.zst
echo "Install one with, eg $> sudo pacman -U ./tomboy-ng...."
