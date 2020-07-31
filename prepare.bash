#!/bin/bash
# copyright David Bannon, 2019, 2020, use as you see fit, but retain this statement.
#

# This script takes a master.zip file from github, tomboy-ng
# and prepares things to build it into source deb
# Move both a fresh master.zip and this script into a clean
# subdirectory, run the script, change to tomboy-ng.{ver}
# and run debuilder -us -uc

# David Bannon, July 2020

APP="tomboy-ng"
DEBEMAIL="tomboy-ng@bannons.id.au"
VER="unknown"
LAZ_BLD=""


	# Looks for fpc and lazbuild on PATH, if in root space, do nothing,
	# if in user space, because debuild will miss them, makes two files.
function CheckFPC_LAZ () {
	echo " ----- laz_bld is $LAZ_BLD --------"
	FPC=`which fpc`
	if [ -x "$FPC" ]; then
		PREFIX="${FPC:0:4}"
		if [ "$PREFIX" = "/usr" ]; then
			echo "root space fpc, all good"
		else
			echo "Leaving a fpc file for buildit"
			echo "$FPC" > WHICHFPC
		fi
	else
		echo "----------- ERROR, no fpc found ------------"
		exit 1
	fi
	if [ "$LAZ_BLD" = "" ]; then 	# we had better try to find it		
		LAZ_BLD=`which lazbuild`
	fi
	if [ -x "$LAZ_BLD" ]; then
		PREFIX="${LAZ_BLD:0:4}"
		if [ "$PREFIX" = "/usr" ]; then
			echo "root space Lazarus, all good"
		else
			echo "Leaving a lazbuild file for buildit"
			echo "$LAZ_BLD" > WHICHLAZ
		fi
	else
		echo " --- ERROR, no lazbuild found, try -l ? ---"
		exit 1
	fi
}


function KControls () {
	wget https://github.com/kryslt/KControls/archive/master.zip   # watch this name does not change.
	unzip -q master.zip
	mv KControls-master "$APP"_"$VER"/kcontrols
}

function ShowHelp () {
    echo " "
    echo "Assumes FPC of some sort in path, available and working, ideally 3.2.0."
    echo "Will look for Lazarus kits in repo, or download (and cache) it." 
    echo "David Bannon, July 2020" 
    echo "-h   print help message"
#    echo "-c   specify CPU, default is x86_64, also supported arm"
    echo "-l   a path to a viable lazbuild, eg at least where lazbuild and lcl is."
    echo "This script is useful in the SRC Deb tool chain only for creating the."
    echo "initial tarball and working directory. Specify a lazbuild to use or"
    echo "next use getlaz.bash to build a minimal laz install to use."
    echo ""
    exit
}


while getopts ":hdc:L:l:" opt; do
  case $opt in
    h)
      ShowHelp
      ;;
    l)
	LAZ_BLD="$OPTARG"
	;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      ShowHelp
      ;;
  esac
done



if [ -f tomboy-ng-master.zip ]; then
	echo " ----- laz_bld is $LAZ_BLD --------"
	CheckFPC_LAZ
	export DEBEMAIL
	unzip -q tomboy-ng-master.zip
	VER=`cat "$APP"-master/package/version`
	mv "$APP-master" "$APP"_"$VER"
	KControls
	cd "$APP"_"$VER"
	rm -Rf experimental
	rm -Rf patches
	# 966537: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=966537
	dch --create --package=tomboy-ng --newversion="$VER" "Initial release of $VER, please see github for details."
	dch --release "blar"
	cd ..
	tar czf "$APP"_"$VER".orig.tar.gz "$APP"_"$VER"
	# which fpc > WHICHFPC
	# echo "/usr/bin/fpc" > WHICHFPC
	# if [ "$LAZ_BLD" = "" ]; then
	#	echo "No Lazarus specified, use getlaz.bash or create a valid a WHICHLAZ"
	#else
	#	echo "$LAZ_BLD" > WHICHLAZ
	#fi
	echo "If no errrors, you should now cd ""$APP"_"$VER; debuild -us -uc"
else
	echo ""
	echo "   Sorry, I cannot see a tomboy-ng-master.zip file. This"
	echo "   script must be run in a directory containing that file"
	echo "   (obtained from github) and probably nothing else."
	echo ""
fi



