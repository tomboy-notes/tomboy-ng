#!/bin/bash
# copyright David Bannon, 2019, 2020, use as you see fit, but retain this statement.
#

# This script takes a tomboy-ng zip file from github, and prepares things to
# build it a source deb or just run buildit.bash to make a tomboy-ng binary"
# Hardwired data in this script are specific to the Deb source build.

# Move both a fresh tomboy-ng-master.zip and this script into a clean
# subdirectory, run the script, change to tomboy-ng.{ver} and run -
#
#      debuilder -us -uc      if thats OK, then repeat with -
#      debuilder -S
#      dput ppa:d-bannon/ppa-tomboy-ng tomboy-ng_0.29e-1_source.changes [enter]
#

# David Bannon, July 2020

APP="tomboy-ng"
# DEBEMAIL="dbannon@internode.on.net"
DEBEMAIL="tomboy-ng@bannons.id.au"	# This matches cert I use to sign tomboy-ng stuff
DEBFULLNAME="tomboy-ng"			# This matches cert I use to sign tomboy-ng stuff
VER="unknown"
LAZ_BLD=""
UFILES="NO"	# debug tool, update Makefile
CLEAN="NO"	# debug tool, remove files from previous run, assume same ver.
WIDGET=""	# empty says make a GTK2, only other possibility is Qt5

	# Looks for fpc and lazbuild on PATH, if in root space, do nothing,
	# if in user space, because debuild will miss them, makes two files.
function CheckFPC_LAZ () {
	FPC=`which fpc`
	if [ -x "$FPC" ]; then
		PREFIX="${FPC:0:4}"
		if [ "$PREFIX" = "/usr" ]; then
			echo "---------- root space fpc, all good"
		else
			echo "---------- Leaving a fpc file for buildit"
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
			echo "---------- root space Lazarus, all good"
		else
			echo "---------- Leaving a lazbuild file for buildit"
			echo "$LAZ_BLD" > WHICHLAZ
		fi
	else
		echo " --- ERROR, no lazbuild found, try -l ? ---"
		exit 1
	fi
}

	# Here we remove file that are not needed in the Debian SRC kit.
function CleanSource () {
	rm -Rf experimental
	rm -Rf patches
	rm -Rf doc/gallery
	rm -Rf doc/html
	rm -Rf doc/wiki
	rm -Rf po/*.mo
	rm -f  doc/*.svg doc/*.png doc/*.note
	rm -f  glyphs/*.png glyphs/*.ico glyphs/*.svg glyphs/*.icns
	rm -fR glyphs/help
	rm -fR glyphs/demos
	rm -fR kcontrols/demos 
	rm -fR kcontrols/help
}


function KControls () {	
	if [ -e "master.zip" ]; then
		echo "Warning, reusing KControls zip"
	else
		wget https://github.com/kryslt/KControls/archive/master.zip   # watch this name does not change.
	fi
	unzip -q master.zip
	# rm -f master.zip
	mv KControls-master "$APP"_"$VER""-1"/kcontrols
}

function ShowHelp () {
    echo " "
    echo "Assumes FPC of some sort in path, available and working, ideally 3.2.0."
    echo "Needs Lazarus, <=2.0.6 in root space or specified with -l option."
    echo "Needs devscripts preinstalled and maybe an edit of email address above if"
    echo "it is to be used in the DEB SRC tool chain. Its role there is just to create"
    echo "an initial tarball and working directory (including inserting kcontrols)."
    echo "David Bannon, August 2020" 
    echo "-h   print help message"
    echo "-l   a path to a viable lazbuild, eg at least where lazbuild and lcl is."
    echo "-C   clean out deb files from previous run, debug use only."
    echo "-U   update Makefile and/or buildit.bash,   debug use only."
    echo "-Q   Make a Qt5 version instead of default GTK2"
    echo "-p   Pause before creating .orig. to change content, use another term."
    echo ""
    echo "Davo uses: wget https://github.com/tomboy-notes/tomboy-ng/archive/master.zip"
    echo "           mv master.zip tomboy-ng-master.zip"   
    echo "           bash ./prepare.bash -l /home/dbannon/bin/Lazarus/trunk/lazbuild -p"
    exit
}


while getopts "hpQUCl:" opt; do
  case $opt in
    h)
      ShowHelp
      ;;
    l)
	LAZ_BLD="$OPTARG"
	;;
    U)
	UFILES="YES"
	;;
    C)
	CLEAN="YES"
	;;
    p)
	PAUSE="YES"
	;;
    Q)
	WIDGET="Qt5"
	APP="$APP""-qt5"
	;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      ShowHelp
      ;;
  esac
done

echo "---------- laz_bld is $LAZ_BLD"
echo "---------- CLEAN is $CLEAN"
echo "---------- UFILES is $UFILES"

rm -f WHICHFPC WHICHLAZ		

if [ -f tomboy-ng-master.zip ]; then
	CheckFPC_LAZ
	export DEBEMAIL
	export DEBFULLNAME
	unzip -q tomboy-ng-master.zip
	if [ "$UFILES" = "YES" ]; then
		if [ "Makefile" -nt "tomboy-ng-master/Makefile" ]; then
			echo "---------- UPDATING Makefile"
			cp Makefile tomboy-ng-master/Makefile
		fi
		if [ "buildit.bash" -nt "tomboy-ng-master/buildit.bash" ]; then
			echo "---------- UPDATING buildit.bash"
			cp buildit.bash tomboy-ng-master/buildit.bash
		fi
	fi
	VER=`cat "$APP"-master/package/version`
	if [ "$CLEAN" = "YES" ]; then
		echo "---------- Removing existing DEB files"
		rm -Rf "$APP"_"$VER""-1"
		rm -f "tomboy-ng_""$VER"".orig.tar.gz"
		rm -f "tomboy-ng_$VER-1_amd64.buildinfo"
		rm -f "tomboy-ng_$VER-1_amd64.changes"
       		rm -f "tomboy-ng_$VER-1_amd64.deb"
 		rm -f "tomboy-ng_$VER-1.debian.tar.xz"
		rm -f "tomboy-ng_$VER-1.dsc"
		rm -f "tomboy-ng_$VER.orig.tar.gz"
	fi
	mv "$APP-master" "$APP"_"$VER""-1"
	KControls
	cd "$APP"_"$VER""-1"
	CleanSource
	# 966537: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=966537
	dch --create --package=tomboy-ng --newversion="$VER""-1" "Initial release. (Closes: #966537)"
	dch --append "Please see github for change details"
	if [ "$WIDGET" = "Qt5" ]; then
		dch --append "Qt5 version"
		cp "$APP"_"$VER""-1/debian/control.qt5" "$APP"_"$VER""-1/debian/control"
	fi
	dch --release "blar"
	cd ..
	if [ "$PAUSE" = "YES" ]; then
		read -p "Edit things in another term, press Enter."
	fi
	tar czf "$APP"_"$VER".orig.tar.gz "$APP"_"$VER""-1"
	echo "If no errrors, you should now cd ""$APP"_"$VER""-1; debuild -us -uc"
else
	echo ""
	echo "   Sorry, I cannot see a tomboy-ng-master.zip file. This"
	echo "   script must be run in a directory containing that file"
	echo "   (obtained from github) and probably little else."
	echo "   If you used wget to download tomboy-ng, it will be named master.zip,"
	echo "   you should rename it tomboy-ng-master.zip to avoid confusion."
	echo ""
fi



