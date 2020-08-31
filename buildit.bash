#!/bin/bash
# copyright David Bannon, 2019, 2020, use as you see fit, but retain this statement.
#
# A script to build tomboy-ng from source without using the Lazarus GUI
#
# tomboy-ng depends directly on fpc, lazbuild, lcl and kcontrols.  Because the 
# package based install of fpc/lazarus is quite different to the popular
# installs in user space, we need to allow for both. debuild does not pass the
# user's PATH through so we need to allow for those user space installs.

# * We expect to find FPC (ideally 3.2.0, min 3.0.4) preinstalled. Either on a path
#   indicated by the file ../WHICHFPC or PATH.  Only the ../WHICHFPC method will work
#   if its installed in user space, fpc is not passed through the SRC Deb tool chain. 

# * Lazarus, must find lazbuild, first tries ../WHICHLAZ, then PATH. 
#   If lazbuild is in root space, then lcl is 
#   probably pointed to by /etc/alteratives/lazarus.  If its user space, then
#   we can assume lcl is in the same place as the lazbuild command itself.

# * KControls is bundled into the deb source kit by prepare.bash, its not 
#   present in the github zip file where tomboy-ng calls home. If building
#   from a SRC Deb kit, kcontrols is already in the 'orig' tarball.    

# While really intended to be part of a tool chain to build a Debian Source
# package, its useful as a standalone build tool if all you want is the binary.
# 

# This script runs in the upper tomboy-ng source tree (level with, eg Makefile).

# The files, WHICHFPC and WHICHLAZ are in the directory above, while they can
# created by hand, the script, prepare.bash will also do so if necessary.
# The prepare.bash script will take a github zip file, unpack it, add kcontrols
# and create the necessary ../WHICH* files if it can.  (An optional script, 
# getlaz.bash will build a minimal local lazarus and create its WHICHLAZ.)



# This is where I keep tarballs and zips to avoid repeated large downloads.

MYREPO="$HOME/Documents/Kits"		# set an alterantive with -r
LAZ_VER="trunk"				# an alternative is lazarus-2.0.10-2
LAZ_INT_NAME="blar"

#CPU="x86_64"				# default x86_64, can be arm
CPU=$HOSTTYPE               # might return i686, we change to i386 
OS="linux"
PROJ=Tomboy_NG             # the formal name of the project, it's in project file.
START_DIR=$PWD
SOURCE_DIR="$PWD/source"      	
TARGET="$CPU-$OS"
K_DIR="$PWD/kcontrols/packages/kcontrols"
WIDGET="gtk2"				# either gtk2 or qt5
TEMPCONFDIR=`mktemp -d`
# lazbuild writes, or worse might read a default .lazarus config file. We'll distract it later.

AUTODOWNLOAD=FALSE			# downloading large file, use -d to allow it

# ------------------------ Some functions ------------------------

function ShowHelp () {
    echo " "
    echo "Assumes FPC of some sort in path, available and working, ideally 3.2.0."
    echo "Will look for Lazarus and KControls kits in repo, or download to it." 
    echo "David Bannon, July 2020" 
    echo "-h   print help message"
    echo "-c   specify CPU, default is $HOSTTYPE - supported x86_64, i386, arm"
    echo "-Q   build a Qt5 version (default gtk2)"
    echo "When used in SRC DEB toolchain, set -c (if necessary) options in the Makefile."
    echo ""
    exit 1
}

	# Looks to see if we have a viable fpc, exits if not
function CheckFPC () {
	if [ -f "../WHICHFPC" ]; then		# If existing, the msg files take precedance
		COMP_DIR=`cat ../WHICHFPC | rev | cut -c -4 --complement | rev`
		if [ ! -x "$COMP_DIR""/fpc" ]; then		
			echo "Sorry, WHICHFPC is not a viable compiler"
			echo "---------------------  EXITING ---------------------"
			exit 1
		fi
		PATH="$COMP_DIR":"$PATH"
		export PATH		
	else
		# OK, is it on path ?  This won't work in SRC Deb mode if its installed in user space
		COMP_DIR=`which fpc | rev | cut -c -4 --complement | rev`
		if [ ! -x "$COMP_DIR""/fpc" ]; then		
			echo "Sorry, not finding a viable compiler"
			echo "---------------------  EXITING ---------------------"
			exit 1
		fi
	fi
	COMPILER="$COMP_DIR""/fpc"		# we will need that later
}

	# looks for a lazbuild, first in WHICHLAZ, then in PATH, failing exits.
function CheckLazBuild () {
	if [ -f "../WHICHLAZ" ]; then
		LAZ_DIR=`cat ../WHICHLAZ | rev | cut -c -9 --complement | rev`
		if [ ! -x "$LAZ_DIR""/lazbuild" ]; then
			echo "Sorry, WHICHLAZ is not a viable lazarus install"
			echo "---------------------  EXITING ---------------------"
			exit 1		 
		fi
		PATH="$LAZ_DIR":"$PATH"
		export PATH		
	else
		LAZ_DIR=`which lazbuild | rev | cut -c -9 --complement | rev`
	fi
	if [ ! -x "$LAZ_DIR""/lazbuild" ]; then
		echo "---------- ERROR, cannot find lazbuild ----------"
		exit 1
	fi 
	# if LAZ_DIR starts with /usr then its installed in root space and we should
	# assume that the lcl components are not 'along side' lazbuild.  In fact
	# might be somewhere like /usr/lib/lazarus/2.0.8, should we assume its
	# /etc/alternatives/lazarus ?
	PREFIX="${LAZ_DIR:0:4}"
	if [ "$PREFIX" = "/usr" ]; then
		LAZ_DIR="/etc/alternatives/lazarus"
	fi	

}

		# We default to GTK2 but if a file is left in working dir called
		# Qt5 then we build that. Note a -q does the same thing.
function CheckForQt5 () {
	if [ -f "Qt5" ]; then
		WIDGET="qt5"
	fi
}

# ------------ It all starts here ---------------------


while getopts "hQc:" opt; do
  case $opt in
    h)
      ShowHelp
      ;;
    c)
	CPU="$OPTARG"
	TARGET="$CPU-$OS"
	;;
    Q)
	WIDGET="qt5"
	;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      ShowHelp
      ;;
  esac
done

if [ "$CPU" = "i686" ]; then
    CPU="i386"
fi
TARGET="$CPU-$OS"
CheckFPC
CheckLazBuild
CheckForQt5

# OK, if to here, we have a fpc and lazbuild, but which FPC ?
FPCVERSION=$($COMPILER -iV)
case $FPCVERSION in
	3.0.4)
		EXCLUDEMESSAGE=" -vm2005,5027 "
	;;
	3.2.0)
		EXCLUDEMESSAGE=" -vm6058,2005,5027 "
	;;
	*)
		echo "Compiler reported [$FPCVERSION]"
		echo "Unclear about your compiler, maybe edit script to support new one, exiting ..."
		exit 1
	;;
esac

# 6058 - note about things not being inlined
# 5027 - var not used
# 2005 - level 2 comment

# We can assume we have FPC at this stage, lets try for Lazarus.

K_DIR="$PWD/kcontrols/source"

cd "$K_DIR"		# WARNING, kcontrols is not part of the github zip file, its added by prepare.bash

# Here we build just the kmemo.pas part of kcontrols.

mkdir -p "lib/$TARGET"			# this is where kcontrols object files end up.

FPCKOPT=" -MObjFPC -Scgi -Cg -O1 -g -gl -l -vewnibq -vh- -Fi$K_DIR"
FPCKUNITS=" -Fu$LAZ_DIR/packager/units/$TARGET -Fu$LAZ_DIR/components/lazutils/lib/$TARGET"
FPCKUNITS="$FPCKUNITS -Fu$LAZ_DIR/components/buildintf/units/$TARGET -Fu$LAZ_DIR/components/freetype/lib/$TARGET"
FPCKUNITS="$FPCKUNITS -Fu$LAZ_DIR/lib/$TARGET -Fu$LAZ_DIR/lcl/units/$TARGET -Fu$LAZ_DIR/lcl/units/$TARGET/$WIDGET"
FPCKUNITS="$FPCKUNITS -Fu$LAZ_DIR/components/cairocanvas/lib/$TARGET/$WIDGET -Fu$LAZ_DIR/components/lazcontrols/lib/$TARGET/$WIDGET"
FPCKUNITS="$FPCKUNITS -Fu$LAZ_DIR/components/ideintf/units/$TARGET/$WIDGET -Fu$LAZ_DIR/components/printers/lib/$TARGET/$WIDGET"
FPCKUNITS="$FPCKUNITS -Fu$LAZ_DIR/components/tdbf/lib/$TARGET/$WIDGET -Fu. -FUlib/$TARGET"

RUNIT="$COMPILER $EXCLUDEMESSAGE $FPCKOPT $FPCKUNITS kmemo.pas"

echo "$RUNIT"
echo "-----------------"

$RUNIT

# exit


if [ ! -e "$K_DIR/lib/$CPU-$OS/kmemo.o" ]; then
	echo "ERROR failed to build KControls, exiting..."
	K_DIR=""
	exit 1
fi
cd "$START_DIR"

VERSION=`cat "package/version"`

COMPILER="fpc"

echo "------------------------------------------------------"
echo "OK, we seem to have both Lazarus LCL and KControls available : "
echo "kcontrols = $K_DIR"
echo "Lazarus   = $LAZ_DIR"
echo "Compiler  = $COMPILER"
echo "PATH      = $PATH"
echo "CPU type  = $CPU"
echo "Exclude Compiler Messages = $EXCLUDEMESSAGE"
echo "tomboy-ng version = $VERSION"
# echo "-------------------------------------------------------"

# Test to see if we find the tomboy-ng source. 
if [ ! -e "$SOURCE_DIR/editbox.pas" ]; then
	echo "----------------------------------------------------------------"
	echo "Looked for [$SOURCE_DIR/editbox.pas]"
	echo "Not finding tomboy-ng source, exiting ...."
	echo " "
	ShowHelp
fi

echo "In buildit.bash, ready to start building tomboy" >> "$HOME"/build.log

cd $SOURCE_DIR

# Its necessary to call fpc directly here, lazbuild will not help us because kcontrols
# is not in the location that the project file expects it to be. lazbuild does not
# have an option to add an arbitary extra package location.

# DEBUG options -O1,   (!) -CX, -g, -gl, -vewnhibq

OPT1="-MObjFPC -Scghi -CX -Cg -O3 -XX -Xs -l -vewnibq $EXCLUDEMESSAGE -Fi$SOURCE_DIR/lib/$TARGET"

UNITS="$UNITS -Fu$K_DIR/lib/$TARGET"
UNITS="$UNITS -Fu$LAZ_DIR/components/tdbf/lib/$TARGET/$WIDGET"
UNITS="$UNITS -Fu$LAZ_DIR/components/printers/lib/$TARGET/$WIDGET"
UNITS="$UNITS -Fu$LAZ_DIR/components/cairocanvas/lib/$TARGET/$WIDGET"
UNITS="$UNITS -Fu$LAZ_DIR/components/lazcontrols/lib/$TARGET/$WIDGET"
UNITS="$UNITS -Fu$LAZ_DIR/components/lazutils/lib/$TARGET"
UNITS="$UNITS -Fu$LAZ_DIR/components/ideintf/units/$TARGET/$WIDGET"
UNITS="$UNITS -Fu$LAZ_DIR/lcl/units/$TARGET/$WIDGET"
UNITS="$UNITS -Fu$LAZ_DIR/lcl/units/$TARGET"
UNITS="$UNITS -Fu$LAZ_DIR/packager/units/$TARGET"

UNITS="$UNITS -Fu$SOURCE_DIR/"
UNITS="$UNITS -FU$SOURCE_DIR/lib/$TARGET/" 

OPT2=" -dLCL -dLCL$WIDGET" 
DEFS="-dDisableLCLGIF -dDisableLCLJPEG -dDisableLCLPNM -dDisableLCLTIFF"


# We must force a clean compile, no make looking after us here.
# I have not found a way of telling the compiler to write its .o and .ppu files 
# somewhere else so not to compete with an existing Lazarus, but both this script 
# and Lazarus is quite happy to write new ones whenever needed. So, flush it clean. 

rm -Rf "lib/$TARGET"
rm -f tomboy-ng
rm -f "$PROJ"
mkdir -p "lib/$TARGET"

RUNIT="$COMPILER $OPT1 $UNITS $OPT2 $DEFS $PROJ.lpr"
echo "--------------- COMPILE COMMAND ------------------------"
echo "$RUNIT" 

TOMBOY_NG_VER="$VERSION" $RUNIT 1>tomboy-ng.log

if [ ! -e "$PROJ" ]; then
	echo "ERROR - COMPILE FAILED, please see source/tomboy-ng.log"
	exit 1
else
	cp "$PROJ" "tomboy-ng"
fi 
exit 0

