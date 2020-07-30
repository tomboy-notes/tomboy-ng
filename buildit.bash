#!/bin/bash
# copyright David Bannon, 2019, 2020, use as you see fit, but retain this statement.
#
# A script to build tomboy-ng from source without using the Lazarus GUI
#
# tomboy-ng depends directly on fpc, lazbuild, lcl and kcontrols.

# * Here we expect to find find FPC (ideally 3.2.0) preinstalled. Either on a path
#   indicated by the file ../WHICHFPC or in the (roor space) PATH.  A path to a 
#   user space fpc is not passed through the SRC Deb tool chain !
# * Lazarus, first tries ../WHICHLAZ, next root space PATH. Thats likely to be a 
#   problem because sensible Lazarus installs are, IMHO in user space.
# * KControls is bundled into the deb source kit by prepare.bash, its not 
#   present in the github zip file where tomboy-ng calls home. If building
#   from a SRC Deb kit, kcontrols is already in the 'orig' tarball.    

# While really intended to be part of a tool chain to build a Debian Source
# package, its useful as a standalone build tool if all you want is the binary.
# 

# This script runs in the upper tomboy-ng source tree (level with, eg Makefile).

# If it finds files, WHICHFPC and WHICHLAZ in directory above, it uses them to
# find its FPC Compiler and Lazarus things like lazbuild and the LCL directory.
# Failing that, it tries to find above using the PATH.

# Note that debuild does not pass existing path down here, it allows only a 
# simplified root space one. So, fpc and lazarus installed in user space will
# not work (in SRC DEB mode) unless you provide the WHICHFPC and WHICHLAZ 
# paths to fpc and lazbuild respectivly.

# The prepare.bash script will take a github zip file, unpack it, add kcontrols
# and create the necessary ../WHICH* files if it can.  An optional script, 
# getlaz.bash will build a minimal local lazarus and create its WHICHLAZ.



# This is where I keep tarballs and zips to avoid repeated large downloads.

MYREPO="$HOME/Documents/Kits"		# set an alterantive with -r
LAZ_VER="trunk"				# an alternative is lazarus-2.0.10-2
LAZ_INT_NAME="blar"

CPU="x86_64"				# default x86_64, can be arm
OS="linux"
PROJ=Tomboy_NG             # the formal name of the project, it's in project file.
START_DIR=$PWD
SOURCE_DIR="$PWD/tomboy-ng"      	
TARGET="$CPU-$OS"
K_DIR="$PWD/kcontrols/packages/kcontrols"
WIDGET="gtk2"				# untested with "qt5"
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
    echo "-c   specify CPU, default is x86_64, also supported arm"
    echo "When used in SRC DEB toolchain, set -c (if necessary) options in the Makefile."
    echo ""
    exit
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
		exit 1
	fi 
}

# ------------ It all starts here ---------------------


while getopts "hc:" opt; do
  case $opt in
    h)
      ShowHelp
      ;;
    c)
	CPU="$OPTARG"
	TARGET="$CPU-$OS"
	;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      ShowHelp
      ;;
  esac
done

CheckFPC
CheckLazBuild

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

cd "$K_DIR"		# WARNING, kcontrols is not part of the github zip file, its added by prepare.bash

LAZBUILD="$LAZ_DIR/lazbuild  -qq --pcp="$TEMPCONFDIR" --cpu=$CPU --widgetset=$WIDGET --lazarusdir=$LAZ_DIR kcontrolslaz.lpk"
echo "Laz build command is $LAZBUILD"
$LAZBUILD 1> KControls.log
rm -Rf "$TEMPCONFDIR"
if [ ! -e "$K_DIR/lib/$CPU-$OS/kmemo.o" ]; then
	echo "ERROR failed to build KControls, exiting..."
	K_DIR=""
	exit
fi
cd "$START_DIR"

VERSION=`cat "package/version"`

COMPILER="fpc"

echo "------------------------------------------------------"
echo "OK, we seem to have both Lazarus LCL and KControls available : "
echo "K_DIR = $K_DIR"
echo "Lazarus   = $LAZ_DIR"
echo "Compiler  = $COMPILER"
echo "PATH      = $PATH"
echo "CPU type = $CPU"
echo "Exclude Compiler Messages = $EXCLUDEMESSAGE"
echo "tomboy-ng version = $VERSION"
echo "-------------------------------------------------------"

# Test to see if we find the tomboy-ng source. 
if [ ! -e "$SOURCE_DIR/editbox.pas" ]; then
	echo "----------------------------------------------------------------"
	echo "Looked for [$SOURCE_DIR/editbox.pas]"
	echo "Not finding tomboy-ng source, exiting ...."
	echo " "
	ShowHelp
fi

echo "In buildit.bash, ready to start building tomboy" >> "$HOME"/build.log

# exit

cd $SOURCE_DIR
rm tomboy-ng

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

OPTS2=" -dLCL -dLCL$WIDGET" 
DEFS="-dDisableLCLGIF -dDisableLCLJPEG -dDisableLCLPNM -dDisableLCLTIFF"
# reported to reduce binary size but made no difference for me
# http://wiki.lazarus.freepascal.org/Lazarus_2.0.0_release_notes#Compiler_defines_to_exclude_some_graphics_support


# We must force a clean compile, no make looking after us here.
# I have not found a way of telling the compiler to write its .o and .ppu files
# somewhere else so not to compete with Lazarus, but both this script and Lazarus
# is quite happy to write new ones whenever needed. So, flush it clean. 
if [ -d "lib/$TARGET" ]; then
    rm -Rf "lib/$TARGET"
fi
mkdir -p "lib/$TARGET"

if [ -f "$PROJ" ]; then
    rm "$PROJ"
fi

echo "----- Building tomboy-ng in $PWD -------"

# echo "OPTS2 - $OPTS2"

RUNIT="$COMPILER $OPT1 $UNITS $OPT2 $DEFS $PROJ.lpr"
echo "--------------- COMPILE COMMAND ------------------------"
echo "$RUNIT"
echo "--------------------------------------------------------"

TOMBOY_NG_VER="$VERSION" $RUNIT

if [ ! -e "$PROJ" ]; then
	echo "ERROR - COMPILE FAILED"
else
	cp "$PROJ" "tomboy-ng"
	#cp "$PROJ" "$START_DIR/tomboy-ng$CPU"
	cd "$START_DIR"
	echo "OK, lets see how we got on "
	ls -l
fi 

