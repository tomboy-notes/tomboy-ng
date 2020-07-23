#!/bin/bash
# copyright David Bannon, 2019, 2020, use as you see fit, but retain this statement.
#
# -------------------------------------------------------------
# Do not run this script, its under development.
# -------------------------------------------------------------
#
# A script to build tomboy-ng from source without using the Lazarus GUI
# You still need to get Lazarus LCL and various other parts of its package
# and the easy (only?) way to get that is to download lazarus source but
# you only need to build selected parts of it and don't need to activate 
# the GUI. As a prerequisite (for tomboy-ng) you need svn, fpc, lazarus source
# and kcontrols source. 
#
# And you are ready to go. Or you may wish to point the 'LAZ_FULL_DIR' to an existing
# Lazarus install. It must find the lazbuild binary and Lazarus' LCL.

# This script will run in a dir called either tomboy-ng or tomboy-ng-master (from github)
# below it is a dir tomboy-ng that contains the *.pas and *.lfm files.
# level with it are the po, docs, package etc directories 
#

# This is where I keep tarballs and zips to avoid repeated large downloads.
MYREPO='/home/dbannon/Documents/Kits/"
LAZ_VER="trunk"		# an alternative is "branches/fixes_2"  CODE BELOW ASSUMES TRUNK   !!!!!
CPU="x86_64"
OS="linux"
PROJ=Tomboy_NG             # the formal name of the project, it's in project file.

CURRENT_DIR=$PWD

SOURCE_DIR="$PWD/tomboy-ng"      # we will have to rename tomboy-ng-master"

TARGET="$CPU-$OS"

LAZ_FULL_DIR="$PWD/Pascal/$LAZ_VER"

K_DIR="$PWD/Pascal/kontrols/packages/kcontrols"

WIDGET="gtk2"
COMPILER="fpc"			# set an explicite path if you prefer, this needs to be on path

TEMPCONFDIR=`mktemp -d`
# lazbuild writes, or worse might read a default .lazarus config file. We'll distract it later.
GETLAZARUS=NO
GETFPC=NO
GETKCONTROLS=NO
GETTOMBOYNG=NO
REFRESHTOMBOYNG=NO

function ShowHelp () {
    echo " "
    echo "Assumes FPC of some sort available and working."
    echo "Will look for Lazarus and KControls kits in repo," 
    echo "David Bannon, July 2020" 
    echo "-h   print help message"
    echo "-c   specify CPU, default is x86_64, also supported arm"
    echo " "
    exit
}

function BuildLaz () {    # builds only  lazbuild and LCL
	echo "--- Building lazbuild and lcl ---"
	cd "$LAZ_FULL_DIR"
	make lazbuild 1> Lazbuild_log.txt
	if [ ! -e lazbuild ]; then
		echo "ERROR failed to build lazbuild, exiting ....."
		LAZ_FULL_DIR=""
		exit
	fi
	make lcl 1> LCL_log.txt
 	LAZ_FULL_DIR=$PWD			# ToDo : must test that somehow......  
}

function NeededFiles () {
	if [ ! -d "$MYREPO/Lazarus" ]; then
		mkdir -p "$MYREPO/Lazarus"
	fi
	if [ ! -f "$MYREPO/Lazarus/upstream.zip" ]; then
		wget https://github.com/graemeg/lazarus/archive/upstream.zip 
		mv upstream.zip "$MYREPO/Lazarus/upstream.zip"
	fi
	if [ ! -f "$MYREPO/Lazarus/kcontrols.zip" ]; then
		wget https://github.com/kryslt/KControls/archive/master.zip
		mv master.zip "$MYREPO/Lazarus/kcontrols.zip"
	fi
	#  MUST ADD FPC to here ....
}





while getopts ":hc:" opt; do
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


NeededFiles		# will download the needed files to a cache dir.

if [ ! -d "Pascal" ]; then
	mkdir -p "Pascal"
fi

# OK, do we have a good FPC available ?
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
		exit
	;;
esac

# 6058 - note about things not being inlined
# 5027 - var not used
# 2005 - level 2 comment


# We can assume we have FPC at this stage.


	unzip "$MYREPO/Lazarus/upstream.zip";
	mv lazarus-upstream trunk
	LAZ_FULL_DIR="$PWD/trunk"
	cd "$LAZ_FULL_DIR"
	BuildLaz			# always build if its fresh
	cd "$CURRENT_DIR"

	if [ -a "$LAZ_FULL_DIR/lazbuild" ]; then 
		echo "We have lazbuild"
	else
		echo "We do not have $LAZ_FULL_DIR/lazbuild - thats sad."
		exit
	fi



# We can assume by here we have both FPC and Lazbuild
echo "OK, we will get and build KControls"
cd Pascal
mv -f kcontrols.zip kcontrols.zip-old
wget https://github.com/kryslt/KControls/archive/master.zip
mv master.zip kcontrols.zip
rm -Rf KControls-master
unzip "$MYREPO/kcontrols.zip"
mv kcontrols-master kcontrols
cd "$K_DIR"
LAZBUILD="$LAZ_FULL_DIR/lazbuild  -qq --pcp="$TEMPCONFDIR" --cpu=$CPU --widgetset=$WIDGET --lazarusdir=$LAZ_FULL_DIR kcontrolslaz.lpk"
echo "Laz build command is $LAZBUILD"
$LAZBUILD
rm -Rf "$TEMPCONFDIR"
if [ ! -e "$K_DIR/lib/$CPU-$OS/kmemo.o" ]; then
	echo "ERROR failed to build KControls, exiting..."
	K_DIR=""
	exit
fi
cd "$CURRENT_DIR"

VERSION=`cat "package/version"`

echo "------------------------------------------------------"
echo "OK, we seem to have both Lazarus LCL and KControls available : "
echo "K_DIR = $K_DIR"
echo "Lazarus   = $LAZ_FULL_DIR"
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

exit










rm tomboy-ng
cd $SOURCE_DIR

# DEBUG options -O1,   (!) -CX, -g, -gl, -vewnhibq

# OPT1="-MObjFPC -Scghi -CX -Cg -O3 -XX -Xs -l -vewnibq -vm6058,2005,5027 -Fi$SOURCE_DIR/lib/$TARGET"
OPT1="-MObjFPC -Scghi -CX -Cg -O3 -XX -Xs -l -vewnibq $EXCLUDEMESSAGE -Fi$SOURCE_DIR/lib/$TARGET"

UNITS="$UNITS -Fu$K_DIR/lib/$TARGET"
UNITS="$UNITS -Fu$LAZ_FULL_DIR/components/tdbf/lib/$TARGET/$WIDGET"
UNITS="$UNITS -Fu$LAZ_FULL_DIR/components/printers/lib/$TARGET/$WIDGET"
UNITS="$UNITS -Fu$LAZ_FULL_DIR/components/cairocanvas/lib/$TARGET/$WIDGET"
UNITS="$UNITS -Fu$LAZ_FULL_DIR/components/lazcontrols/lib/$TARGET/$WIDGET"
UNITS="$UNITS -Fu$LAZ_FULL_DIR/components/lazutils/lib/$TARGET"
UNITS="$UNITS -Fu$LAZ_FULL_DIR/components/ideintf/units/$TARGET/$WIDGET"
UNITS="$UNITS -Fu$LAZ_FULL_DIR/lcl/units/$TARGET/$WIDGET"
UNITS="$UNITS -Fu$LAZ_FULL_DIR/lcl/units/$TARGET"
UNITS="$UNITS -Fu$LAZ_FULL_DIR/packager/units/$TARGET"

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
	cp "$PROJ" "$CURRENT_DIR"/tomboy-ng
	cd "$CURRENT_DIR"
	echo "OK, lets see how we got on "
	ls -l
fi 

