#!/bin/bash
# copyright David Bannon, 2019, 2020, no license, use as you see fit.
#
# -------------------------------------------------------------
# Do not run this script in the directory you found it in, it needs to be
# moved to a otherwise empty directory, read the full header before using.
# -------------------------------------------------------------
#
# A script to build tomboy-ng from source without using the Lazarus GUI
# You still need to get Lazarus LCL and various other parts of its package
# and the easy (only?) way to get that is to download lazarus source but
# you only need to build selected parts of it and don't need to activate 
# the GUI. As a prerequisite (for tomboy-ng) you need svn, fpc, lazarus source
# and kcontrols source. You might, for example -
#
# mkdir Pascal
# cd Pascal
# svn checkout https://svn.freepascal.org/svn/lazarus/branches/fixes_2_0     # almost 400meg !
# wget https://github.com/kryslt/KControls/archive/master.zip ; mv master.zip kcontrols.zip ; unzip kcontrols.zip
# wget https://github.com/tomboy-notes/tomboy-ng/archive/master.zip ; mv master.zip tomboy-ng.zip ; unzip tomboy-ng.zip
#
# And you are ready to go. Or you may wish to point the 'LAZ_FULL_DIR' to an existing
# Lazarus install. It must find the lazbuild binary and Lazarus' LCL.
#

LAZ_VER="trunk"		# an alternative "branches/fixes_2"
CPU="x86_64"
OS="linux"
PROJ=Tomboy_NG             # the formal name of the project, it's in project file.
CURRENT_DIR=$PWD
SOURCE_DIR="$PWD/tomboy-ng-master"
COMPILE_DIR="$SOURCE_DIR/tomboy-ng"

TARGET="$CPU-$OS"
#LAZ_FULL_DIR="$PWD/fixes_2_0"	# point to pre-existing copy if you like but don't rebuild it !!!!!! 
				# it should have KControls preinstalled so will know where to look.
LAZ_FULL_DIR="$HOME/bin/Lazarus/trunk"
LAZ_FULL_DIR="$PWD/Pascal/$LAZ_VER"

K_DIR="$PWD/Pascal/KControls-master/packages/kcontrols"
WIDGET="gtk2"
COMPILER="/usr/bin/fpc"

TEMPCONFDIR=`mktemp -d`
# lazbuild writes, or worse might read a default .lazarus config file. We'll distract it later.
GETLAZARUS=NO
GETFPC=NO
GETKCONTROLS=NO
GETTOMBOYNG=NO
REFRESHTOMBOYNG=NO
EXITNOW=NO

function ShowHelp () {
    echo " "
    echo "Assumes FPC of some sort available and working."
    echo "Will look for LCL and KControls where left from previous run unless set below" 
    echo "-h   print help message"
    echo "-L   get (and compile) Lazarus, big download !"
#    echo "-F   get FPC - not implemented"
    echo "-K   get (and compile) KControls"
    echo "-T   download fresh tomboy-ng source"
    echo "-R   refresh existing tomboy-ng source, dont use with -T"
    echo " "
    EXITNOW=YES 
}


while getopts ":hKFLTR" opt; do
  case $opt in
    h)
      ShowHelp
      ;;
    L)
      echo "Get Lazarus" >&2
      GETLAZARUS=YES
      ;;
    F)
      echo "Get FPC" >&2;
      GETFPC=YES;
      echo "Do it yourself !"
      ;;
    K)
      echo "Get KControls" >&2
      GETKCONTROLS=YES
      ;;
    T)					# must check these are sane, not both and zip available
      GETTOMBOYNG=YES
      ;;
    R)
      REFRESHTOMBOYNG=YES
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      ShowHelp
      ;;
  esac
done

if [ "$EXITNOW" = "YES" ]; then exit; fi;

echo "done with options"

if [ ! -d "Pascal" ]; then
	mkdir -p "Pascal"
fi

# We assume we have FPC at this stage.
if [ "$GETLAZARUS" = "YES" ]; then
	echo "OK, we will get Lazarus, TBD";
	cd Pascal
	rm -Rf trunk
# --------------------- TEST MODE don't download --------------
	# svn checkout "https://svn.freepascal.org/svn/lazarus/$LAZ_VER"
	# zip -r trunk.zip trunk
	unzip trunk.zip
	cd "$LAZ_VER"
	make lazbuild 1> Lazbuild_log.txt
	if [ ! -e lazbuild ]; then
		echo "ERROR failed to build lazbuild, exiting ....."
		LAZ_FULL_DIR=""
		exit
	fi
	make lcl 1> LCL_log.txt
 	LAZ_FULL_DIR=$PWD			# ToDo : must test that somehow......
	cd "$CURRENT_DIR"
else
	echo "Hoping to find lazbuild pre built"
	if [ -a "$LAZ_FULL_DIR/lazbuild" ]; then 
		echo "We have lazbuild"
	else
		echo "Do not have a lazbuild in "$LAZ_FULL_DIR", must exit"
		exit
	fi
fi


# We assume by here we have both FPC and Lazbuild
if [ "$GETKCONTROLS" = "YES" ]
then
	echo "OK, we will get KControls"
	cd Pascal
# --------------------------------------- TESTING MODE, don't download -----------
	//wget https://github.com/kryslt/KControls/archive/master.zip
	//mv master.zip kcontrols.zip

	rm -Rf KControls-master
	unzip kcontrols.zip	
	cd KControls-master/packages/kcontrols
	"$LAZ_FULL_DIR/lazbuild"
    	LAZBUILD="$LAZ_FULL_DIR/lazbuild  -qq --pcp="$TEMPCONFDIR" --cpu=$CPU --widgetset=$WIDGET --lazarusdir=$LAZ_FULL_DIR kcontrolslaz.lpk"
    	echo "Laz build command is $LAZBUILD"
    	$LAZBUILD
    	rm -Rf "$TEMPCONFDIR"
	if [ ! -d "lib" ]; then
		echo "ERROR failed to build lazbuild, exiting..."
		K_DIR=""
		exit
	else
		K_DIR="$PWD"
	fi
	cd "$CURRENT_DIR"
fi




echo "OK, we seem to have both Lazarus LCL and KControls available : "
echo "K_DIR = $K_DIR"
echo "Laz   = $LAZ_FULL_DIR"

if [ ! -e "$K_DIR/lib/$CPU-$OS/kmemo.o" ]; then
	echo "Looked for [$K_DIR/lib/$CPU-$OS/kmemo.o]"
	echo "Nope, not finding kmemo, must exit"
	exit
fi



if [ "$GETTOMBOYNG" = "YES" ]; then
	rm -Rf "$SOURCE_DIR"
	wget https://github.com/tomboy-notes/tomboy-ng/archive/master.zip
	mv master.zip tomboy-ng.zip
	unzip tomboy-ng.zip	
fi

echo "Refresh tomboy-ng source = $REFRESHTOMBOYNG"

if [ "$REFRESHTOMBOYNG" = "YES" ]; then
	echo "Will Refresh existing tomboy-ng source"
	rm -Rf "$SOURCE_DIR"	
	unzip tomboy-ng.zip
fi		

VERSION=`cat "$SOURCE_DIR/package/version"`

# exit


cd $COMPILE_DIR

# DEBUG options -O1,   (!) -CX, -g, -gl, -vewnhibq

OPT1="-MObjFPC -Scghi -CX -Cg -O3 -XX -Xs -l -vewnibq -Fi$COMPILE_DIR/lib/$TARGET"

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

UNITS="$UNITS -Fu$COMPILE_DIR/"
UNITS="$UNITS -FU$COMPILE_DIR/lib/$TARGET/" 

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

echo "------------ Building tomboy-ng in $PWD ----------------"

echo "OPTS2 - $OPTS2"

RUNIT="$COMPILER $OPT1 $UNITS $OPT2 $DEFS $PROJ.lpr"
TOMBOY_NG_VER="$VERSION" $RUNIT
echo "OK, lets see how we got on "
ls -l "$PROJ"

