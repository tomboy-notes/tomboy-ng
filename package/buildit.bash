#!/bin/bash
# copyright David Bannon, 2019, 2020, use as you see fit, but retain this statement.
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
# and kcontrols source. 
#
# And you are ready to go. Or you may wish to point the 'LAZ_FULL_DIR' to an existing
# Lazarus install. It must find the lazbuild binary and Lazarus' LCL.
#

LAZ_VER="trunk"		# an alternative is "branches/fixes_2"
CPU="x86_64"
OS="linux"
PROJ=Tomboy_NG             # the formal name of the project, it's in project file.
CURRENT_DIR=$PWD
SOURCE_DIR="$PWD/tomboy-ng-master"
COMPILE_DIR="$SOURCE_DIR/source"

TARGET="$CPU-$OS"
#LAZ_FULL_DIR="$PWD/fixes_2_0"	# point to pre-existing copy if you like but don't rebuild it !!!!!! 
				# it should have KControls preinstalled so will know where to look.
LAZ_FULL_DIR="$HOME/bin/Lazarus/trunk"
LAZ_FULL_DIR="$PWD/Pascal/$LAZ_VER"

K_DIR="$PWD/Pascal/KControls-master/packages/kcontrols"
WIDGET="gtk2"
COMPILER="fpc"			# set an explicite path if you prefer.

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
    echo "Will look for LCL, KControls and tomboy-ng source where left from previous"
    echo "run unless set below. Use an Existing install of Lazarus if available."
    echo "David Bannon, May 2020" 
    echo "-h   print help message"
    echo "-L   get (and compile) Lazarus, big download, ~400Meg!"
#    echo "-F   get FPC - not implemented"
    echo "-K   get (and compile) KControls"
    echo "-T   download fresh tomboy-ng source"
    echo "-R   refresh existing tomboy-ng source, dont use with or before -T"
    echo "-e   dir of existing Lazarus install, eg where we find lazbuild"
    echo "-E   dir of Existing Lazarus install, build the necessary files"
    echo "-c   specify CPU, default is x86_64, also supported arm"
    echo " "
    echo "For example, you have downloaded Lazarus src, this is its first run, you have"
    echo "the tomboy-ng source tree in this dir but need KControls, on a Raspberry Pi :"
    echo "bash ./buildit.bash -E /home/dbannon/bin/Lazarus/trunk -carm -K"
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

while getopts ":hKFLTRE:e:c:" opt; do
  case $opt in
    h)
      ShowHelp
      ;;
    c)
	CPU="$OPTARG"
	TARGET="$CPU-$OS"
	;;
    L)
      echo "Get Lazarus" >&2
      GETLAZARUS=YES
      ;;
    F)"$OPTARG"
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
      if [ "$GETTOMBOYNG" = "YES" ]; then
	echo "You cannot use both -T and -R"
	ShowHelp
      fi
      if [ ! -e "tomboy-ng.zip" ]; then
	echo "no existing tomboy-ng.zip to refresh from, choose -T instead"
	ShowHelp
      fi
      REFRESHTOMBOYNG=YES
      ;;
    e)
	if [ "$GETLAZARUS" = "YES" ]; then
		echo "You cannot use both -L and -e"
		ShowHelp
	fi
	LAZ_FULL_DIR="$OPTARG"
        ;;
    E)
	if [ "$GETLAZARUS" = "YES" ]; then
		echo "You cannot use both -L and -E"
		ShowHelp
	fi
	LAZ_FULL_DIR="$OPTARG"
	BUILDLAZ="YES"
        ;;

    \?)
      echo "Invalid option: -$OPTARG" >&2
      ShowHelp
      ;;
  esac
done


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
	BuildLaz			# always build if its fresh
	cd "$CURRENT_DIR"
else
	echo "Hoping to existing Lazarus files."
	if [ "$BUILDLAZ" = "YES" ]; then
	    BuildLaz
	fi
	cd "$CURRENT_DIR"
	if [ -a "$LAZ_FULL_DIR/lazbuild" ]; then 
		echo "We have lazbuild"
	else
		echo "Maybe you need to use -L to get Lazarus (~400Meg) or"
		echo "use -E to point to an existing Lazarus install."
		echo "Do not have a lazbuild in "$LAZ_FULL_DIR", must exit"
		ShowHelp
	fi
fi


# We assume by here we have both FPC and Lazbuild
if [ "$GETKCONTROLS" = "YES" ]; then
	echo "OK, we will get KControls"
	cd Pascal
	mv -f kcontrols.zip kcontrols.zip-old
	wget https://github.com/kryslt/KControls/archive/master.zip
	mv master.zip kcontrols.zip
	rm -Rf KControls-master
	unzip kcontrols.zip
	cd "$CURRENT_DIR"
else
	if [ ! -e Pascal/KControls-master/kmemo_readme.txt ]; then
		echo "Not finding a KControls install, maybe you need -K"
		echo "This script requires its own KControls install, exiting ...."
		ShowHelp
	fi
fi




# We always rebuild KControls, we don't know if user has changed Lazarus

cd Pascal/KControls-master/packages/kcontrols
# "$LAZ_FULL_DIR/lazbuild"
LAZBUILD="$LAZ_FULL_DIR/lazbuild  -qq --pcp="$TEMPCONFDIR" --cpu=$CPU --widgetset=$WIDGET --lazarusdir=$LAZ_FULL_DIR kcontrolslaz.lpk"
echo "Laz build command is $LAZBUILD"

$LAZBUILD
rm -Rf "$TEMPCONFDIR"
if [ ! -d "lib" ]; then
	echo "ERROR failed to build KControls, exiting..."
	K_DIR=""
	exit
else
	K_DIR="$PWD"
fi
cd "$CURRENT_DIR"

# Test to see if we find some evidence that we have a working KControls. 
if [ ! -e "$K_DIR/lib/$CPU-$OS/kmemo.o" ]; then
	echo "----------------------------------------------------------------"
	echo "Looked for [$K_DIR/lib/$CPU-$OS/kmemo.o]"
	echo "Not finding a functional KControls, exiting ...."
	echo " "
	exit
fi


echo "------------------------------------------------------"
echo "OK, we seem to have both Lazarus LCL and KControls available : "
echo "K_DIR = $K_DIR"
echo "Lazarus   = $LAZ_FULL_DIR"
echo "Refresh tomboy-ng source = $REFRESHTOMBOYNG"
echo "Download a new copy of tomboy-ng = $GETTOMBOYNG"
echo "CPU type = $CPU"
echo "Exclude Compiler Messages = $EXCLUDEMESSAGE"
echo "-------------------------------------------------------"

if [ "$GETTOMBOYNG" = "YES" ]; then
	rm -Rf "$SOURCE_DIR"
	wget https://github.com/tomboy-notes/tomboy-ng/archive/master.zip
	mv master.zip tomboy-ng.zip
	unzip tomboy-ng.zip	
fi


if [ "$REFRESHTOMBOYNG" = "YES" ]; then
	echo "Will Refresh existing tomboy-ng source"
	rm -Rf "$SOURCE_DIR"	
	unzip tomboy-ng.zip
fi


# Test to see if we find the tomboy-ng source. 
if [ ! -e "$COMPILE_DIR/editbox.pas" ]; then
	echo "----------------------------------------------------------------"
	echo "Looked for [$COMPILE_DIR/editbox.pas]"
	echo "Maybe you need -T to download src, or -R to refresh from a previous run"
	echo "Not finding tomboy-ng source, exiting ...."
	echo " "
	ShowHelp
fi

		

VERSION=`cat "$SOURCE_DIR/package/version"`

# exit

rm tomboy-ng
cd $COMPILE_DIR

# DEBUG options -O1,   (!) -CX, -g, -gl, -vewnhibq

# OPT1="-MObjFPC -Scghi -CX -Cg -O3 -XX -Xs -l -vewnibq -vm6058,2005,5027 -Fi$COMPILE_DIR/lib/$TARGET"
OPT1="-MObjFPC -Scghi -CX -Cg -O3 -XX -Xs -l -vewnibq $EXCLUDEMESSAGE -Fi$COMPILE_DIR/lib/$TARGET"

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

