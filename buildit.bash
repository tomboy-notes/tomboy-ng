#!/bin/bash
# copyright David Bannon, 2019, 2020, use as you see fit, but retain this statement.
#
# A script to build tomboy-ng from source without using the Lazarus GUI
# It will download Lazarus (~50Meg) and KControls unless it finds them in
# its nominated local repo (see -h).

# It expects to find FPC (ideally 3.2.0) preinstalled.

# While really intended to be part of a tool chain to build a Debian Source
# package, its possibly useful as a standalone build tool.

# You still need to get Lazarus LCL and various other parts of its package
# and the easy (only?) way to get that is to download lazarus source but
# you only need to build selected parts of it and don't need to activate 
# the GUI. As a prerequisite (for tomboy-ng) you need fpc (binutils, 
# buildessencials) lazarus source (gtk2) and kcontrols source. 
#
# This is found and should be run, script will run in a dir called either 
# tomboy-ng or tomboy-ng-master (from github) below it is a dir tomboy-ng 
# that contains the *.pas and *.lfm files. Level with it are the po, docs, 
# package etc directories 


# This is where I keep tarballs and zips to avoid repeated large downloads.

MYREPO="$HOME/Documents/Kits"		# set an alterantive with -r
LAZ_VER="trunk"				# an alternative is lazarus-2.0.10-2
LAZ_INT_NAME="blar"

CPU="x86_64"				# default x86_64, can be arm
OS="linux"
PROJ=Tomboy_NG             # the formal name of the project, it's in project file.

CURRENT_DIR=$PWD

SOURCE_DIR="$PWD/tomboy-ng"      	

TARGET="$CPU-$OS"

LAZ_FULL_DIR="$PWD/Pascal/$LAZ_VER"

K_DIR="$PWD/Pascal/kcontrols/packages/kcontrols"

WIDGET="gtk2"				# untested with "qt5"
COMPILER="fpc"   			# ========  F I X  M E 


TEMPCONFDIR=`mktemp -d`
# lazbuild writes, or worse might read a default .lazarus config file. We'll distract it later.

AUTODOWNLOAD=FALSE			# downloading large file, use -d to allow it

function ShowHelp () {
    echo " "
    echo "Assumes FPC of some sort available and working."
    echo "Will look for Lazarus and KControls kits in repo," 
    echo "David Bannon, July 2020" 
    echo "-h   print help message"
    echo "-c   specify CPU, default is x86_64, also supported arm"
    echo "-d   downloading large files as needed"
    echo "-r   repo where large files might be or can be put $MYREPO"
    echo "-L   A lable for Lazaru version, trunk, lazarus-2.0.10-2"  
    echo " "
    exit
}

function BuildLaz () {    # builds only  lazbuild and LCL
	echo "--- Building lazbuild in $LAZ_FULL_DIR ---"
	cd "$LAZ_FULL_DIR"
	make -j1 lazbuild 1> Lazbuild_log.txt
	#env PATH="$FPCPATH":"$PATH" make -j1 lazbuild 1> Lazbuild_log.txt
	if [ ! -e lazbuild ]; then
		echo "========================================================="
		echo "ERROR failed to build lazbuild, exiting ....."
		echo "which fpc = "
		which fpc
		echo "pwd ="
		pwd
		LAZ_FULL_DIR=""
		exit
	fi
	echo "--- Building LCL ---"
	#env PATH="$FPCPATH":"$PATH" make -j1 lcl 1> LCL_log.txt
	make -j1 lcl 1> LCL_log.txt
 	LAZ_FULL_DIR=$PWD			# ToDo : must test that somehow......  
}

function NeededFiles () {
	if [ ! -d "$MYREPO/Lazarus" ]; then
		mkdir -p "$MYREPO/Lazarus"
	fi
	if [ ! -f "$MYREPO/Lazarus/$LAZ_VER.zip" ]; then
		if [ "$AUTODOWNLOAD" = "FALSE" ]; then
			echo "Auto download not turned on, need $LAZ_VER"
			echo "looked in $MYREPO/Lazarus/$LAZ_VER.zip"
			ShowHelp
		fi
	fi
	case "$LAZ_VER" in
		"trunk")
			if [ ! -f "$MYREPO/Lazarus/$LAZ_VER.zip" ]; then
				wget https://github.com/graemeg/lazarus/archive/upstream.zip 
				mv upstream.zip "$MYREPO/Lazarus/trunk.zip"
			fi
			LAZ_INT_NAME="lazarus-upstream"
			;;
		"lazarus-2.0.10-2")
			if [ ! -f "$MYREPO/Lazarus/$LAZ_VER.zip" ]; then
				wget "https://sourceforge.net/projects/lazarus/files/Lazarus%20Zip%20_%20GZip/Lazarus%202.0.10/$LAZ_VER.zip"
				mv "$LAZ_VER.zip" "$MYREPO/Lazarus/$LAZ_VER.zip"
			fi
			LAZ_INT_NAME="lazarus"
			;;
		*)
			echo "Sorry, I dont know how to get $LAZ_VER, exiting"
			exit
			;;
	esac
	if [ ! -f "$MYREPO/Lazarus/kcontrols.zip" ]; then
		wget https://github.com/kryslt/KControls/archive/master.zip
		mv master.zip "$MYREPO/Lazarus/kcontrols.zip"
	fi
	#  MUST ADD FPC to here ....
}

# ------------ It all starts here ---------------------

COMPILER=`which xxx`
if [ "$FPCCOMPILER" = "" ]; then
	# If it the toolchain, and FPC is installed in user space, debuild kindly
	# hides its path from us. The prepare.bash script may have left us a hint.
	NEWPATH=`cat ../WHICHFPC | rev | cut -c -4 --complement | rev`
else 
	# Looks like we are running this script "by hand".
	NEWPATH=`echo "$FPCCOMPILER" | rev | cut -c -4 --complement | rev`
fi

if [ "$NEWPATH" = "" ]; then
	echo "Cannot find a free pascal compiler to use"
	exit;
fi

echo "---- Add  $NEWPATH to existing PATH ----"
PATH="$NEWPATH":"$PATH"
export PATH
NEWPATH=""
FPCCOMPILER=""


while getopts ":hdc:L:" opt; do
  case $opt in
    h)
      ShowHelp
      ;;
    c)
	CPU="$OPTARG"
	TARGET="$CPU-$OS"
	;;
    d)
	AUTODOWNLOAD="TRUE"
	;;
    r)
	MYREPO="$OPTARG"
	;;
    L)
	LAZ_VER="$OPTARG"
	;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      ShowHelp
      ;;
  esac
done

echo "Starting buildit.bash" >> "$HOME"/build.log

NeededFiles		# will download the needed files to a cache dir.

rm -Rf Pascal
mkdir -p Pascal

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

# We can assume we have FPC at this stage, lets try for Lazarus.

echo "--- Installing Lazarus ---" >> $HOME/build.log
echo "--- Installing Lazarus ---"
cd Pascal
unzip -q "$MYREPO/Lazarus/$LAZ_VER.zip"
LAZ_FULL_DIR="$PWD/$LAZ_INT_NAME"
cd "$LAZ_FULL_DIR"
BuildLaz			
cd "$CURRENT_DIR"
if [ -a "$LAZ_FULL_DIR/lazbuild" ]; then 
	echo "We have lazbuild"
else
	echo "We do not have $LAZ_FULL_DIR/lazbuild - thats sad."
	exit
fi
echo "--- Installing KControls ---" >> $HOME/build.log
# We can assume by here we have both FPC and Lazbuild
cd "$CURRENT_DIR"
echo "--- Installing KControls ---"
cd Pascal
unzip -q "$MYREPO/Lazarus/kcontrols.zip"
mv KControls-master kcontrols
cd "$K_DIR"

LAZBUILD="$LAZ_FULL_DIR/lazbuild  -qq --pcp="$TEMPCONFDIR" --cpu=$CPU --widgetset=$WIDGET --lazarusdir=$LAZ_FULL_DIR kcontrolslaz.lpk"
echo "Laz build command is $LAZBUILD"
$LAZBUILD 2> KControls.log
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

echo "In buildit.bash, ready to start building tomboy" >> "$HOME"/build.log

# exit

rm tomboy-ng
cd $SOURCE_DIR

# DEBUG options -O1,   (!) -CX, -g, -gl, -vewnhibq

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
	cp "$PROJ" "tomboy-ng"
	#cp "$PROJ" "$CURRENT_DIR/tomboy-ng$CPU"
	cd "$CURRENT_DIR"
	echo "OK, lets see how we got on "
	ls -l
fi 

