#!/bin/bash

# Script to build and zip up the Misty Severs. Passed a viable Lazarus install
# on command line. Creates zip files named like Lazarus Build Modes.

LAZ_FULL_DIR="$1"
BUILDOPTS=
PRODUCT="tomboy-ng"
VERSION=`cat version`
BUILDOPTS=" -B --quiet --quiet"


function ConfigString () {          # passed a path to a Lazarus install, rets ""--pcp=/path/to/config""
	# ----- OK, lets find Laz Config -----
	LAZ_CONFIG=""
	if [ -d "$1" ]; then
		if [ -f "$1"/lazarus.cfg ]; then
			# Assume if we have a cfg, it specifies pcp ?? Will fail otherwise
			LAZ_CONFIG=`grep -i pcp "$1"/lazarus.cfg`
		else
			echo "----- ERROR, dont have a $1/lazarus.cfg file -----"
			exit
		fi
	fi
	if [ -z "$LAZ_CONFIG" ]; then
		echo "----- ERROR, dont have a Laz Config mentioned in $1/lazarus.cfg ----"
		exit
	fi
    echo "$LAZ_CONFIG"			# note LAZ_CONFIG is not global
}

function ModeParamBin () { # expects to be called like   BIN=$(ModeParam ReleaseWin64)
    case $1 in
    	Default)
    		echo "$PRODUCT"
    	;;
        MistyReleaseX86_64)
			echo "misty-server-x86_64"
		;;
		MistyReleaseRasPi32)
			echo "misty-server-arm32"
		;;
		MistyReleaseRasPi64)
			echo "misty-server-arm64"
		;;
		MistyReleaseWin64)
			echo "misty-server.exe"
		;;
    esac
}

function MakeOneMisty () {      # Passed the Lazarus Build Mode name
    LAZ_CONFIG=$(ConfigString "$LAZ_FULL_DIR")
    BIN=$(ModeParamBin "$1")
    if [ "$BIN" == "" ]; then
        echo "ERROR - failed to get a binary name for $1"
        exit
    fi
    cd ../experimental/Misty-Small
    if [ -e "$BIN" ]; then
        rm "$BIN"
    fi
    $LAZ_FULL_DIR/lazbuild $BUILDOPTS $LAZ_CONFIG --build-mode="$1" webserver.lpi
    if [ -f "$BIN" ]; then
        echo "----- Build of $BIN successful."
    else
        echo "----- Failed to build Misty binary, $BIN"
        echo "$LAZ_FULL_DIR/lazbuild $BUILDOPTS $LAZ_CONFIG --build-mode=$1 webserver.lpi"
        ERROR=" $ERROR Failed to build $BIN "
    fi
    cd ../../package
    if [ -e "$1".zip ]; then
        rm "$1".zip
    fi
    rm -f misty-server
    if [ "$BIN" == "misty-server.exe" ]; then             # Windows binary retains its name
        zip -j  "$1".zip ../experimental/Misty-Small/"$BIN"  ../doc/misty-readme.note
    else
        cp ../experimental/Misty-Small/"$BIN" misty-server
        zip -j  "$1".zip misty-server ../doc/misty-readme.note
    fi
    ls -l "$1".zip
}


for MMODE in MistyReleaseX86_64 MistyReleaseRasPi32 MistyReleaseRasPi64 MistyReleaseWin64 ; do
	echo "Building $MMODE"
	rm -f "$MMODE".zip
	MakeOneMisty "$MMODE"
done
