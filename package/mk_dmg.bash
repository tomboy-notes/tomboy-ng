#!/bin/bash
# ------------------------------------------------------------
# A script to generate Mac's tomboy-ng dmg files
# Typical Usage : bash ./mk_dmg.bash $HOME/Desktop/Lazarus/lazarus_3_0
#                 bash ./mk_dmg.bash packageonly
  
# For mac at least, set project specific Compiler Path to $(CompPath)
# that will then use local Laz setting in Preferences->Env->Compiler Executable
# which, at present is set to fpcup....fpc.sh
# note that the config, as read by lazbuild, will remember that and
# ignore path once set. 
# Note we assume laz config is named same as lazarus dir 
#
# Depends (heavily) on https://github.com/andreyvit/create-dmg
# which must be installed.
#
#		 Probably should put license and readme in there too.
# -------------------------------------------------------------

# History 
# 2022-11-02  Made allowance for lazconfig not being in $HOME/.* We now assume
#             instead that ALL Laz installs have a lazarus.cfg with --pcp=
# 2023-11-08  Work in progress, added packageonly and path changes to get it
#             working on the Mac Mini (brew things now in path).
#             Not tested doing a compile first, does make the Universal binary
#	      as long as the two individual binaries exist in ../source
#
# WARNING - as packageonly is not being told where Lazarus is, it cannot find
#	    Lazarus's premade language files.
# March 24  Extended to build the full universal binary
# 2024-04-27 Notes about getting compiler path right. Still have not built
#            rtl fcl for AARCH64 from bin/FPC/fpc.3.2.3 so use fpcupdeluxe
#
# Normally, if not going through a script like this, FPC path is added in .bashrc
# but remember, component path is overridden by hardwired paths in laz config

#FPC_PATH="$HOME"/bin/FPC/fpc-3.3.1/bin        # Thats the FPC built from main March 2024
#FPC_PATH="$HOME"/Pico2/fpc/bin/x86_64-linux   # Thats Michael's compiler, older version of main.
#FPC_PATH="$HOME"/bin/FPC/fpc-3.2.2/bin        # Thats the default, released FPC
#FPC_PATH="$HOME"/bin/FPC/fpc-3.2.3/bin        # Thats Fixes, last rebuild March 2024
FPC_PATH="$HOME"/fpcupdeluxe/fpc/bin/x86_64-darwin        # Thats Fixes from fpcupdeluxe, last rebuild March 2024

# If started from system menu, OLD_PATH is not set and fpc has not been added to path.
if [ "$OLD_PATH" == "" ]; then
    export PATH="$FPC_PATH":"$PATH"
else
    export PATH="$FPC_PATH":"$OLD_PATH"
fi
	      
LAZ_FULL_DIR="$1"
LAZ_DIR=`basename "$LAZ_FULL_DIR"`
PRODUCT=tomboy-ng
WORK=source_folder
CONTENTS="$WORK/""$PRODUCT".app/Contents
VERSION=`cat version`
MANUALS=`cat note-files`
MSGFMT="msgfmt"
#MSGFMT="/usr/local/Cellar/gettext/0.19.8.1/bin/msgfmt"
VERSION=`cat version`
PACKAGEONLY='false'

if [ "$1" == "packageonly" ]; then
	PACKAGEONLY='true';
else
    LAZ_PCP=`cat "$LAZ_FULL_DIR"/lazarus.cfg`

    if [ -z "$LAZ_DIR" ]; then
	echo "Usage : $0 /Full/Path/Lazarus/dir"
	echo "eg    : $0 \$HOME/bin/lazarus/fixes_2_0"
	echo "eg    : $0 \$HOME/Desktop/Lazarus/lazarus_3_0"
	exit
    fi

    if [ ! -f "$LAZ_FULL_DIR"/lazbuild ]; then
	echo "Sorry, ""$LAZ_FULL_DIR"" does not look like it contains a Lazarus build"
	exit
    fi
fi
	# We do some wildcard deletes further on, be safe !
if [ ! -f tomboy-ng.iss ]; then
	echo "Not running in tomboy-ng package dir, too dangerous"
	exit
fi


function MakeBinary () {     # Pass the Lazarus Mode we wish to build
    cd ../source
    BINARY=""
    case $1 in
        CocoaRelease)
            THECPU="x86_64"
	    BINARY="tomboy-ng-x86_64"
        ;;
        CocoaArmRelease)
            THECPU="aarch64"
            BINARY="tomboy-ng-aarch64"
        ;;
    esac
    if [ "$BINARY" == "" ]; then
        echo "============ ERROR bad Mode Name ( $1 ) passed to MakeBinary ======="
        exit
    fi
    echo "--------- Making $BINARY with mode $1 --------"
    TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild "$LAZ_PCP" -B --cpu="$THECPU" --build-mode="$1" Tomboy_NG.lpi
    if [ ! -f "$BINARY" ]; then
        echo "============= ERROR failed to make $BINARY =========="
        exit
    fi
    echo "------------ $BINARY ready sir ------------"
}

function MakeUniversal() {
    cd ../source
    lipo -create -output tomboy-ng tomboy-ng-x86_64 tomboy-ng-aarch64
    strip "$PRODUCT"
    if [ ! -f "$PRODUCT" ]; then
        echo "================= ERROR $PRODUCT is not present in source ========= "
        exit
    fi

    cd ../package
}
    

function MakeDMG () {
    if [ ! -f "../source/$PRODUCT" ]; then
        echo "================= ERROR $PRODUCT is not present in source dir ========= "
        exit
    fi
    BITS="64"
	# -------- Packaging starts here --------
	rm -Rf $WORK
	mkdir -p $CONTENTS
	ln -s /Applications $WORK/Applications
	mkdir "$CONTENTS"/SharedSupport
	mkdir "$CONTENTS"/Resources
	mkdir "$CONTENTS"/MacOS
	MANWIDTH=70 man ../doc/tomboy-ng.1 > "$CONTENTS"/SharedSupport/readme.txt
	cp -R ../doc/html "$CONTENTS"/SharedSupport/.
	sed "s/REPLACEVER/\"$VERSION\"/" Info.plist > "$CONTENTS/Info.plist"
	# cp Info.plist "$CONTENTS/."
	cp PkgInfo "$CONTENTS/."
	cp ../glyphs/tomboy-ng.icns "$CONTENTS/Resources/."
	# for i in $MANUALS; do
	#	cp ../doc/"$i" "$CONTENTS/Resources/.";
	# dmk_dmg.bashone;
	cp -R ../doc/HELP "$CONTENTS/Resources/."
	mkdir "$CONTENTS/MacOS/locale"
	for i in `ls -b ../po/*.??.po`; do
            echo "Name is $i"
            BASENAME=`basename -s.po "$i"`
            CCODE=`echo "$BASENAME" | cut -d '.' -f2`
            echo "CCode is $CCODE"
            BASENAME=`basename -s."$CCODE" "$BASENAME"`
	    mkdir -p "$CONTENTS/MacOS/locale/$CCODE"
	    "$MSGFMT" -o "$CONTENTS/MacOS/locale/$CCODE"/"$BASENAME".mo "$i"
	    "$MSGFMT" -o "$CONTENTS/MacOS/locale/$CCODE"/lclstrconsts.mo "$LAZ_FULL_DIR"/lcl/languages/lclstrconsts."$CCODE".po
	done
	cp ../source/"$PRODUCT" "$CONTENTS/MacOS/."
	rm -f "$PRODUCT""$BITS"_"$VERSION".dmg
	# ~/create-dmg-master/create-dmg --volname "$PRODUCT""$BITS" --volicon "../glyphs/vol.icns" "$PRODUCT""$BITS"_"$VERSION".dmg "./$WORK/"
	create-dmg --volname "$PRODUCT""$BITS" --volicon "../glyphs/vol.icns" "$PRODUCT""$BITS"_"$VERSION".dmg "./$WORK/"
}
# mk_dmg.bash   WTF ?
if [ "$PACKAGEONLY" == "false" ]; then
    if [ "LAZ_PCP" == "" ]; then
	echo "Failed to find config file"
	exit
    fi
fi

rm -f *.dmg
# MakeDMG "carbon"
# We don't bother building carbon any more, it should still build, must test occasionally. July 2020
# MakeDMG "cocoa"

rm ../source/tomboy-ng-*

MakeBinary "CocoaRelease"
MakeBinary "CocoaArmRelease"
MakeUniversal
MakeDMG

