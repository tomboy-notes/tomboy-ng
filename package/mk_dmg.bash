#!/bin/bash
# ------------------------------------------------------------
#
# A script to generate Mac's tomboy-ng dmg files

# Typical Usage : bash ./mk_dmg.bash $HOME/bin/lazarus/fixes_2_0
#                 bash ./mk_dmg.bash packageonly
  
# Note we assume config is named same as lazarus dir, ie .laz-200 
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
	# done;
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

