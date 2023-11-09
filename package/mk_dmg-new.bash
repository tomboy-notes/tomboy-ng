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


function MakeDMG () {
	if [ "$1" = "carbon" ]; then
		CPU="i386"
		BITS="32"
		REL="CarbonRelease"
	else
		CPU="x86_64"
		BITS="64"
		REL="CocoaRelease"
	fi
	cd ../source
	rm -f "$PRODUCT"	# we are building a new one, either with fpc or lipo

	if [ "$PACKAGEONLY" == 'false' ]; then

#	    TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild   --pcp="$HOME/.$LAZ_DIR" -B --cpu="$CPU" --ws="$1" --build-mode="$REL" --os="darwin" Tomboy_NG.lpi
	
	    TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild "$LAZ_PCP"   -B --cpu="$CPU" --ws="$1" --build-mode="$REL" --os="darwin" Tomboy_NG.lpi
	    if [ ! -f "$PRODUCT" ]; then
		echo "------------------------------------"
		echo "Failed to build ""$BITS"" bit binary"
		echo "------------------------------------"
		exit
	    fi
	else		# ie, package only mode
	    lipo -create -output tomboy-ng tomboy-ng-x86_64 tomboy-ng-aarch64
	fi
	strip "$PRODUCT"
	cd ../package

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
MakeDMG "cocoa"

