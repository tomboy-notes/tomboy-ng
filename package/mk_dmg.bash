#!/bin/bash
# ------------------------------------------------------------
#
# A script to generate Mac's tomboy-ng dmg files

# Typical Usage : bash ./mk_dmg.bash $HOME/bin/lazarus/fixes_2_0

# Note we assume config is named same as lazarus dir, ie .laz-200 
#
# Depends (heavily) on https://github.com/andreyvit/create-dmg
# which must be installed.
#
#		 Probably should put license and readme in there too.
# -------------------------------------------------------------
LAZ_FULL_DIR="$1"
LAZ_DIR=`basename "$LAZ_FULL_DIR"`
PRODUCT=tomboy-ng
WORK=source_folder
CONTENTS="$WORK/""$PRODUCT".app/Contents
VERSION=`cat version`
MANUALS=`cat note-files`
MSGFMT="/usr/local/Cellar/gettext/0.19.8.1/bin/msgfmt"
VERSION=`cat version`

if [ -z "$LAZ_DIR" ]; then
	echo "Usage : $0 /Full/Path/Lazarus/dir"
	echo "eg    : $0 \$HOME/bin/lazarus/fixes_2_0"
	exit
fi

if [ ! -f "$LAZ_FULL_DIR"/lazbuild ]; then
	echo "Sorry, ""$LAZ_FULL_DIR"" does not look like it contains a Lazarus build"
	exit
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
	rm -f "$PRODUCT"
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild   --pcp="$HOME/.$LAZ_DIR" -B --cpu="$CPU" --ws="$1" --build-mode="$REL" --os="darwin" Tomboy_NG.lpi
	if [ ! -f "$PRODUCT" ]; then
		echo "------------------------------------"
		echo "Failed to build ""$BITS"" bit binary"
		echo "------------------------------------"
		exit
	fi
	cd ../package

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
	mv ../source/"$PRODUCT" "$CONTENTS/MacOS/."
	rm -f "$PRODUCT""$BITS"_"$VERSION".dmg
	~/create-dmg-master/create-dmg --volname "$PRODUCT""$BITS" --volicon "../glyphs/vol.icns" "$PRODUCT""$BITS"_"$VERSION".dmg "./$WORK/"
}

rm -f *.dmg
# MakeDMG "carbon"
# We don't bother building carbon any more, it should still build, must test occasionally. July 2020
MakeDMG "cocoa"

