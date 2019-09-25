#!/bin/bash
# ------------------------------------------------------------
#
# A script to generate Mac's tomboy-ng dmg files

# Typical Usage : ./mk_dmg.bash $HOME/bin/lazarus/laz-200

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


if [ -z "$LAZ_DIR" ]; then
	echo "Usage : $0 /Full/Path/Lazarus/dir"
	echo "eg    : $0 \$HOME/bin/lazarus/laz-200"
	exit
fi

function MakeDMG () {
	if [ "$1" = "carbon" ]; then
		CPU="i386"
		BITS="32"
		REL="Release"
	else
		CPU="x86_64"
		BITS="64"
		REL="CocoaRelease"
	fi
	cd ../tomboy-ng
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild   --pcp="$HOME/.$LAZ_DIR" -B --cpu="$CPU" --ws="$1" --build-mode="$REL" --os="darwin" Tomboy_NG.lpi
	cd ../package

	rm -Rf $WORK
	mkdir -p $CONTENTS
	ln -s /Applications $WORK/Applications
	mkdir "$CONTENTS"/SharedSupport
	mkdir "$CONTENTS"/Resources
	mkdir "$CONTENTS"/MacOS
	MANWIDTH=70 man ../doc/tomboy-ng.1 > "$CONTENTS"/SharedSupport/readme.txt
	cp -R ../doc/html "$CONTENTS"/SharedSupport/.
	cp Info.plist "$CONTENTS/."
	cp PkgInfo "$CONTENTS/."
	cp ../glyphs/tomboy-ng.icns "$CONTENTS/Resources/."
	for i in $MANUALS; do
		cp ../doc/"$i" "$CONTENTS/Resources/.";
	done;
	mkdir "$CONTENTS/MacOS/locale"
	for i in `ls -b ../po/*.??.po`; do
            echo "Name is $i"
            BASENAME=`basename -s.po "$i"`
            CCODE=`echo "$BASENAME" | cut -d '.' -f2`
            echo "CCode is $CCODE"
            BASENAME=`basename -s."$CCODE" "$BASENAME"`
	    mkdir -p "$CONTENTS/MacOS/locale/$CCODE"
	    "$MSGFMT" -o "$CONTENTS/MacOS/locale/$CCODE"/"$BASENAME".mo "$i"
	    "$MSGFMT" -o "$CONTENTS/MacOS/locale/$CCODE/lclstrconsts.mo "$FULL_LAZ_DIR"/lcl/languages/lclstrconsts."$CCODE".po
	done
	mv "../$PRODUCT"/"$PRODUCT" "$CONTENTS/MacOS/."
	rm "$PRODUCT""$BITS"_"$VERSION".dmg
	~/create-dmg-master/create-dmg --volname "$PRODUCT""$BITS" --volicon "../glyphs/vol.icns" "$PRODUCT""$BITS"_"$VERSION".dmg "./$WORK/"
}

MakeDMG "carbon"
MakeDMG "cocoa"

