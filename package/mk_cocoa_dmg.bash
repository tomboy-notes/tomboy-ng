#!/bin/bash
# ------------------------------------------------------------
#
# A script to generate Mac's tomboy-ng dmg file
#
# Typical Usage : ./mk_cocoa_dmg.bash $HOME/bin/lazarus/laz-200
#
# Note we assume config is named same as lazarus dir, ie .laz-200
#
# Depends (heavily) on https://github.com/andreyvit/create-dmg
# which must be installed.
#
# Todo - 
#		 Probably should put license and readme in there too.
# -------------------------------------------------------------


PRODUCT=tomboy-ng
WORK=source_folder
CONTENTS="$WORK/""$PRODUCT".app/Contents
VERSION=`cat version`
LAZ_FULL_DIR="$1"
LAZ_DIR=`basename "$LAZ_FULL_DIR"`

MANUALS=`cat note-files`

echo "$LAZ_DIR is where we are heading and $LAZ_FULL_DIR is how we get there"

if [ -z "$LAZ_DIR" ]; then
	echo "Usage : $0 /Full/Path/Lazarus/dir"
	echo "eg    : $0 \$HOME/bin/lazarus/laz-200"
	exit
fi

cd ../"$PRODUCT"
TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild  --pcp=~/."$LAZ_DIR" -B --compiler=/usr/local/bin/ppcx64 --cpu="x86_64" --ws=cocoa --build-mode=Release --os="darwin" Tomboy_NG.lpi
cd ../package

rm -Rf $WORK
mkdir $WORK
ln -s /Applications $WORK/Applications
cp -R "$PRODUCT".app-ref "$WORK"/"$PRODUCT".app
mkdir "$CONTENTS"/SharedSupport
MANWIDTH=70 man ../doc/tomboy-ng.1 > "$CONTENTS"/SharedSupport/readme.txt
cp -R ../doc/html "$CONTENTS"/SharedSupport/.
cp Info.plist "$CONTENTS/."
cp ../glyphs/tomboy-ng.icns "$CONTENTS/Resources/."
for i in $MANUALS; do
	cp ../doc/"$i" "$CONTENTS/Resources/.";
done;
rm "$CONTENTS/MacOS/""$PRODUCT"
mv "../$PRODUCT"/"$PRODUCT" "$CONTENTS/MacOS/."

ls -n $WORK/

rm "$PRODUCT"64_"$VERSION".dmg

# ~/create-dmg-master/create-dmg --volname "tomboy-ng" --background ../glyphs/Note_Large.png tomboy-ng.dmg ./source_folder/
~/create-dmg-master/create-dmg --volname "$PRODUCT64" --volicon "../glyphs/vol.icns" "$PRODUCT"64_"$VERSION".dmg "./$WORK/"

