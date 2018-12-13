#!/bin/bash
# ------------------------------------------------------------
#
# A script to generate Mac's tomboy-ng dmg file
#
# Depends (heavily) on https://github.com/andreyvit/create-dmg
# which must be installed.
#
# Todo - Generate a Mac icon and sensible background image.
#		 Probably should put license and readme in there too.
# -------------------------------------------------------------

PRODUCT=tomboy-ng
WORK=source_folder
CONTENTS="$WORK/""$PRODUCT".app/Contents
VERSION=`cat version`
MANUALS="recover.note tomdroid.note sync-ng.note tomboy-ng.note"

cd ../tomboy-ng
TOMBOY_NG_VER="$VERSION" lazbuild -B --cpu="i386" --build-mode=Release --os="darwin" Tomboy_NG.lpi
cd ../package

rm -Rf $WORK

mkdir $WORK
ln -s /Applications $WORK/Applications
cp -R ../"$PRODUCT"/"$PRODUCT".app $WORK/.
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

rm "$PRODUCT"32_"$VERSION".dmg

# ~/create-dmg-master/create-dmg --volname "tomboy-ng" --background ../glyphs/Note_Large.png tomboy-ng.dmg ./source_folder/
# ~/create-dmg-master/create-dmg --volname "tomboy-ng" tomboy-ng.dmg ./source_folder/

~/create-dmg-master/create-dmg --volname "$PRODUCT32" --volicon "../glyphs/vol.icns" "$PRODUCT"32_"$VERSION".dmg "./$WORK/"


