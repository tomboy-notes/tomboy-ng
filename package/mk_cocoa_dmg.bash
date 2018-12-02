#!/bin/bash
# ------------------------------------------------------------
#
# A script to generate Mac's tomboy-ng dmg file

# NOTE - hack up to build 64bit cocoa from 32bit IDE base. May 2018
#
# call with a path set to trunk version of Lazarus, ie
# PATH=/somewhere/:$PATH bash mk_cocoa.bash
# edit the bit before lazbuild...... and where the config file is
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
LAZ_DIR=fixes_2_0
MANUALS="recover.note tomdroid.note"


cd ../"$PRODUCT"
# lazbuild -vm5024 --pcp=~/.laz-svn -B --compiler=ppcx64 --cpu="x86_64" --ws=cocoa --build-mode=Release --os="darwin" Tomboy_NG.lpi
TOMBOY_NG_VER="$VERSION" ../../../"$LAZ_DIR"/lazbuild  --pcp=~/."$LAZ_DIR" -B --compiler=/usr/local/bin/ppcx64 --cpu="x86_64" --ws=cocoa --build-mode=Release --os="darwin" Tomboy_NG.lpi
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

rm "$PRODUCT"64.dmg

# ~/create-dmg-master/create-dmg --volname "tomboy-ng" --background ../glyphs/Note_Large.png tomboy-ng.dmg ./source_folder/
~/create-dmg-master/create-dmg --volname "$PRODUCT64" --volicon "../glyphs/vol.icns" "$PRODUCT"64_"$VERSION".dmg "./$WORK/"

