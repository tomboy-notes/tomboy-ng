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
cd ../tomboy-ng
lazbuild -B --cpu="i386" --build-mode=Release --os="darwin" Tomboy_NG.lpi
cd ../package
rm -Rf source_folder

mkdir source_folder
ln -s /Applications source_folder/Applications
cp -R ../tomboy-ng/tomboy-ng.app source_folder/.
rm source_folder/tomboy-ng.app/Contents/MacOS/tomboy-ng
mv ../tomboy-ng/tomboy-ng source_folder/tomboy-ng.app/Contents/MacOS/.

ls -n source_folder/

rm tomboy-ng.dmg

# ~/create-dmg-master/create-dmg --volname "tomboy-ng" --background ../glyphs/Note_Large.png tomboy-ng.dmg ./source_folder/
~/create-dmg-master/create-dmg --volname "tomboy-ng" tomboy-ng.dmg ./source_folder/

