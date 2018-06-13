#!/usr/bin/bash
# A quick and dirty script playing with where icons go.
# Definitly not for production use.


APPNAME=tomboy-ng
DEST=/usr/share/icons/hicolor

# for i in 16x16 22x22 24x24 32x32 48x48 256x256 ; do
for i in 256x256 ; do
	mkdir "$DEST"/"$i"/apps/"$APPNAME"
	cp "$i.png" "$DEST/$i/apps/$APPNAME/$APPNAME.png"
	echo "$DEST/$i/apps/$APPNAME/$APPNAME.png"
done;
