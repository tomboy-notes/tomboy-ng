#!/bin/bash
# A script to build tomboy and make deb packages and zip up the other binaries
# see https://www.debian.org/doc/manuals/debian-faq/ch-pkg_basics
# we can also add preinst, postinst, prerm, and postrm scripts if required
# David Bannon, November, 2017
# Assumes a working FPC/Lazarus install with cross compile tools as described in
# http://wiki.lazarus.freepascal.org/Cross_compiling_for_Win32_under_Linux and
# http://wiki.lazarus.freepascal.org/Cross_compiling
# and that a 'Release' mode exists.

# ----------------------------------------------------------------------------
# Typical usage -
#          PATH="$HOME"/lazarus/lazarus_1_8_4:"$PATH" bash ./package_debian.sh
# ----------------------------------------------------------------------------

PRODUCT="tomboy-ng"
VERSION=`cat version`

SOURCE_DIR="../tomboy-ng"
ICON_DIR="../glyphs"

WHOAMI="David Bannon <tomboy-ng@bannons.id.au>"
MANUALS_DIR="BUILD/usr/share/doc/$PRODUCT/"
# MANUALS="recover.note tomdroid.note tomboy-ng.note sync-ng.note ToDo.note"
MANUALS=`cat note-files`

# BUILDOPTS=" -B --quiet --quiet"
BUILDOPTS=" -B --quiet"
BUILDDATE=`date -R`

# ----------------------

# Build four binaries. Note that build-mode must be one already defined
# typically in the IDE.
# Lazbuild expects cpu=[x86_64, i386] (good luck with the others)


function BuildIt () {
	cd $SOURCE_DIR
	echo "Building x86_64 Linux"
	TOMBOY_NG_VER="$VERSION" lazbuild $BUILDOPTS --cpu="x86_64" --build-mode=Release --os="linux" Tomboy_NG.lpi
	echo "Building i386 Linux"
	TOMBOY_NG_VER="$VERSION" lazbuild $BUILDOPTS --cpu="i386" --build-mode=ReleaseLin32 --os="linux" Tomboy_NG.lpi
	echo "Building x86_64 Windows"
	TOMBOY_NG_VER="$VERSION" lazbuild $BUILDOPTS --cpu="x86_64" --build-mode=ReleaseWin64 --os="win64" Tomboy_NG.lpi
	echo "Building i386 Windows"
	TOMBOY_NG_VER="$VERSION" lazbuild $BUILDOPTS --cpu="i386" --build-mode=ReleaseWin32 --os="win32" Tomboy_NG.lpi
	echo "Building x86_64 Linux"
	# Todo - should check we now have binaries with todays date.
	echo "------------- FINISHED BUILDING -----------------"
	ls -l tomboy-ng*
	echo "-------------------------------------------------"
	cd ../package
}	

# -----------------------

function DebianPackage () {
	# We build a debian tree in BUILD and call dpkg-deb -b 
	#  BUILD/DEBIAN control,debian-binary and any scripts
	mkdir -p BUILD/DEBIAN
	mkdir BUILD/usr
	mkdir BUILD/usr/bin
	mkdir BUILD/usr/share
	for i in 16x16 22x22 24x24 32x32 48x48 256x256; do
		mkdir -p "BUILD/usr/share/icons/hicolor/$i/apps";
		cp "$ICON_DIR/$i.png" "BUILD/usr/share/icons/hicolor/$i/apps/$PRODUCT.png";
	done;
	mkdir -p BUILD/usr/share/doc/$PRODUCT
	# cp ../copyright BUILD/usr/share/doc/$PRODUCT/copyright
	cp ../doc/authors BUILD/usr/share/doc/$PRODUCT/.
	for i in $MANUALS; do
		cp ../doc/"$i" BUILD/usr/share/doc/$PRODUCT/.;
	done;
    # cp ../doc/recover.note BUILD/usr/share/doc/$PRODUCT/.
	mkdir BUILD/usr/share/applications
	cp "$ICON_DIR/$PRODUCT.desktop" BUILD/usr/share/applications/.
	mkdir -p BUILD/usr/share/man/man1
	gzip -9kn ../doc/$PRODUCT.1
	mv ../doc/$PRODUCT.1.gz BUILD/usr/share/man/man1/.
	if [ "$1" = "amd64" ]; then
		cp $SOURCE_DIR/tomboy-ng BUILD/usr/bin/tomboy-ng
	else
		cp $SOURCE_DIR/tomboy-ng32 BUILD/usr/bin/tomboy-ng
	fi
	# cp "$SOURCE_DIR/$MANUALS" "BUILD/usr/share/doc/$PRODUCT/"
	cp -R "../doc/html" "BUILD/usr/share/doc/$PRODUCT/."
	chmod 0755 "BUILD/usr/share/doc/$PRODUCT/html" 
	chmod 0744 "BUILD/usr/share/doc/$PRODUCT/html/*"

	echo "Package: $PRODUCT" > BUILD/DEBIAN/control
	echo "Version: $VERSION" >> BUILD/DEBIAN/control
	echo "Architecture: $1" >> BUILD/DEBIAN/control
	echo "Maintainer: $WHOAMI" >> BUILD/DEBIAN/control
	echo "Installed-Size: 4096" >> BUILD/DEBIAN/control
	# We don't use libcanberra-gtk-module but binary complains when on an OS that does not have it, sigh ...
	echo "Depends: libgtk2.0-0 (>= 2.6), libc6 (>= 2.14), libcanberra-gtk-module" >> BUILD/DEBIAN/control
	echo "Priority: optional" >> BUILD/DEBIAN/control
	echo "Homepage: https://wiki.gnome.org/Apps/Tomboy" >> BUILD/DEBIAN/control
	echo "Section: x11" >> BUILD/DEBIAN/control
	echo "Description: Tomboy Notes rewritten to make installation and cross platform easier." >> BUILD/DEBIAN/control
	echo " Please report your experiences." >> BUILD/DEBIAN/control

	echo "tomboy-ng ($VERSION)  unstable;  urgency=medium" >> "$MANUALS_DIR"changelog
	echo "  * Initial release" >> "$MANUALS_DIR"changelog
	echo "-- David Bannon <tomboy-ng@bannons.id.au>  $BUILDDATE" >> "$MANUALS_DIR"changelog
	gzip -9n "$MANUALS_DIR"changelog

	echo "Format-Specification: http://svn.debian.org/wsvn/dep/web/deps/dep5.mdwn?op=file&rev=135" > BUILD/usr/share/doc/$PRODUCT/copyright
	echo "Name: tomboy-ng" >> BUILD/usr/share/doc/$PRODUCT/copyright
	echo "Maintainer: $WHOAMI" >> BUILD/usr/share/doc/$PRODUCT/copyright
	echo "Source: https://github.com/tomboy-notes/tomboy-ng" >> BUILD/usr/share/doc/$PRODUCT/copyright
	echo "Copyright: 2017-2018 $WHOAMI" >> BUILD/usr/share/doc/$PRODUCT/copyright
	echo "License: GPL-3+" >> BUILD/usr/share/doc/$PRODUCT/copyright

  	# echo "2.0" >> BUILD/DEBIAN/debian-binary
	# echo "calling dpkg for ""$PRODUCT""_$VERSION-0_$1.deb"
	chmod -R g-w BUILD
  	fakeroot dpkg-deb -b BUILD/. "$PRODUCT""_$VERSION-0_$1.deb"
	rm -rf BUILD
}

function DoZipping {
	# Note windows cannot handle gzip'ed files, use zip. 
	rm *.gz
	cp ../tomboy-ng/tomboy-ng .
	gzip -q tomboy-ng
	mv tomboy-ng.gz tomboy-ng64_$VERSION.gz

	cp ../tomboy-ng/tomboy-ng32 .
	gzip -q tomboy-ng32
	mv tomboy-ng32.gz tomboy-ng32_$VERSION.gz

	rm *.zip
	rm -Rf "$PRODUCT"_"$VERSION"
	mkdir "$PRODUCT"_"$VERSION"
	cp ../tomboy-ng/tomboy-ng64.exe "$PRODUCT"_"$VERSION/."
	cp ../../DLL_64bit/libhunspell.dll "$PRODUCT"_"$VERSION/."
	cp ../../DLL_64bit/libhunspell.license "$PRODUCT"_"$VERSION/."
	for i in $MANUALS; do
		cp ../doc/$i "$PRODUCT"_"$VERSION/."
	done;
	MANWIDTH=70 man -l ../doc/tomboy-ng.1 > "$PRODUCT"_"$VERSION/readme.txt"
	unix2dos "$PRODUCT"_"$VERSION/readme.txt"
	zip "$PRODUCT"_win64_"$VERSION.zip" "$PRODUCT"_"$VERSION"/* 

	cp ../tomboy-ng/tomboy-ng32.exe .
	zip "$PRODUCT"_win32_"$VERSION.zip" tomboy-ng32.exe

	echo "--------------- FINISHED ZIPPING ----------------"
	ls -l *.gz *.zip
	echo "------------------ver $VERSION ------------------"
}

# --------------------------------------
# It all starts here


BuildIt
DebianPackage "i386"
DebianPackage "amd64"
echo "----------------- FINISHED DEBs ver $VERSION ------------"
ls -l *.deb
echo "------------------------------------------------"
DoZipping






