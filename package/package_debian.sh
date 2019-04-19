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
#          ./package_debian.sh $HOME"/lazarus/laz-200

# Note we assume laz config has same name as Laz directory, ie .laz-200
# ----------------------------------------------------------------------------

PRODUCT="tomboy-ng"
VERSION=`cat version`

SOURCE_DIR="../tomboy-ng"
ICON_DIR="../glyphs"

WHOAMI="David Bannon <tomboy-ng@bannons.id.au>"
MANUALS_DIR="BUILD/usr/share/doc/$PRODUCT/"
MANUALS=`cat note-files`

BUILDOPTS=" -B --quiet --quiet"
# BUILDOPTS=" -B --quiet"
BUILDDATE=`date -R`
LAZ_FULL_DIR="$1"
LAZ_DIR=`basename "$LAZ_FULL_DIR"`

if [ -z "$LAZ_DIR" ]; then
	echo "Usage : $0 /Full/Path/Lazarus/dir"
	echo "eg    : $0 \$HOME/bin/lazarus/laz-200"
	exit
fi
# ----------------------

# Build four binaries. Note that build-mode must be one already defined
# typically in the IDE.
# Lazbuild expects cpu=[x86_64, i386] (good luck with the others)

function BuildIt () {
	cd $SOURCE_DIR
	echo "Building x86_64 Linux"
	rm tomboy-ng
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="x86_64" --build-mode=Release --os="linux" Tomboy_NG.lpi
	echo "Building i386 Linux"
	rm tomboy-ng32
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="i386" --build-mode=ReleaseLin32 --os="linux" Tomboy_NG.lpi
	echo "Building x86_64 Windows"
	rm tomboy-ng64.exe
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="x86_64" --build-mode=ReleaseWin64 --os="win64" Tomboy_NG.lpi
	echo "Building i386 Windows"
	rm tomboy-ng32.exe
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="i386" --build-mode=ReleaseWin32 --os="win32" Tomboy_NG.lpi
	echo "Building x86_64 Linux"
	# Todo - should check we now have binaries with todays date.
	echo "------------- FINISHED BUILDING -----------------"
	ls -l tomboy-ng*
	echo "-------------------------------------------------"
	cd ../package
}	

function BuildItLeakCheck () {
	cd $SOURCE_DIR
	echo "Building x86_64 Linux"
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="x86_64" --build-mode=LeakCheckLin64 --os="linux" Tomboy_NG.lpi
	echo "Building i386 Linux"
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="i386" --build-mode=LeakCheckLin32 --os="linux" Tomboy_NG.lpi
	echo "Building x86_64 Windows"
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="x86_64" --build-mode=LeakCheckWin64 --os="win64" Tomboy_NG.lpi
	echo "Building i386 Windows"
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="i386" --build-mode=LeakCheckWin32 --os="win32" Tomboy_NG.lpi
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
	echo "-- $WHOAMI  $BUILDDATE" >> "$MANUALS_DIR"changelog
	gzip -9n "$MANUALS_DIR"changelog

    # See https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/#file-syntax
	echo "Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/" > BUILD/usr/share/doc/$PRODUCT/copyright
	echo "Name: $PRODUCT" >> BUILD/usr/share/doc/$PRODUCT/copyright
	echo "Maintainer: $WHOAMI" >> BUILD/usr/share/doc/$PRODUCT/copyright
	echo "Source: https://github.com/tomboy-notes/tomboy-ng" >> BUILD/usr/share/doc/$PRODUCT/copyright	
	echo "License: GPL-3.0-or-later" >> BUILD/usr/share/doc/$PRODUCT/copyright
    echo "Copyright: 2017-2019 $WHOAMI" >> BUILD/usr/share/doc/$PRODUCT/copyright
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

    # we don't make individual win zip files anymore.
	# mkdir "$PRODUCT"_"$VERSION"
	# cp ../tomboy-ng/tomboy-ng64.exe "$PRODUCT"_"$VERSION/."
	# cp ../../DLL_64bit/libhunspell.dll "$PRODUCT"_"$VERSION/."
	# cp ../../DLL_64bit/libhunspell.license "$PRODUCT"_"$VERSION/."
	# cp ../COPYING "$PRODUCT"_"$VERSION/."
	# cp AfterInstall.txt "$PRODUCT"_"$VERSION/."
	# sed "s/MyAppVersion \"REPLACEME\"/MyAppVersion \"$VERSION\"/" tomboy-ng.iss > "$PRODUCT"_"$VERSION/tomboy-ng.iss"
	# for i in $MANUALS; do
	# 	cp ../doc/$i "$PRODUCT"_"$VERSION/."
	# done;
	# MANWIDTH=70 man -l ../doc/tomboy-ng.1 > "$PRODUCT"_"$VERSION/readme.txt"
	# unix2dos "$PRODUCT"_"$VERSION/readme.txt"
	# zip "$PRODUCT"_win64_"$VERSION.zip" "$PRODUCT"_"$VERSION"/* 
	# cp ../tomboy-ng/tomboy-ng32.exe .
	# zip "$PRODUCT"_win32_"$VERSION.zip" tomboy-ng32.exe


	# Make a zip containing everything we need to make a 32/64bit Inno Setup installer for Windows
	rm -Rf WinPre_"$VERSION"
	mkdir WinPre_"$VERSION"
	cp ../tomboy-ng/tomboy-ng64.exe WinPre_"$VERSION/."
	cp ../../DLL_64bit/libhunspell.dll WinPre_"$VERSION/."
	cp ../../DLL_64bit/libhunspell.license WinPre_"$VERSION/."
	cp ../COPYING WinPre_"$VERSION/."
	cp AfterInstall.txt WinPre_"$VERSION/."
	sed "s/MyAppVersion \"REPLACEME\"/MyAppVersion \"$VERSION\"/" tomboy-ng.iss > WinPre_"$VERSION/tomboy-ng.iss"
	for i in $MANUALS; do
		cp ../doc/$i WinPre_"$VERSION/."
	done;
	MANWIDTH=70 man -l ../doc/tomboy-ng.1 > WinPre_"$VERSION/readme.txt"
	cp ../tomboy-ng/tomboy-ng32.exe WinPre_"$VERSION"/.
	unix2dos WinPre_"$VERSION/readme.txt"
	ls -la WinPre_"$VERSION"
	# zip WinPre_"$VERSION.zip" WinPre_"$VERSION"/*
	
	echo "--------------- FINISHED ZIPPING ----------------"
	ls -l *.gz *.zip
	echo "------------------ver $VERSION ------------------"
}

# --------------------------------------
# It all starts here

if [ "$2" == "LeakCheck" ]; then
	BuildItLeakCheck
else 
	BuildIt
fi

DebianPackage "i386"
DebianPackage "amd64"
echo "----------------- FINISHED DEBs ver $VERSION ------------"
ls -l *.deb
echo "------------------------------------------------"
DoZipping






