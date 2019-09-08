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
#          ./package_debian.sh $HOME"/lazarus/laz-200 <LeakCheck>

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
WIN_DIR=WinPre_"$VERSION"
LEAKCHECK="NO"

if [ -z "$LAZ_DIR" ]; then
	echo "Usage : $0 /Full/Path/Lazarus/dir"
	echo "eg    : $0 \$HOME/bin/lazarus/laz-200"
	exit
fi

if [ "$2" == "LeakCheck" ]; then
	LEAKCHECK="YES"
fi

# ----------------------

# Build four binaries. Note that build-mode must be one already defined
# typically in the IDE.
# Lazbuild expects cpu=[x86_64, i386] (good luck with the others)

function BuildIt () {
	cd $SOURCE_DIR
	echo "Building x86_64 Linux"
	rm tomboy-ng
	if [ "$LEAKCHECK" == "YES" ]; then
		LAZMODE="LeakCheckLin64"
	else
		LAZMODE="Release"
	fi
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="x86_64" --build-mode="$LAZMODE" --os="linux" Tomboy_NG.lpi
	echo "Building i386 Linux"
	if [ "$LEAKCHECK" == "YES" ]; then
		LAZMODE="LeakCheckLin32"
	else
		LAZMODE="ReleaseLin32"
	fi
	rm tomboy-ng32
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="i386" --build-mode="$LAZMODE" --os="linux" Tomboy_NG.lpi
	echo "Building x86_64 Windows"
	rm tomboy-ng64.exe
	if [ "$LEAKCHECK" == "YES" ]; then
		LAZMODE="LeakCheckWin64"
	else
		LAZMODE="ReleaseWin64"
	fi	
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="x86_64" --build-mode="$LAZMODE" --os="win64" Tomboy_NG.lpi
	echo "Building i386 Windows"
	rm tomboy-ng32.exe
	if [ "$LEAKCHECK" == "YES" ]; then
		LAZMODE="LeakCheckWin32"
	else
		LAZMODE="ReleaseWin32"
	fi	
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="i386" --build-mode="$LAZMODE" --os="win32" Tomboy_NG.lpi
	echo "------------- FINISHED BUILDING -----------------"
	ls -l tomboy-ng*
	echo "-------------------------------------------------"
	cd ../package
}	

# -----------------------

function MkLanguages () {
	# abandoned
	for i in es; do
		cat "../po/tomboy-ng.$i.po" "../po/lclstrconsts.$i.po" | msgfmt -o "tomboy-ng.$i.mo" -;
	done;
}

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
    # -------------- Translation Files
    # we end up with, eg, /usr/share/locale/es/LC_MESSAGES/tomboy-ng.mo
    # and /usr/share/locale/es/LC_MESSAGES/lclstrconsts.mo for Linux 

    # echo " --------WARNING UNTESTED CODE to WRITE mo files --------"
    mkdir -p BUILD/usr/share/locale
    for i in `ls -b ../po/*.??.po`; do		# Deal with each country code in turn
        # echo "Name is $i"
        BASENAME=`basename -s.po "$i"`
        # echo "BASENAME is $BASENAME"
        CCODE=`echo "$BASENAME" | cut -d '.' -f2`
        # echo "CCode is $CCODE"
        mkdir -p BUILD/usr/share/locale/"$CCODE"/LC_MESSAGES
        BASENAME=`basename -s."$CCODE" "$BASENAME"`
	msgfmt -o BUILD/usr/share/locale/"$CCODE"/LC_MESSAGES/"$BASENAME".mo "$i"
	msgfmt -o BUILD/usr/share/locale/"$CCODE"/LC_MESSAGES/lclstrconsts.mo "$FULL_LAZ_DIR"/lcl/languages/lclstrconsts."$CCODE".po
    done

    # ------------ 
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
    # -------------- Make control file
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

function DoGZipping {
	# Note windows cannot handle gzip'ed files, use zip. 
	rm *.gz
	cp ../tomboy-ng/tomboy-ng .
	gzip -q tomboy-ng
	mv tomboy-ng.gz tomboy-ng64_$VERSION.gz
	cp ../tomboy-ng/tomboy-ng32 .
	gzip -q tomboy-ng32
	mv tomboy-ng32.gz tomboy-ng32_$VERSION.gz
}

function MkWinPreInstaller() {
	# Make a dir containing everything we need to make a 32/64bit Inno Setup installer for Windows
	rm -Rf "$WIN_DIR"
	mkdir "$WIN_DIR"
	cp ../tomboy-ng/tomboy-ng64.exe "$WIN_DIR/."
	cp ../tomboy-ng/tomboy-ng32.exe "$WIN_DIR"/.
	cp ../../DLL_64bit/libhunspell.dll "$WIN_DIR/."
	cp ../../DLL_64bit/libhunspell.license "$WIN_DIR/."
	cp ../COPYING "$WIN_DIR/."
	cp AfterInstall.txt "$WIN_DIR/."
	sed "s/MyAppVersion \"REPLACEME\"/MyAppVersion \"$VERSION\"/" tomboy-ng.iss > "$WIN_DIR/tomboy-ng.iss"
	for i in $MANUALS; do
	    cp ../doc/$i "$WIN_DIR/."
	done;
	echo " --------WARNING UNTESTED CODE to WRITE mo files --------"
	for i in `ls -b ../po/*.??.po`; do
            echo "Name is $i"
            BASENAME=`basename -s.po "$i"`
            CCODE=`echo "$BASENAME" | cut -d '.' -f2`
            echo "CCode is $CCODE"
            BASENAME=`basename -s."$CCODE" "$BASENAME"`
	    mkdir -p "$WIN_DIR"/locale/"$CCODE"
	    msgfmt -o "$WIN_DIR"/locale/"$CCODE"/"$BASENAME".mo "$i"
	    msgfmt -o "$WIN_DIR"/locale/"$CCODE"/lclstrconsts.mo "$FULL_LAZ_DIR"/lcl/languages/lclstrconsts."$CCODE".po
	done
	MANWIDTH=70 man -l ../doc/tomboy-ng.1 > "$WIN_DIR/readme.txt"
	unix2dos "$WIN_DIR/readme.txt"
	echo "----------- Windows installer dir created -----------"
	ls -la "$WIN_DIR"
}

# --------------------------------------

# It all starts here

#if [ "$2" == "LeakCheck" ]; then
#	BuildItLeakCheck
#else 
	BuildIt
#fi

DebianPackage "i386"
DebianPackage "amd64"
echo "----------------- FINISHED DEBs ver $VERSION ------------"
ls -l *.deb
echo "------------------------------------------------"
DoGZipping
MkWinPreInstaller






