#!/bin/bash
# Ugly hack to make a Qt5 package, Linux 64bit only. Temp measure, must do better.
# expects to find tomboy-ng-qt executable in ../tomboy-ng dir ready made.i
# I cannot, yet build Qt5 version on my build machine [18.04 has old Qt5 lib; 19.10 Lazarus freeze bug]

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
	echo "eg    : $0 \$HOME/bin/lazarus/fixes_2_0"
	echo "or"
	echo "      : $0 clean"
	exit
fi

if [ "$2" == "LeakCheck" ]; then
	LEAKCHECK="YES"
fi


if [ $1 == "clean" ]; then
	rm  -f *.deb
	rm  -f *.gz
	rm  -f *.rpm
	rm -Rf BUILD
	rm -Rf WinPre*
	exit
fi

# ----------------------

# Build four binaries. Note that build-mode must be one already defined
# typically in the IDE.
# Lazbuild expects cpu=[x86_64, i386] (good luck with the others)

function BuildIt () {
	cd $SOURCE_DIR
	echo "Building Qt5 x86_64 Linux"
	rm -f tomboy-ng
#	if [ "$LEAKCHECK" == "YES" ]; then
#		LAZMODE="LeakCheckLin64"
#	else
		LAZMODE="QT5"
#	fi
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="x86_64" --build-mode="$LAZMODE" --os="linux" Tomboy_NG.lpi
	echo "Building x86_64 Linux"
	rm -f tomboy-ng
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
	rm -f tomboy-ng32
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="i386" --build-mode="$LAZMODE" --os="linux" Tomboy_NG.lpi
	echo "Building x86_64 Windows"
	rm -f tomboy-ng64.exe
	if [ "$LEAKCHECK" == "YES" ]; then
		LAZMODE="LeakCheckWin64"
	else
		LAZMODE="ReleaseWin64"
	fi	
	TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="x86_64" --build-mode="$LAZMODE" --os="win64" Tomboy_NG.lpi
	echo "Building i386 Windows"
	rm -f tomboy-ng32.exe
	if [ "$LEAKCHECK" == "YES" ]; then
		LAZMODE="LeakCheckWin32"
	else
		LAZMODE="ReleaseWin32"
	fi	
	TOMBOY_NG_VER="$VERSION" "$LAZ_FULL_DIR"/lazbuild $BUILDOPTS --pcp=~/."$LAZ_DIR" --cpu="i386" --build-mode="$LAZMODE" --os="win32" Tomboy_NG.lpi
	echo "------------- FINISHED BUILDING -----------------"
	ls -l tomboy-ng*
	cd ../package
}	


function DebianPackage () {
	# We build a debian tree in BUILD and call dpkg-deb -b 
	#  BUILD/DEBIAN control,debian-binary and any scripts
	rm -rf BUILD
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
	msgfmt -o BUILD/usr/share/locale/"$CCODE"/LC_MESSAGES/lclstrconsts.mo "$LAZ_FULL_DIR"/lcl/languages/lclstrconsts."$CCODE".po
    done

	mkdir BUILD/usr/share/applications
	cp "$ICON_DIR/$PRODUCT.desktop" BUILD/usr/share/applications/.
	mkdir -p BUILD/usr/share/man/man1
	gzip -9kn ../doc/$PRODUCT.1
	mv ../doc/$PRODUCT.1.gz BUILD/usr/share/man/man1/.
	if [ "$1" = "amd64" ]; then
		cp $SOURCE_DIR/tomboy-ng BUILD/usr/bin/tomboy-ng
	fi
	if [ "$1" = "i386" ]; then
		cp $SOURCE_DIR/tomboy-ng32 BUILD/usr/bin/tomboy-ng
	fi
	if [ "$1" = "amd64Qt" ]; then
		cp $SOURCE_DIR/tomboy-ng-qt BUILD/usr/bin/tomboy-ng
		chmod 755 BUILD/usr/bin/tomboy-ng
		#mkdir -p BUILD/usr/lib/x86_64-linux-gnu
		#cp /usr/lib/x86_64-linux-gnu/libQt5Pas.so.1.2.6 BUILD/usr/lib/x86_64-linux-gnu/.
		#chmod 0644 BUILD/usr/lib/x86_64-linux-gnu/libQt5Pas.so.1.2.6
		#cd BUILD/usr/lib/x86_64-linux-gnu/
		#ln -s libQt5Pas.so.1.2.6 libQt5Pas.so.1.2
		#ln -s libQt5Pas.so.1.2.6 libQt5Pas.so.1
		#cd ../../../../
		#echo "--------------------------------------"
		#pwd
		#ls -la
		#echo "--------------------------------------"
	fi

	# Remove the html files, too hard to maintain
	# cp -R "../doc/html" "BUILD/usr/share/doc/$PRODUCT/."
	# chmod 0755 BUILD/usr/share/doc/"$PRODUCT"/html 
	# chmod 0644 BUILD/usr/share/doc/"$PRODUCT"/html/*
    # -------------- Make control file
	echo "Package: $PRODUCT" > BUILD/DEBIAN/control
	echo "Version: $VERSION" >> BUILD/DEBIAN/control
	if [ "$1" = "amd64Qt" ]; then
		echo "Architecture: amd64" >> BUILD/DEBIAN/control
	else
		echo "Architecture: $1" >> BUILD/DEBIAN/control
	fi
	echo "Maintainer: $WHOAMI" >> BUILD/DEBIAN/control
	echo "Installed-Size: 4096" >> BUILD/DEBIAN/control
	if [ "$1" = "amd64Qt" ]; then
		# echo "Depends: libQt5Pas1 (>= 2.6~beta-4)" >> BUILD/DEBIAN/control
		# echo "Depends: libc6 (>= 2.14), libgcc1 (>= 1:3.0), libqt5core5a (>= 5.7.0), libqt5gui5 (>= 5.6.0~beta) | libqt5gui5-gles (>= 5.6.0~beta), libqt5network5 (>= 5.6.0~beta), libqt5printsupport5 (>= 5.2.0), libqt5widgets5 (>= 5.6.0~beta), libqt5x11extras5 (>= 5.6.0), libstdc++6 (>= 5)" >> BUILD/DEBIAN/control
		echo "Depends: libqt5pas1" >> BUILD/DEBIAN/control
	else
		# We don't use libcanberra-gtk-module but binary complains when on an OS that does not have it, sigh ...
		echo "Depends: libgtk2.0-0 (>= 2.6), libc6 (>= 2.14), libcanberra-gtk-module, appmenu-gtk2-module" >> BUILD/DEBIAN/control
	fi
	echo "Priority: optional" >> BUILD/DEBIAN/control
	echo "Homepage: https://wiki.gnome.org/Apps/Tomboy" >> BUILD/DEBIAN/control
	echo "Section: x11" >> BUILD/DEBIAN/control
	if [ "$1" = "amd64Qt" ]; then
		echo "Description: Tomboy Notes rewritten to make installation and cross platform easier. Experimental Qt5 release." >> BUILD/DEBIAN/control
	else
		echo "Description: Tomboy Notes rewritten to make installation and cross platform easier. " >> BUILD/DEBIAN/control
	fi
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
	#if [ "$1" = "amd64Qt" ]; then
	#	mv "$PRODUCT""_$VERSION-0_$1.deb" "$PRODUCT"_QT5_"$VERSION-0_$1.deb"
	#fi
}

function WriteZipReadMe () {
	RM="$1/readme.txt"
	echo "This is a tar ball of $PRODUCT $VERSION for Linux. Use this if you cannot use" > "$RM"
	echo "either the deb or rpm on your particular distribution. It contains some of the" >> "$RM"
	echo "files you need but does not really install them nor does it resolve dependancies." >> "$RM"
	echo "Its assumed you know what you are doing." >> "$RM"
	echo "* Files and features not provided here include -" >> "$RM"
	echo "* Language other than English" >> "$RM"
	echo "* tomboy-ng help files" >> "$RM"
	echo "* Ability to have tomboy-ng set itself to autostart" >> "$RM"
	echo "If you need help, please post specific question to tomboy-ng github issues." >> "$RM"
}

function DoGZipping {
	# Note windows cannot handle gzip'ed files, use zip.
        GZIP_DIR="$PRODUCT"-"$VERSION"
	rm -f *.tgz
	for TBVer in tomboy-ng32 tomboy-ng; do
		rm -Rf "$GZIP_DIR"	
		mkdir "$GZIP_DIR"
		cp "../$PRODUCT/$TBVer" "$GZIP_DIR"/"$PRODUCT"
		for i in 16x16 22x22 24x24 32x32 48x48 256x256; do
			cp "$ICON_DIR/$i.png" "$GZIP_DIR/$i.png"
		done;
		cp "$ICON_DIR/install-local.bash" "$GZIP_DIR/install-local.bash"
		cp "$ICON_DIR/$PRODUCT.desktop" "$GZIP_DIR/$PRODUCT.desktop"
		gzip -9kn ../doc/$PRODUCT.1
		mv ../doc/$PRODUCT.1.gz "$GZIP_DIR"/.
		WriteZipReadMe "$GZIP_DIR"
		tar czf "$TBVer"-"$VERSION".tgz "$GZIP_DIR"
		#if [ "$TBVer" = "tomboy-ng32" ]; then
		#	mv "$GZIP_DIR".gz "$PRODUCT"_32_$VERSION.gz"
		#else
		#	mv "$GZIP_DIR".gz "$PRODUCT"_64_$VERSION.gz"
		#fi
		echo "made one gz file -----------------------"
	done;
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
	sed "s/MyAppVersion \"REPLACEME\"/MyAppVersion \"$VERSION\"/" tomboy-ng.iss > "$WIN_DIR/tomboy-ng.iss.temp"
	for i in $MANUALS; do
	    cp ../doc/$i "$WIN_DIR/."
	done;
	# " -------- WRITE mo files --------"
	msgfmt -o "$WIN_DIR"/"$PRODUCT".mo ../po/"$PRODUCT".po
	# Source: "tomboy-ng.mo";     DestDir: "{app}\locale"; Flags: ignoreversion
	echo "Source: \""$PRODUCT".mo\";     DestDir: \"{app}\\locale\"; Flags: ignoreversion" > mo.insert
	for i in `ls -b ../po/*.??.po`; do
            # echo "Name is $i"
            BASENAME=`basename -s.po "$i"`
            CCODE=`echo "$BASENAME" | cut -d '.' -f2`
            # echo "CCode is $CCODE"
            BASENAME=`basename -s."$CCODE" "$BASENAME"`
	    msgfmt -o "$WIN_DIR"/"$BASENAME"."$CCODE".mo "$i"
	    msgfmt -o "$WIN_DIR"/lclstrconsts."$CCODE".mo "$LAZ_FULL_DIR"/lcl/languages/lclstrconsts."$CCODE".po
	    echo "Source: \""$BASENAME"."$CCODE".mo\";     DestDir: \"{app}\\locale\"; Flags: ignoreversion" >> mo.insert
	    echo "Source: \"lclstrconsts."$CCODE".mo\";     DestDir: \"{app}\\locale\"; Flags: ignoreversion" >> mo.insert
	done
	sed '/PUTMOLINESHERE/r mo.insert' "$WIN_DIR"/tomboy-ng.iss.temp > "$WIN_DIR"/tomboy-ng.iss
	MANWIDTH=70 man -l ../doc/tomboy-ng.1 > "$WIN_DIR/readme.txt"
	unix2dos -q "$WIN_DIR/readme.txt"
	echo "----------- Windows installer dir created -----------"
	# ls -la "$WIN_DIR"
}

# --------------------------------------

# It all starts here

#if [ "$2" == "LeakCheck" ]; then
#	BuildItLeakCheck
#else 
	BuildIt
#fi

DebianPackage "amd64Qt";
DebianPackage "i386"
DebianPackage "amd64"

#if [ -f "$SOURCE_DIR/tomboy-ng-qt" ]
#  then DebianPackage "amd64Qt";
#	  echo "-------- WARNING also made Qt deb, is bin current ? -------"
#fi

echo "----------------- FINISHED DEBs ver $VERSION ------------"
ls -l *.deb
DoGZipping
MkWinPreInstaller
ls -ltr





