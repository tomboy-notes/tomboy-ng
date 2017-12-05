#!/bin/sh
# A script to build tomboy and make deb packages and zip up the other binaries
# see https://www.debian.org/doc/manuals/debian-faq/ch-pkg_basics
# we can also add preinst, postinst, prerm, and postrm scripts if required
# David Bannon, November, 2017


PRODUCT="tomboy-ng"
VERSION="0.1"
OS="linux"

SOURCE_DIR="../tomboy-ng"

MANUALS_DIR="docs"
MANUALS="Notes.txt"



# ----------------------

# Build four binaries. Note that build-mode must be one already defined
# typically in the IDE.
# Lazbuild expects cpu=[x86_64, i386] (good luck with the others)

function BuildIt () {
	cd $SOURCE_DIR
	echo "Building x86_64 Linux"
	lazbuild -B --cpu="x86_64" --build-mode=Release --os="linux" Tomboy_NG.lpi
	echo "Building i386 Linux"
	lazbuild -B --cpu="i386" --build-mode=ReleaseLin32 --os="linux" Tomboy_NG.lpi
	echo "Building x86_64 Windows"
	lazbuild -B --cpu="x86_64" --build-mode=ReleaseWin64 --os="win64" Tomboy_NG.lpi
	echo "Building i386 Windows"
	lazbuild -B --cpu="i386" --build-mode=ReleaseWin32 --os="win32" Tomboy_NG.lpi
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
    #	/usr/bin/tomboy-ng
	#	/share/tomboy-ng/Notes.txt,license.txt (todo)
 	mkdir BUILD
	mkdir BUILD/DEBIAN
	mkdir BUILD/usr
	mkdir BUILD/usr/bin
	mkdir BUILD/usr/share
	mkdir "BUILD/usr/share/$PRODUCT"
	if [ "$1" = "amd64" ]; then
		cp $SOURCE_DIR/tomboy-ng BUILD/usr/bin/tomboy-ng
	else
		cp $SOURCE_DIR/tomboy-ng32 BUILD/usr/bin/tomboy-ng
	fi
	cp "$SOURCE_DIR/$MANUALS" "BUILD/usr/share/$PRODUCT/"

	echo "Package: $PRODUCT" > BUILD/DEBIAN/control
	echo "Version: $VERSION" >> BUILD/DEBIAN/control
	echo "Architecture: $1" >> BUILD/DEBIAN/control
	echo "Maintainer: David Bannon" >> BUILD/DEBIAN/control
	echo "Installed-Size: 4096" >> BUILD/DEBIAN/control
	echo "Depends: libgtk2.0-0 (>= 2.6)" >> BUILD/DEBIAN/control
	echo "Priority: optional" >> BUILD/DEBIAN/control
	echo "Homepage: https://wiki.gnome.org/Apps/Tomboy" >> BUILD/DEBIAN/control
	echo "Section: x11" >> BUILD/DEBIAN/control
	echo "Description: Alpha Test of a Tomboy Notes rewritten to make installation easy." >> BUILD/DEBIAN/control

  	echo "2.0" >> BUILD/DEBIAN/debian-binary
	# echo "calling dpkg for ""$PRODUCT""_$VERSION-0_$1.deb"
  	dpkg-deb -b BUILD/. "$PRODUCT""_$VERSION-0_$1.deb"
	rm -rf BUILD
	echo "----------------- FINISHED DEBs ----------------"
	ls -l *.deb
	echo "------------------------------------------------"
}

function DoZipping {
	# Note windows cannot handle gzip'ed files, use zip. 
	rm *.gz
	cp ../tomboy-ng/tomboy-ng .
	gzip -q tomboy-ng
	mv tomboy-ng.gz tomboy-ng_$VERSION.gz

	cp ../tomboy-ng/tomboy-ng32 .
	gzip -q tomboy-ng32
	mv tomboy-ng32.gz tomboy-ng32_$VERSION.gz

	rm *.zip
	cp ../tomboy-ng/tomboy-ng64.exe .
	zip tomboy-ng_Win64_$VERSION.zip tomboy-ng64.exe 

	cp ../tomboy-ng/tomboy-ng32.exe .
	zip tomboy-ng_Win32_$VERSION.zip tomboy-ng32.exe

	echo "--------------- FINISHED ZIPPING ----------------"
	ls -l *.gz *.zip
	echo "-------------------------------------------------"
}

# --------------------------------------
# It all starts here

BuildIt
DebianPackage "i386"
DebianPackage "amd64"
DoZipping






