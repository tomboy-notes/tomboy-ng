#!/bin/sh
# A script to make tomboy-ng deb package
# see https://www.debian.org/doc/manuals/debian-faq/ch-pkg_basics
# we can also add preinst, postinst, prerm, and postrm scripts if required
# David Bannon, November, 2017


PRODUCT="tomboy-ng"
VERSION="0.1"
OS="linux"
CPU="x86_64"
#x86_64 or i386 (good luck with the others), these are what the FPC/Lazarus expect.

SOURCE_DIR="../tomboy-ng"

MANUALS_DIR="docs"
MANUALS="Notes.txt"

USR_DIR="usr"

# ----------------------

BuildIt ()
{
  cd $SOURCE_DIR
  lazbuild -B --cpu="$CPU" --build-mode=Release --os="$OS" Tomboy_NG.lpi
  cd ../package
}	

# -----------------------

DebianPackage ()
{
	# This is a remote chance someone might run this script in /
	# doing so would trash their /usr directory, a bad thing. 
	if mkdir $USR_DIR; then		 
		echo "OK $USR_DIR correctly made"
	else
		echo "DANGER - do not run this script in root"
		exit
	fi
	mkdir $USR_DIR/bin
	mkdir $USR_DIR/share
	mkdir $USR_DIR/share/$PRODUCT
	cp $SOURCE_DIR/tomboy-ng $USR_DIR/bin/tomboy-ng
	cp "$SOURCE_DIR/$MANUALS" "$USR_DIR/share/tomboy-ng"

	tar -cvf data.tar $USR_DIR
 	gzip data.tar

 	mkdir DEBIAN

	echo "Package: $PRODUCT" > DEBIAN/control
	echo "Version: $VERSION" >> DEBIAN/control
	echo "Architecture: $ARCH" >> DEBIAN/control
	echo "Maintainer: David Bannon" >> DEBIAN/control
	echo "Installed-Size: 4096" >> DEBIAN/control
	echo "Depends: libgtk2.0-0 (>= 2.6)" >> DEBIAN/control
	echo "Priority: optional" >> DEBIAN/control
	echo "Homepage: https://wiki.gnome.org/Apps/Tomboy" >> DEBIAN/control
	echo "Section: x11" >> DEBIAN/control
	echo "Description: Alpha Test of a Tomboy Notes rewritten to make installation easy." >> DEBIAN/control

  	mv data.tar.gz DEBIAN/
  	echo "2.0" >> DEBIAN/debian-binary

	echo "calling dpkg for $CPU $ARCH"
  	dpkg -b ./ "$PRODUCT""_$VERSION-0_$ARCH.deb"

  	echo "all done, lets tidy up now"
	rm -rf $USR_DIR	# Danger Will Robertson, Danger, that usr
	rm -rf ./DEBIAN		

#  return
}

# --------------------------------------
# It all starts here

if [ "$CPU" = "x86_64" ]; then
	ARCH="amd64"				# These are what appears in the deb file name
else
	ARCH="i386"
fi
BuildIt
DebianPackage






