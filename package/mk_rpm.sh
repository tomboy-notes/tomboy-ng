#!/usr/bin/bash

# ====================================================
# a short script to make RPMs from our tomboy-ng debs
# does more than just call alien as that seems to end up
# with commands to create / and /usr/bin that upset yum
#
# This script must be run as root, so useage is -
# sudo bash mk_rpm
#
# ====================================================

PROD=tomboy-ng
VERS=`cat version`
RDIR="$PROD"-"$VERS"


function DoAlien ()  {
	FILENAME="$PROD"_"$VERS"-0_"$1".deb
	ARCH="$1"
	rm -Rf "$RDIR"
	# Note, debs have a dash after initial version number, RPM an underscore
	if [ "$1" = amd64Qt ]; then
	#	FILENAME="tomboy-ngQt_0.24b-0_amd64.deb"
		ARCH=amd64
	fi
	echo "--- RDIR=$RDIR and building for $1 using $FILENAME ---------"
	alien -r -g -v "$FILENAME"
	# head "$RDIR"/"$RDIR"-2.spec
	sed -i 's#%dir "/"##' "$RDIR"/"$RDIR"-2.spec
	sed -i 's#%dir "/usr/bin/"##' "$RDIR"/"$RDIR"-2.spec
	# head "$RDIR"/"$RDIR"-2.spec
	cd "$RDIR"
	rpmbuild --target "$ARCH" --buildroot "$PWD" -bb "$RDIR"-2.spec
	cd ..
	# if its a Qt one, rename it it so it does not get overwritten subsquently
	if [ "$1" = amd64Qt ]; then
		mv "$RDIR"-2."$ARCH".rpm "$PROD"Qt-"$VERS"-2."$ARCH".rpm
	fi
}

rm -f tom*.rpm
# Must do the "non std" ones first, else have overwrite problems
DoAlien "amd64Qt"
DoAlien "i386"
DoAlien "amd64"
chown "$SUDO_USER" *.rpm
ls -l *.rpm

