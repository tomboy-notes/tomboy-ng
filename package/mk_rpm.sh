#!/usr/bin/bash

# ====================================================
# a short script to make RPMs from our tomboy-ng debs
# does more than just call alien as that seems to end up
# with commands to create / and /usr/bin that upset yum
#
# This script must be run as fake root, so useage is -
# fakeroot bash mk_rpm
#
# History
# 2020/03/10 
# Finally worked out why yum won't work, while the rpm
# command is happy with me calling the arch i386, amd64 (debian speak)
# yum insists on them being x86, x86_64.
# Manually add wmctrl to dependencies.
# 2021/05/15
# don't add gnome-shell-extension-appindicator to dependencies,
# it pulls in half of Gnome desktop, non gnome users would hate me.
# ====================================================

PROD=tomboy-ng
VERS=`cat version`
RDIR="$PROD"-"$VERS"

PACKVER=1	# Starts at '1', rev it if we are repackaging same content

function DoAlien ()  {
	FILENAME="$PROD"_"$VERS"-0_"$1".deb     # this is the input deb file
	ARCH="$1"
	rm -Rf "$RDIR"
	# Note, debs have a dash after initial version number, RPM an underscore
	if [ "$1" = amd64Qt ]; then
	#	FILENAME="tomboy-ngQt_0.24b-0_amd64.deb"
		ARCH=x86_64
	fi
	if [ "$1" = amd64 ]; then
		ARCH=x86_64
	fi
	if [ "$1" = i386 ]; then
		ARCH=i686          # that is used in the output RPM
	fi

	echo "--- RDIR=$RDIR and building for $1 using $FILENAME ---------"
	alien -r -g -v -k "$FILENAME"
	# Alien inserts requests the package create / and /usr/bin and
	# the os does not apprieciate that, not surprisingly.
	# This removes the %dir / 
	sed -i "s/^Release:.*/Release: $PACKVER/" "$RDIR"/"$RDIR"-"$PACKVER".spec  
	sed -i 's#%dir "/"##' "$RDIR"/"$RDIR"-"$PACKVER".spec
	# and this removes %dir /usr/bin
	sed -i 's#%dir "/usr/bin/"##' "$RDIR"/"$RDIR"-"$PACKVER".spec
	sed -i 's#%dir "/usr/"##' "$RDIR"/"$RDIR"-"$PACKVER".spec
	sed -i 's#%dir "/usr/share/"##' "$RDIR"/"$RDIR"-"$PACKVER".spec
	sed -i 's#%dir "/usr/share/doc/"##' "$RDIR"/"$RDIR"-"$PACKVER".spec
	sed -i 's#%dir "/usr/share/icons/"##' "$RDIR"/"$RDIR"-"$PACKVER".spec
	sed -i 's#%dir "/usr/share/man/"##' "$RDIR"/"$RDIR"-"$PACKVER".spec
	sed -i 's#%dir "/usr/share/man/man1/"##' "$RDIR"/"$RDIR"-"$PACKVER".spec
	
	sed -i '10i Packager: David Bannon <tomboy-ng@bannons.id.au>' "$RDIR"/"$RDIR"-"$PACKVER".spec
	sed -i '11i URL: https://githup.com/tomboy/tomboy-ng' "$RDIR"/"$RDIR"-"$PACKVER".spec

	gunzip "$RDIR"/usr/share/man/man1/tomboy-ng.1.gz
	bzip2  "$RDIR"/usr/share/man/man1/tomboy-ng.1
	sed -i "s/tomboy-ng.1.gz/tomboy-ng.1.bz2/" "$RDIR"/"$RDIR"-"$PACKVER".spec

	echo "%changelog" >> "$RDIR"/"$RDIR"-"$PACKVER".spec
	CHDATE=`date +"%a %b %d %Y"`
	CHDATE="* $CHDATE David Bannon <tomboy-ng@bannons.id.au> $VERS"-"$PACKVER"
	echo "$CHDATE" >> "$RDIR"/"$RDIR"-"$PACKVER".spec
	echo "- Release of package" >> "$RDIR"/"$RDIR"-"$PACKVER".spec
	while read -r line; do
		echo -e "- $line" >> "$RDIR"/"$RDIR"-"$PACKVER".spec
	done <"../whatsnew"

	# rpmbuild detects the dependencies but it misses wmctrl due to way its used.
	# So we add it to the spec file manually, insert as line 5.
	sed -i '5i Requires: wmctrl ' "$RDIR"/"$RDIR"-"$PACKVER".spec
	# cp -r "$RDIR" "$RDIR"-"$1"
	cd "$RDIR"
	

	cp "$RDIR"-"$PACKVER".spec ../../"$RDIR"-"$PACKVER".spec-"$1"
	rpmbuild --target "$ARCH" --buildroot "$PWD" -bb "$RDIR"-"$PACKVER".spec
	cd ..
	# if its a Qt one, rename it so it does not get overwritten subsquently
	if [ "$1" = amd64Qt ]; then
		mv "$RDIR"-"$PACKVER"."$ARCH".rpm "$PROD"Qt-"$VERS"-"$PACKVER"."$ARCH".rpm
	fi
}

rm -f tom*.rpm
# Must do the "non std" ones first, else have overwrite problems
DoAlien "amd64Qt"
DoAlien "i386"
DoAlien "amd64"
chown "$SUDO_USER" *.rpm
#echo "OK, we will now sign - david, use the longer passphrase !"
#for i in `ls -b *.rpm`; do rpm --addsign "$i"; echo "Signed $i"; done
ls -l *.rpm

