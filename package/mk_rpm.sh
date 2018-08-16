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
	rm -Rf "$RDIR"
	# ls -l
	# Note, debs have a dash after initial version number, RPM an underscore
	alien -r -g -v "$PROD"_"$VERS"-"$1"
	head "$RDIR"/"$RDIR"-2.spec
	sed -i 's#%dir "/"##' "$RDIR"/"$RDIR"-2.spec
	sed -i 's#%dir "/usr/bin/"##' "$RDIR"/"$RDIR"-2.spec
	head "$RDIR"/"$RDIR"-2.spec
	cd "$RDIR"
	sudo rpmbuild --buildroot "$PWD" -bb "$RDIR"-2.spec
	cd ..
	ls -la *.rpm
}

DoAlien "0_amd64.deb"
#DoAlien "0_i386.deb"


