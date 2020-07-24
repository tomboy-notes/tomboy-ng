#!/bin/bash
# copyright David Bannon, 2019, 2020, use as you see fit, but retain this statement.
#

# This script takes a master.zip file from github, tomboy-ng
# and prepares things to build it into source deb
# Move both a fresh master.zip and this script into a clean
# subdirectory, run the script, change to tomboy-ng.{ver}
# and run debuilder -us -uc

# David Bannon, July 2020

if [ -f tomboy-ng-master.zip ]; then
	APP="tomboy-ng"
	MYEMAIL="tomboy-ng@bannons.id.au"
	unzip -q tomboy-ng-master.zip
	VER=`cat "$APP"-master/package/version`
	mv "$APP-master" "$APP"_"$VER"
	cd "$APP"_"$VER"
	DEBEMAIL="$MYEMAIL" dch --create --package=tomboy-ng --newversion="$VER" "blar"
	dch --release "blar"
	cd ..
	tar czf "$APP"_"$VER".orig.tar.gz "$APP"_"$VER"
	which fpc > WHICHFPC
	echo "If no errrors, you should now cd ""$APP"_"$VER; debuild -us -uc"
else
	echo ""
	echo "   Sorry, I cannot see a tomboy-ng-master.zip file. This"
	echo "   script must be run in a directory containing that file"
	echo "   (obtained from github) and probably nothing else."
	echo ""
fi



