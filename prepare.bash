#!/bin/bash

# This script takes a master.zip file from github, tomboy-ng
# and prepares things to build it into source deb
# Move both a fresh master.zip and this script into a clean
# subdirectory, run the script, change to tomboy-ng.{ver}
# and run debuilder -us -uc

# David Bannon, July 2020

APP="tomboy-ng"
MYEMAIL="tomboy-ng@bannons.id.au"
unzip -q master.zip
VER=`cat "$APP"-master/package/version`
mv "$APP-master" "$APP"_"$VER"
cd "$APP"_"$VER"
DEBEMAIL="$MYEMAIL" dch --create --package=tomboy-ng --newversion="$VER" "blar"
dch --release "blar"
cd ..
tar czf "$APP"_"$VER".orig.tar.gz "$APP"_"$VER"




