#!/usr/bin/bash
set -e

TESTDIR=`pwd`
TESTDIR="$TESTDIR""-TEST"

if [ -d "$TESTDIR" ]; then
	rm -Rf "$TESTDIR"
fi
mkdir "$TESTDIR"
cp *.xz *.gz *.dsc "$TESTDIR"/.
cd "$TESTDIR"
dpkg-source -x *.dsc
SRCDIRNAME=`find . -maxdepth 1 -type d -name "tomboy*" `
cd "$SRCDIRNAME"
dpkg-buildpackage -us -uc
cd ..
echo "------ Calling lintian -IiE *.changes -------"
lintian -IiE *.changes
pwd
ls -l
