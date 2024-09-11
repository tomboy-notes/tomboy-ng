#!/usr/bin/bash
set -e			# exit on error
#
# Requires linuxdeploy and its qt plugin https://github.com/linuxdeploy/linuxdeploy
# Requires qmake and/or qmake6. linuxdeploy and its qt plugin must be in PATH.
# https://github.com/linuxdeploy
#
WIDGET="Qt6"		# Default, also supports Qt5 (could do gtk2 I suspect)
APPDIR="../AppDir"
# LDEPLOY="$HOME/Downloads/linuxdeploy-x86_64.AppImage"
LDEPLOY="default"
QMAKE6="/usr/bin/qmake6"
SYS_ARCH=`uname -m`		#  eg x86_64, armv71, i686, ...

function ShowHelp () {
	echo "A script to build tomboy-ng AppImages, requires linuxdeploy and qt plugin."
	echo "For Qt requires appropriate libQtxPas-Dev (and all its depencies)"
	echo "Run from dir containing tomboy-ng's Makefile"
	echo "-w Qt5|Qt6  use that widget, default is Qt6"
	echo "-h          show this help and exit"
	echo ""
	exit
}

while getopts "w:h" OPT; do
	case ${OPT} in
		w) WIDGET="${OPTARG}"
			;;
		h) ShowHelp			# does not return
			;;
	esac
done

if [ "$WIDGET" != "Qt6" ]; then
	QMAKE6="/usr/bin/qmake"				# must be Qt5 or nothing
	if [ "$WIDGET" != "Qt5" ]; then
		echo "Sorry, $WIDGET is not a valid widget choice, exiting..."
		exit
	fi
fi


case "${SYS_ARCH}" in
	x86_64) LDEPLOY="linuxdeploy-x86_64.AppImage"
		;;
	i686)   LDEPLOY="linuxdeploy-i386.AppImage"
		;;
	armv71) LDEPLOY="linuxdeploy-armhf"
		;;
esac	

if [ "$LDEPLOY" == "default" ]; then
	echo "Sorry, don't support your arch $SYS_ARCH exiting"
	exit
fi

# leave a semophore file for buildit so it knows which widget set to use. 
if [ "$WIDGET" == "Qt6" ]; then
	if [ -f "Qt5" ]; then
		rm Qt5
	fi
	touch Qt6
else
	if [ -f "Qt6" ]; then
		rm Qt6
	fi
	touch Qt5
fi

# And a semaphore to ensure we don't apply Hardening, has a problem in older OS.
touch NOHARDENING

if [ "$APPDIR" == "" ]; then	# OK, so I am a coward !
	echo "ERROR, var APPDIR is empty, disaster is on your door step, exiting.."
	exit
fi

# ----------- O K   L O O K S   L I K E   I T   M I G H T   W O R K ----------
#
rm -Rf "$APPDIR"
make PREFIX="$APPDIR/usr" appimage		# Will make binary (using buildit) and then 'install' it
if [ ! -e source/tomboy-ng ]; then
	echo "ERROR, failed to build binary"
	exit
fi

# insert script that provides the Qt env var so linuxdeploy does not add symlink
# Maybe, just maybe this is not necessary with Qt6 and qt plugin ?
cp scripts/AppRun "$APPDIR"/AppRun

# remove that env var from desktop files's Exec line before linuxdeploy sees it.
# Exec=env QT_QPA_PLATFORM=xcb tomboy-ng %f
sed "s/env QT_QPA_PLATFORM=xcb //" "$APPDIR"/usr/share/applications/tomboy-ng.desktop > "$APPDIR"/usr/share/applications/tomboy-ng.desktop-new

# sed "s/env QT_QPA_PLATFORM=xcb //" glyphs/tomboy-ng.desktop > glyphs/tomboy-ng.desktop-new
# mv glyphs/tomboy-ng.desktop-new glyphs/tomboy-ng.desktop 
mv "$APPDIR"/usr/share/applications/tomboy-ng.desktop-new "$APPDIR"/usr/share/applications/tomboy-ng.desktop 

# QMAKE=/usr/bin/qmake6  "$LDEPLOY" --appdir "$APPDIR" --plugin qt --output appimage
QMAKE="$QMAKE6"  "$LDEPLOY" --appdir "$APPDIR" --plugin qt --output appimage

ls -la *.AppImage
