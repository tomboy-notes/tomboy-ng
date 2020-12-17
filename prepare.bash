#!/bin/bash
# copyright David Bannon, 2019, 2020, use as you see fit, but retain this statement.
#

# This script takes a tomboy-ng zip file from github, and prepares things to
# build it a source deb or just run buildit.bash to make a tomboy-ng binary"
# Hardwired data in this script are specific to the Deb source build.

# -------------------- Making a Debian SRC ----------------------
# To build a Debian Source Package, on eg a current Bullseye -
# 	Install fpc, lazarus, libnotifier-dev, devscripts and ? libqt6pas-dev
#	Ensure appropriate pgp key in place ($HOME/.gnupg), see below 
#	In an empty dir, following commands -
#	wget https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/prepare.bash
# Unless you are me, you need to edit the above, DEBFULLNAME and DEBEMAIL _OR_ have env set 
#	wget https://github.com/tomboy-notes/tomboy-ng/archive/master.zip
#	mv master.zip tomboy-ng-master.zip
#	bash ./prepare.bash -D bullseye
#	cd tomboy-ng_XXXX                       // Whatever the name is at this stage....
#	debuild -S
#	The files you want are in ../.

# -------------------- Signing Details -------------------------
# Below are hardwired my personal full name and tomboy-ng email address. These
# will only be used if relevent env vars are empty. Note that they
# must match whats available in a gpg key. AND if that does not match
# the Maintainer: entry from control, we get a non maintainer upload warning.
#
# 
# -------------------- Building the Ubuntu PPA kit --------------
# Move both a fresh tomboy-ng-master.zip and this script into a clean
# subdirectory, run the script, change to tomboy-ng.{ver} and run -
#
#      debuilder -us -uc      if thats OK, then repeat with -
#      debuilder -S
#      dput ppa:d-bannon/ppa-tomboy-ng tomboy-ng_0.29e-1_source.changes [enter]
#

# David Bannon, July 2020
# History -
#	2020-09-02 Added -D distro switch

APP="tomboy-ng"
# These are mine, they are used as defaults if NOT set in env. 
DEF_EMAIL="tomboy-ng@bannons.id.au"	# This matches cert I use to sign tomboy-ng stuff
DEF_FULLNAME="David Bannon"		# and this ...
VER="unknown"
LAZ_BLD=""
UFILES="NO"	# debug tool, update Makefile
CLEAN="NO"	# debug tool, remove files from previous run, assume same ver.
WIDGET=""	# empty says make a GTK2, only other possibility is Qt5
QT5INNAME=""	# May have content we add to qt5 package name (when -Q) 

	# Looks for fpc and lazbuild on PATH, if in root space, do nothing,
	# if in user space, because debuild will miss them, makes two files.
function CheckFPC_LAZ () {
	FPC=`which fpc`
	if [ -x "$FPC" ]; then
		PREFIX="${FPC:0:4}"
		if [ "$PREFIX" = "/usr" ]; then
			echo "---------- root space fpc, all good"
		else
			echo "---------- Leaving a fpc file for buildit"
			echo "$FPC" > WHICHFPC
		fi
	else
		echo "----------- ERROR, no fpc found ------------"
		exit 1
	fi
	if [ "$LAZ_BLD" = "" ]; then 	# we had better try to find it		
		LAZ_BLD=`which lazbuild`
	fi
	if [ -x "$LAZ_BLD" ]; then
		PREFIX="${LAZ_BLD:0:4}"
		if [ "$PREFIX" = "/usr" ]; then
			echo "---------- root space Lazarus, all good"
		else
			echo "---------- Leaving a lazbuild file for buildit"
			echo "$LAZ_BLD" > WHICHLAZ
		fi
	else
		echo " --- ERROR, no lazbuild found, try -l ? ---"
		exit 1
	fi
}

	# Here we remove file that are not needed in the Debian SRC kit.
function CleanSource () {
	rm -Rf experimental
	rm -Rf patches
	rm -Rf doc/gallery
	rm -Rf doc/html
	rm -Rf doc/wiki
	rm -Rf po/*.mo
	rm -f  doc/*.svg doc/*.png doc/*.note
	rm -f  glyphs/*.png glyphs/*.ico glyphs/*.svg glyphs/*.icns
	rm -fR glyphs/help
	rm -fR glyphs/demos
	KC="kcontrols"
	KCS="$KC/source"
	rm -fR "$KC"/demos 
	rm -fR "$KC"/help
	rm -Rf "$KC"/packages kcontrols/tools 
	rm -Rf "$KC"/resource_src/khexeditor_icons "$KC"/resource_src/kgrid_icons
	rm "$KCS"/kbuttons.pas "$KCS"/kdbgrids.pas "$KCS"/kgrids.* "$KCS"/kicon.pas 
	rm "$KCS"/klabels.pas "$KCS"/kmemodlg*.* "$KCS"/kxml.pas "$KCS"/kwidewinprocs.pas
	rm "$KCS"/kmemofrm.* "$KCS"/kpagecontrol.* "$KCS"/kprogress.* "$KCS"/ksplitter.pas  
	rm "$KC"/*.txt "$KC"/*.json "$KC"/*.bat
	rm -Rf "$KC"/packages "$KC"/tools "$KC"/resource_src/khexeditor_icons "$KC"/resource_src/kgrid_icons
}


function KControls () {	
	if [ -e "master.zip" ]; then
		echo "Note: reusing KControls zip"
	else
		wget https://github.com/kryslt/KControls/archive/master.zip   # watch this name does not change.
	fi
	unzip -q master.zip
	# rm -f master.zip
	mv KControls-master "$APP"_"$VER""-1"/kcontrols
}

function ShowHelp () {
    echo " "
    echo "Assumes FPC of some sort in path, available and working, ideally 3.2.0."
    echo "Needs Lazarus, <=2.0.6 in root space or specified with -l option."
    echo "Needs devscripts preinstalled and maybe an edit of email address above if"
    echo "it is to be used in the DEB SRC tool chain. Its role there is just to create"
    echo "an initial tarball and working directory (including inserting kcontrols)."
    echo "David Bannon, August 2020" 
    echo "-h   print help message"
    echo "-l   a path to a viable lazbuild, eg at least where lazbuild and lcl is."
    echo "-C   clean out deb files from previous run, debug use only."
    echo "-U   update Makefile and/or buildit.bash,   debug use only."
    echo "-Q   Make a Qt5 version instead of default GTK2"
    echo "-q   Make a Qt5 one for a Debian Repo" 
    echo "-p   Pause before creating .orig. to change content, use another term."
    echo "-D   distro, eg bionic, focal, bullseye"
    echo ""
    exit
}


while getopts "hpqQUCl:D:" opt; do
  case $opt in
    h)
      ShowHelp
      ;;
    l)
	LAZ_BLD="$OPTARG"
	;;
    U)
	UFILES="YES"
	;;
    C)
	CLEAN="YES"
	;;
    p)
	PAUSE="YES"
	;;
    Q)
	WIDGET="Qt5"
	APP="$APP""-qt5"
	QT5INNAME="YES"
	;;
    q)
	WIDGET="Qt5"
	;;
    D)
	DISTRO1="-D""$OPTARG"
	DISTRO2="--force-distribution"
	;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      ShowHelp
      ;;
  esac
done

echo "---------- laz_bld is $LAZ_BLD"
echo "---------- CLEAN is $CLEAN"
echo "---------- UFILES is $UFILES"

rm -f WHICHFPC WHICHLAZ		

if [ -f tomboy-ng-master.zip ]; then
	CheckFPC_LAZ
	# In practise, we should have these env set, my defaults just in case. 
	if [ "$DEBEMAIL" = "" ]; then
		DEBEMAIL="$DEF_EMAIL"
		export DEBEMAIL
	fi
	if [ "$DEBFULLNAME" = "" ]; then
		DEBFULLNAME="$DEF_FULLNAME"
		export DEBFULLNAME
	fi
	unzip -q tomboy-ng-master.zip
	if [ "$UFILES" = "YES" ]; then
		if [ "Makefile" -nt "tomboy-ng-master/Makefile" ]; then
			echo "---------- UPDATING Makefile"
			cp Makefile tomboy-ng-master/Makefile
		fi
		if [ "buildit.bash" -nt "tomboy-ng-master/buildit.bash" ]; then
			echo "---------- UPDATING buildit.bash"
			cp buildit.bash tomboy-ng-master/buildit.bash
		fi
	fi
	VER=`cat tomboy-ng-master/package/version`
	if [ "$CLEAN" = "YES" ]; then
		echo "---------- Removing existing DEB files"
		rm -Rf "$APP"_"$VER""-1"
		rm -f "tomboy-ng_""$VER"".orig.tar.gz"
		rm -f "tomboy-ng_$VER-1_amd64.buildinfo"
		rm -f "tomboy-ng_$VER-1_amd64.changes"
       		rm -f "tomboy-ng_$VER-1_amd64.deb"
 		rm -f "tomboy-ng_$VER-1.debian.tar.xz"
		rm -f "tomboy-ng_$VER-1.dsc"
		rm -f "tomboy-ng_$VER.orig.tar.gz"
	fi
	mv "tomboy-ng-master" "$APP"_"$VER""-1"
	KControls
	cd "$APP"_"$VER""-1"
	CleanSource
	# 966537: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=966537  -- No, this was closed.
	# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=897688
	dch "$DISTRO1" "$DISTRO2" --create --package="$APP" --newversion="$VER""-1" "Initial release. (Closes: #897688)"
	dch --append "Please see github for change details"
	if [ "$WIDGET" = "Qt5" ]; then
		dch --append "Qt5 version"
		if [ "$QT5INNAME" = "YES" ]; then
			cp debian/rules.qt5 debian/rules
			cp debian/control.qt5 debian/control
		else
			cp debian/control.qt5-unnamed debian/control
		fi
		# sed  "s/#REPLACEME_QT5/DESTDIR += -qt5/" Makefile > Makefile.temp
		# mv Makefile.temp Makefile
		touch Qt5
	fi
	dch --release "blar"
	cd ..
	if [ "$PAUSE" = "YES" ]; then
		read -p "Edit things in another term, press Enter."
	fi
	# Next block is to avoid the dreaded lintian no-debian-changes
	# The copyright file in there alone does not generate that error
	mv "$APP"_"$VER""-1"/debian ./debian
	mkdir "$APP"_"$VER""-1"/debian
	cp debian/copyright "$APP"_"$VER""-1"/debian/.
	tar czf "$APP"_"$VER".orig.tar.gz "$APP"_"$VER""-1"
	# OK, we have our .orig. file, put most of it back.
	rm -Rf "$APP"_"$VER""-1"/debian
	rm debian/control.qt5-DEBIAN debian/control.qt5
	rm debian/rules.qt5
	mv ./debian "$APP"_"$VER""-1"/. 

	echo "If no errrors, you should now cd ""$APP"_"$VER""-1; debuild -us -uc"
else
	echo ""
	echo "   Sorry, I cannot see a tomboy-ng-master.zip file. This"
	echo "   script must be run in a directory containing that file"
	echo "   (obtained from github) and probably little else."
	echo "   If you used wget to download tomboy-ng, it will be named master.zip,"
	echo "   you should rename it tomboy-ng-master.zip to avoid confusion."
	echo ""
fi



