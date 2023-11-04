#!/bin/bash

# A script to build tomboy and make deb packages and zip up the other binaries
# see https://www.debian.org/doc/manuals/debian-faq/ch-pkg_basics
# we can also add preinst, postinst, prerm, and postrm scripts if required
# David Bannon, November, 2017
# Assumes a working FPC/Lazarus install with cross compile tools as described in
# http://wiki.lazarus.freepascal.org/Cross_compiling_for_Win32_under_Linux and
# http://wiki.lazarus.freepascal.org/Cross_compiling
# and that a 'Release' mode exists.

# ----------------------------------------------------------------------------
# Typical usage -
#          ./package_debian.sh $HOME"/lazarus/laz-200 <LeakCheck>

# Note we assume laz config has same name as Laz directory, ie .laz-200
# ----------------------------------------------------------------------------

PRODUCT="tomboy-ng"
VERSION=`cat version`

SOURCE_DIR="../source"
ICON_DIR="../glyphs"

WHOAMI="David Bannon <tomboy-ng@bannons.id.au>"
MANUALS_DIR="BUILD/usr/share/doc/$PRODUCT/"
MANUALS=`cat note-files`

BUILDOPTS=" -B --quiet --quiet"
# BUILDOPTS=" -B --verbose"
BUILDDATE=`date -R`
LPI="Tomboy_NG.lpi"
LAZ_FULL_DIR="$1"
LAZ_DIR=`basename "$LAZ_FULL_DIR"`
WIN_DIR=WinPre_"$VERSION"
LEAKCHECK="NO"

if [ -z "$LAZ_DIR" ]; then
	echo "Usage : $0 /Full/Path/Lazarus/dir"
	echo "eg    : $0 /home/dbannon/bin/Lazarus/laz_2_2_rc2"
	echo "or"
	echo "      : $0 clean"
	echo "      : $0 qt6only      // to package an existing binary in ../source"
	echo "      : $0 raspionly    // to package an existing binary in ../source"
	exit
fi

if [ "$2" == "LeakCheck" ]; then
	LEAKCHECK="YES"
fi


if [ $1 == "clean" ]; then
	rm  -f *.deb
	rm  -f *.tgz
	rm  -f *.rpm
	rm -Rf BUILD
	rm -Rf WinPre*
	exit
fi

# ----------------------


function LookForBinary () {
	cd "$SOURCE_DIR"
	if [ -a "$1" ]; then
		echo "Binary $1 was made"
	else	
		echo "---------- ERROR $1 was not made"
	fi
	cd "../package"
}

function ModeParamArch () { # expects to be called like   ARCH=$(ModeParamArch ReleaseLin64)
    case $1 in              # Only useful in debian packaging
        ReleaseLin64)
            echo "amd64"
        ;;
        ReleaseLin32)
            echo "i386"
        ;;
        ReleaseQT5)
            echo "amd64Qt"
        ;;
        ReleaseRasPi)
            echo "armhf"
        ;;
	ReleaseQT6)
	    echo "amd64Qt6"
	;;
    esac
}


function ModeParamBin () { # expects to be called like   BIN=$(ModeParam ReleaseWin64)
    case $1 in
        ReleaseLin64)
            echo "$PRODUCT"-64
        ;;
        ReleaseLin32)
            echo "$PRODUCT"-32
        ;;
        ReleaseWin32)
            echo "$PRODUCT"-32.exe
        ;;
        ReleaseWin64)
            echo "$PRODUCT"-64.exe
        ;;
        ReleaseQT5)
            echo "$PRODUCT"-qt5
        ;;
        ReleaseQT6)
            echo "$PRODUCT"-qt6
        ;;        
        ReleaseRasPi)
            echo "$PRODUCT"-armhf
        ;;
    esac
}

# Modes (as defined in IDE) ReleaseLin64 ReleaseLin32 ReleaseWin64 ReleaseWin32 ReleaseRasPi ReleaseQT5

function BuildAMode () {
    echo "------------- Building Mode $1 --------"
    cd ../source
    BIN=$(ModeParamBin "$1")
    rm -f "$BIN"
    #CMD="TOMBOY_NG_VER=$VERSION $LAZ_FULL_DIR/lazbuild $BUILDOPTS $LAZ_CONFIG --build-mode=$1 $LPI"
    #echo "CMD is $CMD"
    TOMBOY_NG_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS $LAZ_CONFIG --build-mode="$1" "$LPI"
    if [ ! -f "$BIN" ]; then
	    echo "----- $1 ERROR failed to build $BIN ---------"
	    echo "$LAZ_FULL_DIR/lazbuild $BUILDOPTS $LAZ_CONFIG --build-mode=$1 $LPI"
	   exit
    fi	 
    cd ../package
}




function DebianTemplate () {        # the common to all versions things
	# We build a debian tree in BUILD and call dpkg-deb -b 
	#  BUILD/DEBIAN control,debian-binary and any scripts
	rm -rf BUILD
	mkdir -p BUILD/DEBIAN
	mkdir -p BUILD/usr/bin
	mkdir -p BUILD/usr/share/"$PRODUCT"
	for i in 16x16 22x22 24x24 32x32 48x48 256x256; do
		mkdir -p "BUILD/usr/share/icons/hicolor/$i/apps";
		cp "$ICON_DIR/$i.png" "BUILD/usr/share/icons/hicolor/$i/apps/$PRODUCT.png";
	done;
	mkdir -p BUILD/usr/share/doc/$PRODUCT
	cp ../doc/authors BUILD/usr/share/doc/$PRODUCT/.
	cp -R ../doc/HELP BUILD/usr/share/"$PRODUCT"/.
	# -------------- Translation Files
	# we end up with, eg, /usr/share/locale/es/LC_MESSAGES/tomboy-ng.mo
	# and /usr/share/locale/es/LC_MESSAGES/lclstrconsts.mo for Linux 
	mkdir -p BUILD/usr/share/locale
	for i in `ls -b ../po/*.??.po`; do		# Deal with each country code in turn
	        # echo "Name is $i"
	        BASENAME=`basename -s.po "$i"`
	        # echo "BASENAME is $BASENAME"
	        CCODE=`echo "$BASENAME" | cut -d '.' -f2`
	        # echo "CCode is $CCODE"
	        mkdir -p BUILD/usr/share/locale/"$CCODE"/LC_MESSAGES
	        BASENAME=`basename -s."$CCODE" "$BASENAME"`
		msgfmt -o BUILD/usr/share/locale/"$CCODE"/LC_MESSAGES/"$BASENAME".mo "$i"
		msgfmt -o BUILD/usr/share/locale/"$CCODE"/LC_MESSAGES/lclstrconsts.mo "$LAZ_FULL_DIR"/lcl/languages/lclstrconsts."$CCODE".po
	done
	mkdir BUILD/usr/share/applications
	cp "$ICON_DIR/$PRODUCT.desktop" BUILD/usr/share/applications/.
	mkdir -p BUILD/usr/share/man/man1
	gzip -9kn ../doc/$PRODUCT.1
	mv ../doc/$PRODUCT.1.gz BUILD/usr/share/man/man1/.
	cp ../debian/copyright BUILD/usr/share/doc/"$PRODUCT"/.
}

	# gets called with the Lazarus Build Mode name for each one we are packaging ...
function DebianPackage () {
    rm -Rf BUILD
    DebianTemplate
    ARCH=$(ModeParamArch "$1")
    BIN=$(ModeParamBin "$1")
    CTRL_ARCH=$ARCH
	CTRL_DEPENDS="libgtk2.0-0 (>= 2.6), libc6 (>= 2.14), libcanberra-gtk-module, wmctrl, libnotify-bin"
	CTRL_RELEASE="GTK2 release."
	cp $SOURCE_DIR/$BIN BUILD/usr/bin/$PRODUCT
	# ----------- Some Special Cases ----------------
	case "$1" in
	"ReleaseQT5")
		echo "++++++++++ Setting QT5 +++++++++"
		CTRL_ARCH="amd64"
		CTRL_DEPENDS="libqt5pas1 (>= 2.15), libc6 (>= 2.14), wmctrl, libnotify-bin, qt5ct"
		CTRL_RELEASE="Qt5 release."
		# we must force qt5 app to use qt5ct because of a bug in qt5.tsavedialog
	    # note ugly syntax, qt5 strips it off (and anything after it) before app sees it.
	    sed -i "s/Exec=tomboy-ng %f/Exec=env QT_QPA_PLATFORMTHEME=qt5ct tomboy-ng %f/" BUILD/usr/share/applications/"$PRODUCT".desktop 
	    #sed -i "s/Exec=tomboy-ng %f/Exec=tomboy-ng %f --platformtheme qt5ct/" BUILD/usr/share/applications/"$PRODUCT".desktop
		;;
	"ReleaseQT6")
		echo "++++++++++ Setting QT6 +++++++++"
		CTRL_ARCH="amd64"
		CTRL_DEPENDS="libqt6pas6, libc6 (>= 2.34), wmctrl, libnotify-bin, libqt6pas6 (>= 6.2.7)"
		CTRL_RELEASE="Qt6 release."
		# we must force qt6 app to use qt6ct because of a bug in qt6.tsavedialog
	    # note ugly syntax, qt6 strips it off (and anything after it) before app sees it. 
	    sed -i "s/Exec=tomboy-ng %f/Exec=env QT_QPA_PLATFORMTHEME=qt6ct tomboy-ng %f/" BUILD/usr/share/applications/"$PRODUCT".desktop	
		;;		
	"ReleaseRasPi")
		CTRL_RELEASE="Raspberry Pi release."
		CTRL_DEPENDS="libqt5pas1, libc6 (>= 2.14), wmctrl, libnotify-bin"
		
		;;
	esac
	chmod 755 BUILD/usr/bin/tomboy-ng
	# -------------------- Changelog -----------------
	cp ../debian/changelog "$MANUALS_DIR"changelog
	DEBEMAIL="David Bannon <tomboy-ng@bannons.id.au>" dch --changelog "$MANUALS_DIR"changelog -v "$VERSION" -D unstable --force-distribution "Release of new version"    
	if [ -f ../whatsnew ]; then
		echo "----------- Including whatsnew in changelog"
		while IFS= read -r Line; do
			DEBEMAIL="David Bannon <tomboy-ng@bannons.id.au>" dch --changelog "$MANUALS_DIR"changelog --append "$Line"
		done < ../whatsnew
	fi
	DEBEMAIL="David Bannon <tomboy-ng@bannons.id.au>" dch --changelog "$MANUALS_DIR"changelog --append "Please see github for change details."
	gzip -9n "$MANUALS_DIR"changelog
    	# -------------------------------- Make control file -------------------------
	echo "Package: $PRODUCT" > BUILD/DEBIAN/control
	echo "Version: $VERSION" >> BUILD/DEBIAN/control
	echo "Architecture: $CTRL_ARCH" >> BUILD/DEBIAN/control
	echo "Maintainer: $WHOAMI" >> BUILD/DEBIAN/control
	# -------------------------------- Calculate size, thanks circular@LazForum
	SIZE_IN_KB="$(du -s BUILD | awk '{print $1;}')"
	echo "Installed-Size: ${SIZE_IN_KB}" >> "BUILD/DEBIAN/control"
	echo "Depends: $CTRL_DEPENDS" >> BUILD/DEBIAN/control
	echo "Priority: optional" >> BUILD/DEBIAN/control
	echo "Homepage: https://github.com/tomboy-notes/tomboy-ng/wiki" >> BUILD/DEBIAN/control
	#echo "Homepage: https://wiki.gnome.org/Apps/Tomboy" >> BUILD/DEBIAN/control
	echo "Section: x11" >> BUILD/DEBIAN/control
	echo "Description: Tomboy Notes rewritten to make installation and cross platform easier." >> BUILD/DEBIAN/control
	echo " $CTRL_RELEASE" >> BUILD/DEBIAN/control
	echo " Please report your experiences." >> BUILD/DEBIAN/control
	
	chmod -R g-w BUILD
  	fakeroot dpkg-deb -b BUILD/. "$PRODUCT""_$VERSION-0_"$ARCH".deb"
	# --------------------------------- Clean up -----------
#	rm -Rf BUILD
}

function WriteZipReadMe () {
	RM="$1/readme.txt"
	echo "This is a tar ball of $PRODUCT $VERSION for Linux. Use this if you cannot use" > "$RM"
	echo "either the deb or rpm on your particular distribution. It contains some of the" >> "$RM"
	echo "files you need and a very basic installer but does not resolve dependancies." >> "$RM"
	echo "Its assumed you know what you are doing." >> "$RM"
	echo "Files and features not provided here include -" >> "$RM"
	echo "* Language other than English" >> "$RM"
	echo "* tomboy-ng help files" >> "$RM"
	echo "* Ability to have tomboy-ng set itself to autostart" >> "$RM"
	echo "Dependencies include libgtk2.0-0, libcanberra-gtk-module, libnotify, wmctrl." >> "$RM"
	echo "  or, in the Qt5 version, libqt5pas1, libnotify, wmctrl" >> $RM
	echo "If you need help, please post specific question to tomboy-ng github issues." >> "$RM"
}



function DoGZipping {
	BIN=$(ModeParamBin "$1")
	ARCH=$(ModeParamArch "$1")
        GZIP_DIR="$PRODUCT"-"$VERSION"
#	rm -f *.tgz
#	for TBVer in tomboy-ng32 tomboy-ng; do
	rm -Rf "$GZIP_DIR"	
	mkdir "$GZIP_DIR"
	cp "$SOURCE_DIR"/"$BIN" "$GZIP_DIR"/"$PRODUCT"
	for i in 16x16 22x22 24x24 32x32 48x48 256x256; do
		cp "$ICON_DIR/$i.png" "$GZIP_DIR/$i.png"
	done;
	cp "$ICON_DIR/install-local.bash" "$GZIP_DIR/install-local.bash"
	cp "$ICON_DIR/$PRODUCT.desktop" "$GZIP_DIR/$PRODUCT.desktop"
	gzip -9kn ../doc/$PRODUCT.1
	mv ../doc/$PRODUCT.1.gz "$GZIP_DIR"/.
	WriteZipReadMe "$GZIP_DIR"
	tar czf "$PRODUCT"-"$VERSION"-"$ARCH".tgz "$GZIP_DIR"
	rm -Rf "$GZIP_DIR"	
}

function MkWinPreInstaller() {
	# Make a dir containing everything we need to make a 32/64bit Inno Setup installer for Windows
	rm -Rf "$WIN_DIR"
	mkdir "$WIN_DIR"
	cp "$SOURCE_DIR"/tomboy-ng-64.exe "$WIN_DIR"/tomboy-ng64.exe
	cp "$SOURCE_DIR"/tomboy-ng-32.exe "$WIN_DIR"/tomboy-ng32.exe
	# cp ../../DLL/* "$WIN_DIR"/.
	cp ../../DLL/libhunspell.dll "$WIN_DIR/."
	cp ../../DLL/libhunspell.license "$WIN_DIR/."
	cp ../COPYING "$WIN_DIR/."
	cp AfterInstall.txt "$WIN_DIR/."
	sed "s/MyAppVersion \"REPLACEME\"/MyAppVersion \"$VERSION\"/" tomboy-ng.iss > "$WIN_DIR/tomboy-ng.iss.temp"
	# mkdir -p "$WIN_DIR/HELP/EN"
	# mkdir -p "$WIN_DIR/HELP/ES"
	# for i in $MANUALS; do
	#    cp ../doc/$i "$WIN_DIR/."
	# done;
	mkdir "$WIN_DIR/HELP_DIR"
	cp -R ../doc/HELP "$WIN_DIR/HELP_DIR/."
	# " -------- WRITE mo files --------"
	msgfmt -o "$WIN_DIR"/"$PRODUCT".mo ../po/"$PRODUCT".po
	# Source: "tomboy-ng.mo";     DestDir: "{app}\locale"; Flags: ignoreversion
	# echo "Source: \""$PRODUCT".mo\";     DestDir: \"{app}\\locale\"; Flags: ignoreversion" > mo.insert
	# above line commented Feb 2022, don't need tomboy-ng.mo in any package 
	for i in `ls -b ../po/*.??.po`; do
            # echo "Name is $i"
            BASENAME=`basename -s.po "$i"`
            CCODE=`echo "$BASENAME" | cut -d '.' -f2`
            # echo "CCode is $CCODE"
            BASENAME=`basename -s."$CCODE" "$BASENAME"`
	    msgfmt -o "$WIN_DIR"/"$BASENAME"."$CCODE".mo "$i"
	    msgfmt -o "$WIN_DIR"/lclstrconsts."$CCODE".mo "$LAZ_FULL_DIR"/lcl/languages/lclstrconsts."$CCODE".po
	    echo "Source: \""$BASENAME"."$CCODE".mo\";     DestDir: \"{app}\\locale\"; Flags: ignoreversion" >> mo.insert
	    echo "Source: \"lclstrconsts."$CCODE".mo\";     DestDir: \"{app}\\locale\"; Flags: ignoreversion" >> mo.insert
	done
	sed '/PUTMOLINESHERE/r mo.insert' "$WIN_DIR"/tomboy-ng.iss.temp > "$WIN_DIR"/tomboy-ng.iss
	MANWIDTH=70 man -l ../doc/tomboy-ng.1 > "$WIN_DIR/readmeUNIX.txt"
	awk 'sub("$", "\r")' "$WIN_DIR/readmeUNIX.txt" > "$WIN_DIR/readme.txt"
	rm "$WIN_DIR/readmeUNIX.txt"
	# unix2dos -q "$WIN_DIR/readme.txt"    Hmm, unix2dos seems to have dissapeared !
	echo "----------- Windows installer dir created -----------"
	rm mo.insert
	# ls -la "$WIN_DIR"
}


if [ $1 == "qt6only" ]; then	# this is wrong ? cannot find laz provided language files ?
    DebianPackage ReleaseQT6
    exit;
fi

if [ $1 == "raspionly" ]; then
    DebianPackage ReleaseRasPi
    exit;
fi

	# ------- OK, lets find Laz Config ---------------------------------

# It all starts here
if [ -f "$LAZ_FULL_DIR"/lazarus.cfg ]; then
	# Assume if we have a cfg, it specifies pcp ?? Will fail otherwise
    	LAZ_CONFIG=`grep -i pcp "$LAZ_FULL_DIR"/lazarus.cfg`
else
if [ -d "$HOME/.Laz_$LAZ_DIR" ]; then     # try my way of naming config first
    LAZ_CONFIG="$HOME/.Laz_$LAZ_DIR";
else
    echo "------ Testing for the .Laz config $HOME------"
    if [ -d "$HOME/.$LAZ_DIR" ]; then
        LAZ_CONFIG="$HOME/.$LAZ_DIR";
        fi
    fi
fi

if [ -z "$LAZ_CONFIG" ]; then
    echo "--------- ERROR, dont have a Laz Config -------"
    exit
fi

echo "-----  LAZ_CONFIG is $LAZ_CONFIG ------"

# Note: because we must build Qt6 on later that U20.04 and for all others, we must build on U20.04
# due to the libc issue, we cannot build our qt6 one all at the same time.

for BIN in ReleaseLin64 ReleaseLin32 ReleaseWin64 ReleaseWin32 ReleaseRasPi ReleaseQT5; # ReleaseQt6; Note yet !  Must build elsewhere, bring biary here.
	do BuildAMode $BIN; 
done

#if [ "$2" == "LeakCheck" ]; then




rm tom*.deb
for BIN in ReleaseLin64 ReleaseLin32 ReleaseRasPi ReleaseQT5 ReleaseQT6; # this expects we have put a prepared qt6 binary in source dir.
	do DebianPackage $BIN ; 
done

rm tom*.tgz
for MODE in ReleaseLin64 ReleaseLin32 ;
	do DoGZipping $MODE;
done	

MkWinPreInstaller
# ls -ltr
fakeroot bash ./mk_rpm.sh
# echo "OK, if that looks OK, run   fakeroot bash ./mk_rpm.sh"
# Dont sign under fakeroot, its messy
echo "OK, we will now sign the RPMs - david, use the longer passphrase !"
for i in `ls -b *.rpm`; do rpm --addsign "$i"; echo "Signed $i"; done
ls -l *.rpm *.deb "$WIN_DIR"/*.exe


