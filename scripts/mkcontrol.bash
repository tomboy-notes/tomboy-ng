#!/usr/bin/bash

# A small script to replace critical parameters in control.template
# With every target release/distro requiring a different control
# file, this is an attempt to concentrate all the variation into
# one place.
# copyright David Bannon tomboy-ng@bannons.id.au
# License Do what you want but mention the copyright above.

WDIR="../"
PACKAGE="tomboy-ng"         # note, in debian QT5 its still called tomboy-ng
FPCVER=">=3.2.2"
LAZVER=">=2.2.2"
DEBHVER="=13"
STDVER="4.6.1.0"
DESC="This is the GTK2 based version."
DEPENDS="libgtk2.0-0 (>= 2.6), libcanberra-gtk-module"
BUILDDEPENDS="libgtk2.0-dev"

function AdjustValues () {
    if [ "$2" == "qt5" ]; then          # Note : we ignore anything else
        DESC="This is the QT5 based version."   
        DEPENDS="libqt5pas1"
        BUILDDEPENDS="libqt5pas-dev, lcl-qt5, libcairo2-dev, libpango1.0-dev" 
        PACKAGE="tomboy-ng-qt5"         # but reverse that in Debian (don't do yet anyway)     
    fi
    case $1 in
        "bionic")
            FPCVER=">=3.2.0"
            LAZVER=">=2.0.10c"
            DEBHVER="=11"
            STDVER="4.5.0"
            ;;
        "focal")
            DEBHVER="=12"
            STDVER="4.5.0"
            ;;
        "unstable")                     # ie Debian, as set above
            PACKAGE="tomboy-ng"         # in case its a Debian QT5 build, always tomboy-ng in Debian
            ;;
    esac
}

function ReplaceToken () {
    sed "s/$1/$2/" "$WDIR"control > "$WDIR"control.new
    mv "$WDIR"control.new "$WDIR"control
}

# ------ It starts here --------------
# $1=dir to work in  $2=Target Distro  [$3=qt5]
if [ -f "$1""control.template" ]; then
    if [ "$2" != "" ]; then
        if [ "$2" != "help" ]; then
            if [ "$2" != "-h" ]; then
                WDIR="$1"
                cp "$WDIR""control.template" "$WDIR"control
                AdjustValues $2 $3
                ReplaceToken "%PACKAGE%" "$PACKAGE"
                ReplaceToken "%FPCVER%"  "$FPCVER"
                ReplaceToken "%LAZVER%"  "$LAZVER"
                ReplaceToken "%DEBHVER%" "$DEBHVER"
                ReplaceToken "%STDVER%"  "$STDVER"
                ReplaceToken "%DESC%"    "$DESC"
                ReplaceToken "%DEPENDS%"  "$DEPENDS"
                ReplaceToken "%BUILD-DEPENDS%"    "$BUILDDEPENDS"
                exit
            fi
        fi
    fi
else
    echo "ERROR - $0 did not find ""$1""control.template"
fi
echo "Usage : $0 Valid_Working_Dir [bionic focal unstable] [qt5]"
echo "   eg : $0  ../  focal" 


#   sed "s/(= \${binary:Version}/(>= 3.2.2/g" "$CFILE" > "$CFILE.temp"
#   rm "$CFILE"
#   mv "$CFILE.temp" "$CFILE"




