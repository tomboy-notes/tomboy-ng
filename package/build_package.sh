#!/bin/sh
#
# This script generates packages for the Virtual Magnifying Glass
#

##################################
# Constants
##################################

PRODUCT="Virtual Magnifying Glass"
VERSION="3.6"
OS="linux"

TARGET_DIR="./magnifier-$OS-$VERSION/"
TARGET_TAR="magnifier-$OS-$VERSION.tar"
TARGET_ZIP="magnifier-$VERSION.zip"

MANUALS_DIR="docs"
MANUALS="README-EN.rtf README-PT.rtf README-EN.pdf README-PT.pdf"

RESOURCES="topleft.bmp topright.bmp bottomleft.bmp bottomright.bmp top.bmp left.bmp bottom.bmp right.bmp icon3.ico icon3.png cecae.bmp feusp.bmp vmg.bmp lupa.bmp usplegal.bmp"

SCRIPTS="install.sh uninstall.sh"

DEBIAN_USR_DIR=/home/felipe/Programas/magnifier/build/usr

##################################
# Builds a binary tar package
##################################
BuildBinary ()
{
  # Goes to the root directory of the magnifier

  cd ..

  # Builds the software

  ./make.sh

  strip --strip-all magnifier

  # Creates main directory

  mkdir $TARGET_DIR

  # Copies files to the directory

  cp $RESOURCES $TARGET_DIR

  cp $SCRIPTS $TARGET_DIR

  mkdir $TARGET_DIR/$MANUALS_DIR

  cd $MANUALS_DIR

  cp $MANUALS ../$TARGET_DIR/$MANUALS_DIR

  cd ..

  cp ./magnifier $TARGET_DIR

  # Creates the archive

  tar -cvf $TARGET_TAR $TARGET_DIR

  bzip2 $TARGET_TAR

  # Clean up

  rm -rf $TARGET_DIR

  ./clean.sh

  cd build

  return
}


##################################
# Creates a source zip package
##################################
SourcePackage ()
{
  # Goes to the root directory of the magnifier

  cd ..

  # Clean up

  echo "Clean up"
  ./clean.sh
  rm -rf ../magnifier-$VERSION/

  # We use SVN export to get rid of the heavy svn files
  # copies all files to a new temporary directory

  echo "svn export ./ ../magnifier-$VERSION/"
  svn export ./ ../magnifier-$VERSION/

  # Creates the package

  echo "zip -rv ../$TARGET_ZIP ../magnifier-$VERSION/"
  zip -rv ../$TARGET_ZIP ../magnifier-$VERSION/

  # Clean up

  echo "Clean up"
  rm -rf ../magnifier-$VERSION/
  cd build

  return
}

##################################
# Set up the RPM build environment
##################################
CreateRPMEnvironment ()
{
  # Creates the directory structure

  mkdir $HOME/RPM
  mkdir $HOME/RPM/BUILD # This directory is utilized by RPM to build the package.
  mkdir $HOME/RPM/RPMS # Here you can find binary RPMs after you build them.
  mkdir $HOME/RPM/SOURCES # Place your compressed tar files and patches here.
  mkdir $HOME/RPM/SPECS # Place all your spec files here.
  mkdir $HOME/RPM/SRPMS # Here you can find source RPMs after you build them. 

  # rpmbuild environment file

  touch $HOME/.rpmmacros

  echo "%_topdir                /home/felipe/RPM/" >> $HOME/.rpmmacros
  echo "%_tmppath               /home/felipe/tmp" >> $HOME/.rpmmacros
  echo "" >> $HOME/.rpmmacros
  echo "%_signature             gpg" >> $HOME/.rpmmacros
  echo "%_gpg_name              Mandrakelinux" >> $HOME/.rpmmacros
  echo "%_gpg_path              ~/.gnupg" >> $HOME/.rpmmacros

  # Spec file

  cp magnifier.spec $HOME/RPM/SPECS/

  # Zip file

  cp ../../$TARGET_ZIP $HOME/RPM/SOURCES/

  return
}

##################################
# Builds a binary and source RPM package
##################################
RPMPackage ()
{
  # Set up the RPM build environment
  CreateRPMEnvironment

  # now build it
  echo "rpmbuild -ba --clean $HOME/RPM/SPECS/magnifier.spec"
  rpmbuild -ba --clean $HOME/RPM/SPECS/magnifier.spec

  return
}

##################################
# Creates a Debian package
##################################
DebianPackage ()
{
  # Goes to the root directory of the magnifier

  cd ..

  # Builds the software

#  ./make.sh

  strip --strip-all magnifier

  # Returns to build dir

  cd build

  # Creates the control.tar.gz file

  tar -cvf control.tar control

  gzip control.tar

  # Creates the data.tar.gz file

  mkdir $DEBIAN_USR_DIR
  mkdir $DEBIAN_USR_DIR/bin
  mkdir $DEBIAN_USR_DIR/share
  mkdir $DEBIAN_USR_DIR/share/magnifier

  cd ..

  cp ./magnifier $DEBIAN_USR_DIR/bin/vmg

  cp $RESOURCES $DEBIAN_USR_DIR/share/magnifier

  cd $MANUALS_DIR

  cp $MANUALS $DEBIAN_USR_DIR/share/magnifier

  cd ..

  cd build

  tar -cvf data.tar $DEBIAN_USR_DIR

  gzip data.tar

  # Creates the package

  mkdir DEBIAN

  cp control DEBIAN/
  cp data.tar.gz DEBIAN/
  cp debian-binary DEBIAN/

  dpkg -b ./ magnifier_3.6-0_i386.deb

  # Clean up

  echo "Clean up"

  rm -rf $DEBIAN_USR_DIR

  rm -rf ./DEBIAN

  rm -rf data.tar.gz

  rm -rf control.tar.gz

  cd ..

  ./clean.sh

  cd build

  return
}


##################################
# Main section
##################################

echo "========================================================"
echo "    Virtual Magnifying Glass build script"
echo "========================================================"
echo ""
echo " Please select which package you would like to build:"
echo ""
echo " 1 > Linux Gtk2 binary tar.bz2 package"
echo " 2 > FreeBSD Gtk2 binary tar.bz2 package"
echo " 3 > Source .zip package"
echo " 4 > RPM package (source and binary)"
echo " 5 > Debian package"
echo " 0 > Exit"

read command

case $command in

  1) BuildBinary;;

  2) OS="freebsd"
     TARGET_DIR="./magnifier-$OS-$VERSION/"
     TARGET_TAR="magnifier-$OS-$VERSION.tar"

     BuildBinary;;

  3) SourcePackage;;

  4) RPMPackage;;

  5) DebianPackage;;

  0) exit 0;;

  *) echo "Invalid command"
     exit 0;;

esac
