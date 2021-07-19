Building Debian or PPA source packages.
===========



This document is about building the source packages that are uploaded to either Debian Mentors or Launchpad PPA. With tomboy-ng, we use two scripts to do the initial setup prior to building and then instructions here to complete the processes. In all cases, we upload a source package, it must build on their remote system.



These notes are primarly for my, David's, use and assume that we are using the already configured VM's for this job. There is, however, a section on configuring VMs for this purpose, setting up ID and certificates.



**debhelper version** Debian Bullseye requires 13, building the PPA on U20.04 requires 12 but if its U18.04, must be 11. Sigh ....


**Debian Source**
========
Understand that there are two distinct build processes for making a Deb Src. The first, used initially or when releasing a new version of tomboy-ng, involves downloading the tomboy-ng tree from github, making some changes and generating a new .orig file. Then making a .changes and .dsc file. In this model, the resulting package always has a -1 debian suffix.



The other building model is when a build problem is noted, the tomboy-ng source has not changed, just something cleaned up in the build.  Here we take the previous .orig file, extract its contents, make whatever change is necessary and rebuild. The .orig file is not re-uploaded and the resulting package will have -2 or -3, -4 on a really bad day.



The process is download (or extract) tomboy-ng source, remove unnecessary content, build the SRC package, copy files to a clean directory, do a test build (that makes the .deb file) and run an pedantic lintian. If thats all satisfactory, we upload to Mentors.



**Debian SRC Build steps**
--------
(all assuming you are David and using a pre configured VM, Debian-T, rev the release number as required)



    export DebVer="Debv33"
    mkdir "Build""$DebVer"; cd "Build""$DebVer"
    wget https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/prepare.debian
    bash ./prepare.debian -D unstable -n
    cd tomboy-ng [tab]
    debuild -S
    cd .. 
    mkdir ../Test"$DebVer"
    cp *.xz *.gz *.dsc ../Test"$DebVer"; cd ../Test"$DebVer"
    dpkg-source -x *.dsc
    cd tomboy-ng [tab]
    dpkg-buildpackage -us  -uc; cd ..
    lintian -IiE --pedantic *.changes
OK, if everything is OK, go back and and upload it

    cd ../"Build""$DebVer"
    dput -f mentors *.changes


REMEMBER to feed changlog back to github tree !



**Launchpad PPA**
========
Is built on a different VM, U2003mQt



**PPA build Steps**
--------
    export PPAVer="PPAv33"
    mkdir "Build""$PPAVer"; cd "Build""$PPAVer"
    wget https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/prepare.ppa
    bash ./prepare.ppa -D bionic
    cd tomboy-ng [tab]
    debuild -S; cd ...
    bash ./prepare.ppa -D bionic -Q      // the -Q says make a Qt5 version [-Q] 
    cd tomboy-ng-qt5 [tab]
    debuild -S; cd ..
OK, if all looks OK, go back and upload
    dput ppa:d-bannon/ppa-tomboy-ng *.changes




**Building just a tomboy-ng Binary**
========
If all you want is the binary, not building src packages at all, not cross compiling, then don't worry about signing etc, just -

* install FPC (>=3.2.0),  Lazarus (>=2.0.10), libnotifier-dev

* install libqt5pas-dev if building a QT5 version

* `wget https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/prepare.ppa`

* `bash ./prepare.ppa [-Q]`      // the -Q says make a Qt5 version please.

* `cd tomboy-ng[tab] [enter]`

* `bash ./buildit.bash`

The binary will be in the source/. directory below where you are now standing.



**VM Setup**
========
A PGP key is required to upload to Mentors or Launchpad. It lives in ~/.gnupg. 

    pgp ~/.gpg/public-keys/tomboy-ng-GPU-KEY [enter]
    pub   rsa3072 2020-03-10   
      `79445......`

    uid   tomboy-ng <tomboy-ng@bannons.id.au>


(The 79445.... is the fingerprint)



On the Launchpad PPA machine, In users home dir, a file called .devscripts.conf that contains 

DEBSIGN_KEYID=7944 5......   ie, the full key fingerprint.



I am unsure how, on the Debian VM, the script knows which PGP Key to use.



Both prepare scripts have hardwired my personal full name and tomboy-ng email address. These will only be used if relevent env vars are empty. Note that they must match whats available in a gpg key. AND if that does not match

the Maintainer: entry from control, we get a non maintainer upload warning.



Debian Bullseye likes debhelper = 13, Ubuntu is still on 12 in control file



**Install** 

devscripts

Lazarus >= 2.0.10, FPC >= 3.2.0



Debian  need a config file, .dput.cf in $HOME that points to mentors, see mentors website. https://mentors.debian.net  The Launchpad PPA VM does not seem to have that, we put destination address in the dput command line.  The Debian file looks like -

    [mentors]
    fqdn = mentors.debian.net
    incoming = /upload
    method = https
    allow_unsigned_uploads = 0
    progress_indicator = 2
    # Allow uploads for UNRELEASED packages
    allowed_distributions = .*



