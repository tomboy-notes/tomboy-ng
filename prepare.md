Building Debian or PPA source packages.
===========



This document is about building the source packages that are uploaded to either Debian Mentors or Launchpad PPA. With tomboy-ng, we use scripts (in the scripts dir) to do the initial setup prior to building and then instructions here to complete the processes. In all cases, we upload a source package, it must build on their remote system.



These notes are primarly for my, David's, use and assume that we are using the already configured VM's for this job. There is, however, a section on configuring VMs for this purpose, setting up ID and certificates.



**debhelper version** Debian Bullseye requires 13, building the PPA on U20.04 requires 12 but because we target  U18.04 for GTK2, must be 11 there. The PPA for the Qt5 version is 12 because we target Focal as a starting point (U18.04 will not run Qt5 apps easily). This, and other variations are all covered in the mkcontrol.bash script.

**IMPORTANT**
======
While fiddling around, making sure its all going to build, easy to blow away the working dir and start again. BUT, once you have ***submitted something to Mentors or the PPA*** you cannot "start again", a package rebuild must use the original files, the scripts help here but only if you have the original files !

So, do not, delete a failed build from Launchpad, ever ! 


**Debian Source**
========
Understand that there are two distinct build processes for making a Deb Src. The first, used initially or when releasing a new version of tomboy-ng, involves downloading the tomboy-ng tree from github, making some changes and generating a new .orig file. Then making a .changes and .dsc file. In this model, the resulting package always has a -1 debian suffix.



The other building model is when a build problem is noted, the tomboy-ng source has not changed, just something cleaned up in the build.  Here we take the previous .orig file and associated files, ideally safely tucked away, or downloaded .orig. and other files andextract its contents.  Make whatever change is necessary and rebuild. The .orig file is not re-uploaded and the resulting package will have -2 or -3, -4 on a really bad day.



The process is download (or extract) tomboy-ng source, remove unnecessary content, build the SRC package, copy files to a clean directory, do a test build (that makes the .deb file) and run an pedantic lintian. If thats all satisfactory, we upload to Mentors.


**Debian SRC Build step by step**
--------
(all assuming you are David and using a pre configured VM (see below), DebTestMate0922, rev the release number as required, it has no effect on product, just a convienance while building). The Debian script makes a src package that targets unstable, that name does not change.

And, note, the Debian build machines run unstable, not testing ! See the VM Setup section below.

    
This, as of Nov 2022, now makes a Qt5 version.

    export DebVer="34g"
    mkdir "Build""$DebVer"; cd "Build""$DebVer"
    wget https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/scripts/prepare.debian
    bash ./prepare.debian -n -q
    cd tomboy-ng [tab]
    debuild -S
    cd .. 
    
OK, if everything is OK, both build VMs have a script, test-debs.bash in ~/bin, run it in the working directory, where, eg, the .orig.tar.gz file is, and it will take the necessary files to a new dir, build the binary deb and run Lintian.


If its still all good, go back and and upload the src packages.

    cd ../"Build""$DebVer"
    dput  mentors  *.changes

If you don't get a response, did you include 'mentors' in the dput line ?

***REMEMBER to feed changlog back to github tree !***




**Launchpad PPA**
========
Is built on a different VM, U2004mQt. A little more complicated because we also build the Qt5 version, changelog needs to be 'adjusted'. There is a script that automates the whole build SRC packages, unpack and build binaries. The PPA script will make a Qt5 Focal package or a GTK2 Bionic package depending on the -Q switch. You need to make both so add a -Q the export var below.

At some time in the future, it will be necessary to adjust those distro names.

**WARNING - ppa prepare.ppa script is wrong if a package needs to be repackaged, it cannot, itself, increment the package number, must be done manually. See the debian one.**

**PPA build Steps**
--------

    export PPAVer="PPAv33"
    mkdir "Build""$PPAVer"; cd "Build""$PPAVer"
    wget https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/scripts/prepare.ppa
    bash ./prepare.ppa   [-Q]
    cd tomboy-ng [tab]
    debuild -S; cd ...
    
    
OK, if all looks OK, go back and upload with

    dput ppa:d-bannon/ppa-tomboy-ng *.changes
    
Now, the QT5 version Important, we target focal not bionic

    cd ..
    export PPAVer="PPAv33QT"
    mkdir "Build""$PPAVer"; cd "Build""$PPAVer"
    wget https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/prepare.ppa
    bash ./prepare.ppa  -Q      // the -Q says make a Qt5 version [-Q] 
    cd tomboy-ng [tab]
    debuild -S; cd ..
OK, if all looks OK, go back and upload

    dput ppa:d-bannon/ppa-tomboy-ng *.changes

Did you follow that about versions ?  To target u18.04 we must specify (in control) debhelper 11, in Focal 12, in Bullseye 13.

**Repackaging**
=======
Many problems that happen can be fixed with a repackage, you must keep the original files and its easy. In the directory where they are (or where you extract downloaded ones) you again use  one of the prepare.* scripts, leave off the -n and but retaining the **-Q** in the Qt5 ones. The script will find the src working dir, rename it with a newer packaging number, make the all too important addition to the change log and set you on your way.

eg, first repackage, from -1 to -2

    mkdir ../Build35-2qt5
    cp *.gz *.xz *.dsc prepare.debian ../Build35-2qt5
    cd ../Build35-2qt5
    dpkg-source -x *.dsc
    mv tomboy-ng-0.35 tomboy-ng-0.35-2
    bash ./prepare.debian -q -r "Added new dependencies"

    // Now, make whatever changes are necessary to packaging (not source) and then 
    // in the tomboy-ng-0.35-2 directory, run -

    debuild -S
    
    Test and submit as above, remember, send to mentors (dput defaults to ftp.debian and it gets dropped on the floor).
    


**PPA: **If you don't have the initial files, .orig.tar.gz, .dsc and .xz then you can download them from Launchpad (even if deleted), use the Package Name Contains filter, left of screen. Then, in a dir with just those three files (and perhapse prepare.ppa) run 

    dpkg -x *.dsc

But that will extract a dir called, eg tomboy-ng-qt5-0.35, **you must add the -1 to that dir name** and then use prepare.ppa as mentioned above.



**Building just a tomboy-ng Binary**
========
If all you want is the binary, not building src packages at all, not cross compiling, then don't worry about signing etc, just -

* install FPC (>=3.2.0),  Lazarus (>=2.2.0), libnotify-dev

* install libqt5pas-dev if building a QT5 version

* `wget https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/scripts/prepare.ppa`

* `bash ./prepare.ppa [-Q]`      // the -Q says make a Qt5 version please.

* `cd tomboy-ng[tab] [enter]`

* `bash ./buildit.bash`

The binary will be in the source/. directory below where you are now standing.



**VM Setup**
========
The machine to at least test the source package for Debian needs to be a current unstable. Further, because on the mentor's machines, apt does not install Recommended, we need to ensure the test machines is the same.

So, take a current VM of Testing, update irs /etc/apt/sources.lst to point to unstable instead of, eg, bookwork. Apt update; apt upgrade.
Then, add a file, /etc/apt/apt.config that has one line, 

     apt::install-recommends "false";
    
May be a good idea to use this machine as the build machine as well, and treat it as temporary. But remember to take a snapshot of the *.gz *.xz and *.dsc files from a sumnitted build and keep, somewhere.

A PGP key is required to upload to Mentors or Launchpad. It lives in ~/.gnupg. 


    David Bannon <tomboy-ng@bannons.id.au>


(The 17741.... is the fingerprint)



On the Launchpad PPA machine, In users home dir, a file called .devscripts.conf that contains 

DEBSIGN_KEYID=nnnn......   ie, the full key fingerprint.



I am unsure how, on the Debian VM, the script knows which PGP Key to use.



Both prepare scripts have hardwired my personal full name and tomboy-ng email address. These will only be used if relevent env vars are empty. Note that they must match whats available in a gpg key. AND if that does not match the Maintainer: entry from control, we get a non maintainer upload warning.



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



