Download_Release
===========



**tomboy-ng v0.42 Release Page**
========


<sub>Don't edit this file, its generated from Releases.note in doc dir, converted to md and processed by releasepage tool.</sub>



Downloads and information for specific platforms further down the page.



**Index**
========
   * [Release Notes](https://github.com/tomboy-notes/tomboy-ng/wiki/Download_Release#Release-Notes)

   * [Windows Downloads](https://github.com/tomboy-notes/tomboy-ng/wiki/Download_Release#Windows)

   * [MacOS Downloads](https://github.com/tomboy-notes/tomboy-ng/wiki/Download_Release#MacOS)

   * [Linux Downloads](https://github.com/tomboy-notes/tomboy-ng/wiki/Download_Release#Linux)



**Release Notes**
========


   * Allow --disableaccurateform switch for RasPiOS, this will prevent a small, non-responsive window appearing when you start tomboy-ng. Why ?  Ask the RasPiOS developers.

   * Tab key now creates Bullet or extends an existing Bullet (or extends an existing Indent).

   * Shift-Tab to reduce Bullet or Indent.

   * Ctrl-Tab to create Indent (but only one paragraph at a time).

   * Indent now more compatible with original Tomboy. You can now open an note from the old Tomboy and indents at the start of a paragraph will appear as intended. However, indents (ie tabs) in the middle of a paragraph will not, eg, create columns of text.

   * Github Sync now allows tokens without an expiry date.

   * New command line option, -c, to create new note, eg with Operating System Hot Key.

   * Linux Qt5 and Qt6 packages require their respective libQtxPas libraries. Most older Linux distributions have out of date versions of these libraries, the known exceptions being Ubuntu 24.04, 24.10, Debian Trixie, Fedora  41+ and up to date Arch based systems. See below for where to get current libraries -



https://github.com/davidbannon/libqt5pas/releases

https://github.com/davidbannon/libqt6pas/releases





**Qt5 and Qt6 users may need to install a new libqt5pas1 or libqt6pas6 library, your distro version may be out of date unless you are using eg Fedora 41, Ubuntu 24.04, 24.10, any Arch system or Debian Trixie. Other Linux systems should get the new libraries from the links below and install them before or at the same time as you install tomboy-ng. (New libraries are not needed if you choose the GTK2 version.)**



https://github.com/davidbannon/libqt5pas/releases

https://github.com/davidbannon/libqt6pas/releases



**Installation**
========


tomboy-ng is available for Linux, Mac and Windows. Some Linux Distributions have tomboy-ng in its repositories but newer releases are usually available here.



**Windows**
--------
Tested on 32bit Windows Vista and 64bit Windows 10 and 11.  Windows users should download the Windows Installer, https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng-setup-0.42.exe, it works for both 32 and 64bit installs.  Windows Defender will (quite rightly) tell you it stopped an unrecognised app, warning its risky. Click "More Info" and you will see a "Run Anyway". Now you will be asked if you want to allow an unknown publisher (me!) to change your device. If you do, click yes. Depending on your build of Win11, you may need to go through a process of clicking the More Info link and Run Anyway, possibly through several stages.



**MacOS**
--------
Tested and built on an Intel based Mac Monterey but Apple Silicon (aka aarch64) is now also directly supported. Use (https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng64_0.42.dmg). Install by double clicking the .dmg file and drag the tomboy-ng icon into the nearby Applications Folder. Because I don't pay a fee to Apple, first time users will need to control click (that is, hold the Control Key down and click) the tomboy-ng icon in Applications and select "Open".



On macOS 15 and later, it has become even harder to launch non-codesigned installers and applications. Here's an explanation of what you have to do: https://www.idownloadblog.com/2024/08/07/apple-macos-sequoia-gatekeeper-change-install-unsigned-apps-mac/ . I'd like to point out that a malware writer, who stands to make a lot of money from his project, is quite likely to pay Apple a signing fee. On the other hand, I make no money from this project and cannot justify paying an arbitrary fee to Apple.



This is a "Universal Package", will install on both Intel based and ARM64 based systems.



**Linux**
========
Linux users need to be a bit careful about which version they download. With quite a range of Distributions,  Desktops, 32bit and 64bit and GTK2 v. Qt5 v. Qt6 to cater for, its not easy ! The Qt5 version requires Qt5.12 (preferably Qt5.15) or later and 64bit. The GTK version, needs GTK2, pre installed on many Distros at present.



**AppImage**
--------
I now provide two AppImage files that can be downloaded directly, marked as executable and, possibly run directly without any further install.  There are a few things to note -

* Will work on Linux Distros as old as Ubuntu 20.04. The Qt5 version will work in Debian Bullseye but not the Qt6 version.

* Like all AppImages, the download will need to be marked executable, the download process removes that attribute.

         $>chmod u+x tomboy-ng--x86_64-Qt5.AppImage <enter>
* These packages were built on an **Ubuntu 20.04** system (and therefore contains libraries from U20.04) and are not guaranteed to work on significantly older systems. I note the the Qt5 version works fine on **Debian 11 Bullseye** but the Qt6 one does not.

* Both packages are around 35Meg

* On newer Ubuntu systems, the app may fail to start with a message mentioning fuse, a library to allow user level file system mounting. (Ubuntu of course have their own bundling system, Snaps.)  The necessary library is easily added

         $>sudo apt install install libfuse2t64 <enter>
* Argon (and reportedly other OpenSUSE systems) also need fuse, install with

         $> sudo zypper install fuse libfuse2 <enter>
* Your feedback about the use of AppImage would be appreciated.



Note  : The pacman package is, of necessity, built on a system that used glibc v2.36 or later and the binary will not work on systems running glibc less than v2.34. In a pacman world I expect this not to be a problem but if I'm wrong, please let me know.



If you use a **dark theme** with the QT5 version you should also read the man page (man tomboy-ng <enter>) or  [Dark Theme](https://github.com/tomboy-notes/tomboy-ng/wiki/Bugs-and-Known-Issues#QT5-versions-and-Dark-Theme).



As a general rule, the gtk2 version is your best bet on older operating systems, newer ones, should use the Qt5 or Qt6 ones. The Qt5 and especially the Qt6 may require a few more libraries at install time but they are mostly libraries other apps will want anyway.



**Linux Qt5 and Qt6 Libraries**
--------
New in 0.37, is the need to install a new, non-standard, libqt5pas or libqt6pas library. If using the Qt5 or Qt6 tomboy-ng of 0.37 or later, you MUST install special libraries unless your distro has caught up. At the last check, Fedora 41, Ubuntu 24.04, 24.10, Debian Trixie and any Arch based distro do not need these manually loaded libraries. They are at



https://github.com/davidbannon/libqt5pas/releases  - 64bit Intel or AMD users want  libqt5pas-2.15-3.x86_64.rpm or  libqt5pas1_2.15-1_amd64.deb

https://github.com/davidbannon/libqt6pas/releases - 64bit Intel or AMD users want libqt6pas6-6.2.7-2.x86_64.rpm or libqt6pas6_6.2.7-1_amd64.deb



Download the appropriate library (for your system and for the type of tomboy-ng you intend to use) and install it before or during the tomboy-ng install.





**PPA using distros, Ubuntu, Linux Mint etc**
--------
Sadly, I am finding the effort of maintaining the tomboy-ng PPA just a bit too much. I have not given up yet but it is way out of date right now. Please consider downloading from here.

<sub>Ubuntu and derivative users (of 64bit U18.04 and U20.04 to U24.04) can use a PPA to install and update this and subsequent releases of tomboy-ng.  Please see https://launchpad.net/~d-bannon/+archive/Ubuntu/ppa-tomboy-ng for details. Available a few days after release here. New in version 0.37, the PPA will not be updating the Qt5 versions until the distros have caught up with the new LibQt5Pas library. So, it will be gtk2 only and that works badly with Gnome. Sorry, not anything I can do other than suggest a manual install.</sub>



For GTK based distributions that use .deb packages, do yourself a favour, install Gdebi before you start. But it will require a lot of gtk2 libraries you might otherwise not need.



**Ubuntu**
--------
Ubuntu 24.04 has tomboy-ng v0.39 but v0.42 fixes some important bugs and has a few new minor features.



Generally, all versions work fine but earlier distributions should use GTK2 and recent ones may use Qt5 based version.

   * 16.04 and 18.04 (while out of date, we still test with tomboy-ng) Use GTK2 version [32bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_i386.deb) or [64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_amd64.deb). Will not work with QT5.

   * 20.04  the GTK2 [64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_amd64.deb) version which needs only a couple of extra things and is probably a better choice than the Qt5  [64bit QT](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_amd64Qt5.deb) version which needs the Qt5 libraries

   * 22.04 and beyond, I suggest the the Qt5 [64bit QT](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_amd64Qt5.deb) version or Qt6 [64bit QT](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_amd64Qt6.deb) version. The GTK2 [64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_amd64.deb) version is probably OK too as long as you are not using Gnome.

   * If using **Qt5 or Qt6**, see the "Linux Qt5 and Qt6 Libraries" section above.

   * 24.04 does have the correct libQt5Pas in its repository (but not libQt6Pas).



On Ubuntu, you can install the appropriate package in one of several way  -



   * **Recommended, install gdebi first**.  Its available from your normal repos and is easy and reliable. Then download the appropriate tomboy-ng package, your browser will direct it to Gdebi. Strongly recommended for U16.04.

   * Depending on your version, you may be able to download and get directed to another tool to install, perhaps Software Installer, it probably will not work.

   * Thirdly, will work on all except U16.04, click on the appropriate link, download and save the file. Use  the following apt command (note the './' in front of the filename, you must explicitly state the location of the file, else apt will look in its own repositories). Adjust the deb name for your specific downloaded file -



Like this -

    sudo apt install ./tomboy-ng_0.42-0_amd64.deb [enter]


**Linux Mint**
--------
Pretty much the same as Ubuntu



**Fedora**
--------
Tested on Fedora 38, 39, 40 and 41. Generally, earlier Fedora the [GTK2 32bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng-0.42-1.i686.rpm) or [GTK2 64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng-0.42-1.x86_64.rpm) version is recommended. Newer versions should use Qt5 [64bit QT](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ngQt5-0.42-1.x86_64.rpm) version or Qt6 [64bit QT](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ngQt6-0.42-1.x86_64.rpm) version. If using Qt5 or Qt6, see the "Linux Qt5 and Qt6 Libraries" section above.



To install, please click on appropriate link, download and save, then use the yum or dnf command, eg -



    sudo yum localinstall ./tomboy-ng-0.42-0.x86_64.rpm [enter]


**Fedora's default install is based on Gnome** and  recent Gnome versions block the System Tray icon. Please see https://github.com/tomboy-notes/tomboy-ng/wiki/System-Tray-on-Linux .  Other flavours of Fedora (eg  **Plasma, Cinnamon, XFce**) all seem happy to display the System Tray Icon.





**Debian**
--------
An older version of tomboy-ng, v0.32 is available in the Debian **Bullseye** repository and 0.36c in Bookworm. **Trixie** will have v0.41 shortly and does already have a correct libQt5Pas library. Please see https://github.com/tomboy-notes/tomboy-ng/wiki/System-Tray-on-Linux if using Gnome. Both Bullseye (11) and Bookworm (12) benefit from manually installing the latest release from here, I recommend the gtk2 type for simplicity, debs : [32bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_i386.deb) or [64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_amd64.deb) .  Also available is the  Qt5 [64bit QT](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_amd64Qt5.deb) version or Qt6 [64bit QT](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_amd64Qt6.deb) type.



I recommend downloading the file and using the apt command (note the full path to the filename, you must explicitly state the location of the file, else apt will look in its own repositories).



    cd ~/Downloads
    sudo apt install ./tomboy-ng_0.42-0_amd64.deb [enter]    -- note the Qt5 version has a slightly different file name.


If you wish to sync to a SMB: network share, open that connection before configuring tomboy-ng sync. If you cannot see it in the File Dialog, you probably will need to install gvfs and gvfs-fuse (but bookworm seems to have them preinstalled, good).





**Arch and Manjaro**
--------
And other Linux Distributions that use the Arch, Pacman package system. I have made only a [GTK 64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng-0.42-1-x86_64.pkg.tar.zst) version and, recommended, a [Qt6 64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng-0.42-1-Qt6-x86_64.pkg.tar.zst) at this stage, could make other combinations if needed. Click the link, choose download and install with this command from ~/Downloads -



sudo pacman -U ./tomboy-ng-0.42-1-x86_64.pkg.tar.zst





**OpenSuse**
--------
Generally use the  [Qt5 64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ngQt5-0.42-1.x86_64.rpm) version or  [Qt6 64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ngQt6-0.42-1.x86_64.rpm) and look at https://github.com/tomboy-notes/tomboy-ng/wiki/System-Tray-on-Linux if you are using Gnome. Other Desktops should be OK with [GTK2 32bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng-0.42-1.i686.rpm) or [GTK2 64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng-0.42-1.x86_64.rpm). Almost all the necessary libraries seem to be pre installed in either case. If using **Qt5 or Qt6**, see the "Linux Qt5 and Qt6 Libraries" section above.



Suse is quite pushy about signing rpm packages, more work for me and it means you need to import the **PGP signature** I have used. So, please download [tomboy-ng-GPG-KEY](https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/package/tomboy-ng-GPG-KEY) (you may need to right click and choose "Save Link as") and run this command in the directory you downloaded to, note this is only necessary once -



    sudo rpm --import tomboy-ng-GPG-KEY [enter]


Next, to install tomboy-ng, please click on appropriate link, and if it suggests YaST2: Ruby, allow it to install, it works well.  If you receive an error message, its almost certainly because you have not yet installed the signature file mentioned above. You may prefer to download and use the zypper command -



    sudo zypper install ~/Downloads/tomboy-ngQt-0.42-1-x86_64.rpm [enter]


**Mageia**
--------
Mageia warns you if you try to install a package who's signature it does not recognise. You can tell it not to worry and install anyway or, to put its mind at rest, download [tomboy-ng-GPGKEY](https://raw.githubusercontent.com/tomboy-notes/tomboy-ng/master/package/tomboy-ng-GPG-KEY)  (you may need to right click and choose "Save Link as") and run this command in the directory you downloaded to, note this is only necessary once -



    sudo rpm --import tomboy-ng-GPG-KEY [enter]


Then, you can just click on the appropriate tomboy rpm link  [GTK2 32bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng-0.42-1.i686.rpm) , [GTK2 64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng-0.42-1.x86_64.rpm) ,  [Qt5 64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ngQt5-0.42-1.x86_64.rpm) or  [Qt6 64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ngQt6-0.42-1.x86_64.rpm) and allow Software Installer to install it for you but I found it occasionally generated an error that you don't get to see ! Try it by all means but if you don't find your tomboy-ng installed or upgraded, then revert to old fashioned way, download the file, save it, and use a command a bit like this -



    sudo -c "yum localinstall ./tomboy-ng-0.42-1.x86_64.rpm" [enter]


   * **Mate, XFCE, KDE (ie Plasma) and Cinnamon** work fine.

   * **Mageia with Enlightenment**  is probably better with the gtk2 or Qt5 version.

   * **Mageia Gnome,** its likely that the comments relating to Fedora Gnome above apply but have not been tested.



To install, please download and save the file, then use the rpm command, note that rpm seems to require you to uninstall a previous version, doing so will not remove notes or config.  Using the **Qt5 version on Mageia** is possible if you replace its very dated version of lib64qt6pas1 with the appropriate one from https://github.com/davidbannon/libqt5pas/releases.



**Raspberry PI**
--------
Users of the official Raspberry Pi OS (previously know as Raspbian) or, better still, other deb based Linux Distros made for Raspberry Pi, can now download a deb and install it using the apt commands mentioned above.



If your Operating System is 32bit download [Qt5 32bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_armhf.deb)

If your Operating System is a 64bit download [gtk2 64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_arm64.deb) or, recommended,  [Qt5 64bit](https://github.com/tomboy-notes/tomboy-ng/releases/download/v0.42/tomboy-ng_0.42-0_arm64Qt5.deb) I note that the official RasPi OS is using Wayland (even though its desktop is XFCe), the Qt5 version of tomboy-ng works least badly there. You should start tomboy-ng with the new command line option -

    $> tombot-ng --disableaccurateform <enter>


These problems do not exists in other Linux distributions.



I have not made Raspberry Pi rpms, seems few users.


















