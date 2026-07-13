Misty
===========



If you are looking for the quick start guide, well, bad luck !



Misty is a (software) server for a tomboy-ng sync system. Its one of the ways you can keep multiple instances of tomboy-ng all in sync. Its a development project and, at present is experimental. Please be aware of the security issues, see the Security discussion below.



**Where can I run Misty ?**
--------
Misty can run on a Linux or Windows box (MacOS not just yet) and can be on the machine you also use for tomboy-ng or a stand alone machine like a Raspberry PI. It does not need a powerful machine, just needs enough available disk space to accommodate a copy of all your notes. It is launched from the command line and can be left running indefinitely or just started up when needed. Misty must be used with tomboy-ng 0.42c or later. It must be allowed to make its own synced note repo, do not, under any circumstances, point Misty to your existing tomboy-ng note repository. See below "More about the server machine."



To run Misty on a **Windows** machine, go to your Network & Internet  Settings page, the default setting is probably *"Public Network (recommended)"* but you need to set it to "*Private Network (your device is discoverable ...)*". This is reasonably safe on a local, home network. Further, if you allow the screen saver to activate, it will stop Misty running, so, keep working or disable the screen saver (yes, I know its pretty).



Misty is intended to run on a local home network where, typically a reasonable firewall in the router protects you from external threats. It can run on a machine "live" on the internet, while I don't recommend it, Misty used with a secure web certificate is possibly as safe many other systems. Your call.



Misty should be run as an ordinary user, no benefit (and increased risk) running as root / administrator.



**Do I need a web certificate ?**
--------
No, but its a good idea. While Misty can run in a very easily setup but very insecure mode, a better choice, only a tiny bit harder is the secure mode. The latter uses SSL, a web certificate and a password to protect your notes from prying eyes.

   * This means that your tomboy-ng and Misty must have **SSL libraries** available, Linux users will have them preinstalled but Windows and Mac users may not. They are easy enough to install. See the tomboy-ng wiki pages for help with that. The server needs a **web certificate**. If you use an external facing box you will probably already have certificates, you need to identify where they are. If its a home network then its easy to generate a "self signed certificate" that works fine with tomboy-ng connections (see below).

   * You need a **password**, known to both the server and each instance of tomboy-ng you will be connecting with. In the interest of simplicity I have used 'basic' authentication, its up to you to ensure both ends use the same password.



The Misty server will display some help (and exits) with -h on the command line. When it does start up, it displays a few lines of info including a **URL** you might use to test it and certainly need when configuring tomboy-ng.



You can provide all needed information, you enter it on the command line. Or you can enter it all and add -s and it will be saved and reused with all subsquent starts. When Misty starts up, it shows you the URL you must enter into tomboy-ng misty sync config. Same URL to test the server with a browser. Note, include the http:// or https:// noting the difference, its about security.



**The "Safe Mode" (not guaranteed but pretty good)**
--------
Its safer and absolutely essential if your server box is visible from the internet, maybe too if you plan to leave it running on even a home network. If your server box does not have an official certificate, then its easy to make a "self signed" one. Run the following command line on the box you wish to use as the server.



    $> openssl req -newkey rsa:2048 -x509 -days 365 -keyout domain.key -out domain.crt -nodes


Will, after a few inconsequential questions, make two files, domain.crt and domain.key, Misty requires those files plus a Sync Password shared between tomboy-ng and Misty.



In the directory where you have the Misty binary and, if applicable, the two certificate files run this command -



    $> ./misty-server -c domain.crt -k domain.key -w -s


Misty will prompt for a sync password (that you will also have to use in tomboy-ng) and will make sensible choices for Repo, port (8088) and location of config and log file. The "-s" says save all those settings (leave it off if just playing around).  You can now config tomboy-ng and run a sync. Come back here and press Ctrl-C to stop the server. You can alter those settings at any time with a similar command.



Once a config file is saved, Linux users may want to start the server to run even when you close the terminal session.



$> nohup ./misty-server &



This a user space daemon, runs with only your permissions and, even if security is compromised, can only process files under the Repo directory (ie ~/Misty). Traffic between Misty and tomboy-ng is encrypted, most of the internet runs like this.



**The "no security model"**
--------
The "no security model" needs neither certificate nor password. You may feel comfortable using it only on a home network, behind a reasonable firewall. That is very much your call. It means plain text across the ether, if there is a risk anyone is listening in, dangerous. But its simple, easy and quick to get going.



***Do not, under any circumstances, use the "no security model" on the open internet.***



    $>./misty-server <enter>


Press Ctrl-C to stop it. I suggest you do not leave it running in no security mode. Fire it up, sync, shut it down.



There are options to set the port number and alternative repo location, try with -h for help. Quite possibly no real need to save  config file in this mode but you can (-s).



**Checking with a browser.**
--------
Fun to do, not necessary. Put the URL mentioned above into your browser. You will see a tomboy-ng title, number of notes it has and little else, but  the "No security mode" and self signed certificates worry your browser but are quite safe as you have control of both ends. Just reassure it with a "security exception" (varies between browsers).



On Firefox, for example, the no security and self signed process will get you a screen "Security Risk Ahead", click the "accept the risk and continue". Firefox will remember this.



**Connecting tomboy-ng to the Misty Server.**
--------
tomboy-ng v0.42c and later has a third syc option, open the settings window, click the sync tab and select Misty .. from the "Sync Type" combo box near the top. Enter, carefully, the URL the server told you about and, if working securely, the password, and click Change Sync Repository. Wait a bit ....



Note that in "Secure mode" the URL starts with "https" but the insecure mode has only "http". The URL you enter must start with either http:// or https:// and must end with Misty's port number, Misty default is :8088. You can replace the host name with an ip address eg 182.168.5.17 if you know it and know it won't change but you still need the https:// (or http://) and the port number.



**Security**
--------
OK, some discussion so you can make an informed choice about using Misty or not. In practise there are two risks you face.

   * Listening In - in the "no security" mode, a real risk if anyone in your household knows something about web protocols. In the "Secure" mode that uses SSL encryption, almost impossible.

   * Hacking - lets face it, if you are really targeted by a skilled hacking team, you are in trouble. But in the secure mode you have taken what is considered reasonable precautions.

   * Physical access to your computer is probably a far greater risk and its there with no sync in use. But I still recommend using the secure sync mode.



**What could possibly go wrong ?**
--------
Astroid strike ? Not my fault. Anything wrong with the sync is possibly my problem and I will help to fix it if I can. But, its open source, I cannot be held responsible for any losses or damage you may suffer. Please, if you have a problem, report it !



**More about the server machine.**
--------
As mentioned above, Misty will run on any 64bit Linux or Windows box with up to date SSL libraries. That means OpenSSL 3 or equivalent. Linux users will have those libraries and the openssl command used to generate a self signed certificate preinstalled and, hopefully, kept up to date. Windows and MacOS will possibly need to install  OpenSSL but you may well already have it for another application.



Misty is not very demanding in terms of hardware, it will run happily on an old Raspberry Pi powered by a discarded phone charger. My only concern there is the SDCard, those cards are not terribly reliable, but if this is all you use it for, probably OK. Maybe change the card every few years or plug better storage into the 'Pi'.



An old laptop could easily be pressed into service but would use more power.



If you have a access to a (hopefully) well maintained server directly facing the internet, care is still needed. You absolutly MUST use a certificate, probably the certificate that machine already has, then you are roughly as secure the rest of the internet. Misty, in that mode uses SSL, its the gold standard (my code may not be). Make sure the URL that goes into tomboy-ng starts with "https". No 's' means no security !



Most users, in my opinion, will use Misty in a home network, they will have a firewall (whether or not they know it) in the router / modem and are probably well protected. In this model the URL (given secure mode and a hostname of 'myserver') will be https://myserver.local and thats safe, URLs ending in .local are not accessible from outside. I still suggest Secure Mode but that is up to you.



Many ISP's provide a 'web server' for a smal fee. In a lot of cases, these are shared operating systems and the ISP manages Apache or other web server. Its likely Misty will not work on such systems. I may, in the future make a version of Misty that works in CGI/FCGI mode and it will probably solve that problem.



Again, do NOT run the server as root / admin, not safe and absolutely not necessary.



**Other Questions**
--------
Are best directed to the issues at https://github.com/tomboy-notes/tomboy-ng . But here are a few FAQs -



   * Does Misty allow me to view or edit my notes on line ?   No, not at present. Not difficult to do but I am concerned about security. Maybe in the future.

   * Will Misty run as a CGI/FCGI/Apache Module ? No, maybe in the future.

   * Does Misty use SSL in "No Security mode" ?  No, its either secure or not secure.

   * Does Misty compromise the server it is running on ?  In my opinion, no. Misty will only deal with files below its Sync Repo and does nothing more (at present) than list, upload and download those files.

   * My old Linux does not have OpenSSL 3 ?  Upgrade your Linux, please !

   * I don't have OpenSSL 3 on my Windows box ?  OK. please install it. But beware, if you have an older version of OpenSSL, some app you may be dependent on that old version. If thats possible, just get the DLLs and openssl.exe from the package and put them in same directory as misty-server.

   * My Windows box does not have the OpenSSL command used to generate a certificate but it does have SSL libraries ?  You should have  the command "openssl.exe" somewhere on (or not on) your PATH. If not its possible you  could have just the libraries but not the OpenSSL package.

   * Are you an expert in administrating a Windows box ?  No. Absolutely NOT !

   * Do you collect tips, bounties etc ? No, I have used open source for a large par of my life, this is my 'put back'.

   * In tomboy-ng, I see an error message, "Unable to sync ....reading data from socket, Result Code 0" ?  Its probable that you have a mixed up secure mode with insecure mode. Secure mode must have 'https' at the start of the Settings->Sync->Misty->Repo line, assumes you have provided a certificate to the misty-server.




