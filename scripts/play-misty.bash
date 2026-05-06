#!/usr/bin/bash

echo "Sorry, you MUST make some changes to this script before using it."
echo "At least remove these lines and the 'exit' below AND set the password"
exit

# This is a sample script you may (or may not) like to use to start and 
# stop the misty server. It is not supplied ready to go, you will
# certainly need to change some things. Like the name of the binary if you
# run it on eg a Raspberry Pi, certainly the password, maybe (but hopefully
# not) the use of a SSL certificate and password.
# And you can change where the Repo is created, where the logs go and where
# files such as the binary, the certificate files go. Your call.


# You can put the Misty Repo anywhee you like as long as you have write
# permision.
REPO="~/Misty"

# Similarly the log directory, maybe you need to have a look at it
# from time to time.
LOG="~/Logs"

# This is where you have un-tared the misty tgz and so should now contain
# this script, the misty-server binary and the readme note (that can be
# opened with tomboy-ng). You really should read the note ....
cd ~/bin

if [ "$1" == "stop" ]; then
	killall misty-server
	exit 0
fi

if [ "$1" == "status" ]; then
	ps aux | grep misty-server | grep -v grep
	ls -l ~/Misty/home/manifest.xml
	exit 0
fi

if [ "$1" == "log" ]; then
	cat "$LOGS"/misty.log
	exit 0
fi

# OK, if to here must be a start request. Note, log is started new each time.
# You MUST change the password and satisfy yourself that its safe to leave in the script!
# the port and repo options actually reflect the default, here so you can change them.
# This start line includes ref to certificates, if you made self signed ones, they will
# look like this, if using official certs, up to you to find where they are.
# Just remove the last three options if you are feeling lucky.
 
nohup ./misty-server-x86-64 -r ~/Misty -p 8088 -c domain.crt -k domain.key -w TrustMe > ~/Logs/misty.log &


