#!/usr/bin/bash
# A quick and dirty script playing with where icons go.
# Definitly not for production use.


APPNAME=tomboy-ng
DEST="$HOME"/.icons/hicolor

function WriteDesktop () {
	# we need write a new one cos need to mention user's user name.
	DT_FILE="$HOME"/.local/share/applications/"$APPNAME".desktop
	echo "--- DT_FILE is $DT_FILE"
	echo "[Desktop Entry]" > "$DT_FILE"
	echo "Name=tomboy-ng" >> "$DT_FILE"
	echo "Name[de]=tomboy-ng" >> "$DT_FILE"
	echo "Name[es]=tomboy-ng" >> "$DT_FILE"
	echo "Name[fr]=tomboy-ng" >> "$DT_FILE"
	echo "Comment=Cross Platform Notes" >> "$DT_FILE"
	echo "Comment[de]=Notizen Plattformübergreifend" >> "$DT_FILE"
	echo "Comment[es]=Notas Multiplatforma" >> "$DT_FILE"
	echo "Comment[fr]=Notes Multiplateforme" >> "$DT_FILE"
	echo "GenericName=Note Taker" >> "$DT_FILE"
	echo "GenericName[de]=Notizanwendung" >> "$DT_FILE"
	echo "GenericName[es]=Tomador de apuntes" >> "$DT_FILE"
	echo "GenericName[fr]=Application de prise de notes" >> "$DT_FILE"
	echo "Exec=$HOME/bin/tomboy-ng %f" >> "$DT_FILE"
	echo "Icon=$DEST/256x256/apps/tomboy-ng.png" >> "$DT_FILE"
	echo "Terminal=false" >> "$DT_FILE"
	echo "Type=Application" >> "$DT_FILE"
	echo "Categories=GNOME;Utility" >> "$DT_FILE"
}

function InstallStuff () {
	if [ "$1" = "remove" ]; then
		echo "Remove Mode"
	else
		echo "Install Mode"
	fi
	if [ "$1" = "remove" ]; then
		rm /home/"$USER"/.local/share/applications/"$APPNAME".desktop
		rm "$HOME"/bin/"$APPNAME"
	else
		WriteDesktop
		# cp "$APPNAME".desktop "$HOME"/.local/share/applications/"$APPNAME".desktop
		mkdir -p "$HOME"/bin
		cp "$APPNAME" "$HOME"/bin/"$APPNAME" 
	fi
	# --------- Icons -----------
	mkdir -p "$DEST"
	for i in 16x16 22x22 24x24 32x32 48x48 256x256 ; do
		if [ "$1" = "remove" ]; then
			rm "$DEST/$i/apps/$APPNAME.png"
		else
			mkdir -p "$DEST"/"$i"/apps
			cp "$i.png" "$DEST/$i/apps/$APPNAME.png"
		fi
		# echo "$DEST/$i/apps/$APPNAME.png"
	done;
		gtk-update-icon-cache -t "$DEST"
}


InstallStuff "$1"

