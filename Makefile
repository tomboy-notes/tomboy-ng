
# A makefile specificially to assist with making a SRC Deb for tomboy.
# Might work to install as a src tarball but its not regularly tested for this.
#
# copyright David Bannon, 2020, use as you see fit, but retain this statement.

# https://www.gnu.org/software/make/manual/html_node/index.html

# Some not surprising template stuff

PREFIX = /usr
BIN_DIR = $(PREFIX)/bin
PROGRAM_NAME=tomboy-ng
MAN_DIR = $(PREFIX)/share/man/man1
SHARE_DIR = $(PREFIX)/share
DOC_DIR = $(SHARE_DIR)/$(PROGRAM_NAME)
# Debian and AppImage have seperate meta data file names ! Here I copy both, AppImage is happy ....
METAINFOFILE=io.github.tomboy_notes.tomboy_ng_notes.metainfo.xml
METAAPPIMAGE=io.github.tomboy_notes.tomboy_ng_notes.appdata.xml
# ---- Help Notes, it just replicates existing dir/note structure.
HELP_DIR = $(DOC_DIR)/HELP
RM      = rm -f
RMDIR	= rm -Rf
MKDIR   = mkdir -p
OUTFILES = ../*.deb ../*.build ../*.xz ../*.dsc ../*.buildinfo ../*.changes kcontrols/source/tomboy-ng.log source/tomboy-ng.log
CLEANDIR = kcontrols/packages/kcontrols/lib source/lib kcontrols/source/lib
INSTALL = install
INSTALL_PROGRAM = $(INSTALL) -c -m 0755
INSTALL_DATA    = $(INSTALL) -c -m 0644
CP = cp -R
# ----- Language translation files, just add 2 letter code here -----
LANGUAGES = es fr nl uk
MKDIRLANG = test -d $(DESTDIR)$(SHARE_DIR)/locale/$(LANG)/LC_MESSAGES || $(MKDIR) $(DESTDIR)$(SHARE_DIR)/locale/$(LANG)/LC_MESSAGES
CPLANG = msgfmt -o $(DESTDIR)$(SHARE_DIR)/locale/$(LANG)/LC_MESSAGES/tomboy-ng.mo po/tomboy-ng.$(LANG).po



tomboy-ngx86_64: 
	bash ./buildit.bash
	$(info ========== We have compiled [${PROGRAM_NAME}])
	# $(info ========== $$BIN_DIR is [${BIN_DIR}])


clean:
	rm -Rf 		debian/tomboy-ng
	$(RM)		debian/debhelper-build-stamp
	$(RM)		debian/tomboy-ng.substvars
	$(RMDIR)	$(CLEANDIR)
	$(RM)		$(OUTFILES)
	$(RM)		source/Tomboy_NG source/tomboy-ng
	$(RM)		kcontrols/package/kcontrols/KControls.log
	$(RM)		$(APPDIR)
	# clean is pretty useless here, any change to tree upsets debuild and apparently
	# kcontrols changes some src file during package build. So, refresh !

install: installdirs
	$(info ========== Installing ....)
	$(INSTALL_PROGRAM)	source/tomboy-ng   	$(DESTDIR)$(BIN_DIR)/$(PROGRAM_NAME)
	$(INSTALL_DATA)		doc/tomboy-ng.1 	$(DESTDIR)$(MAN_DIR)/$(PROGRAM_NAME).1
	$(CP)			doc/HELP		$(DESTDIR)$(HELP_DIR)/
	$(CP)			doc/overrides		$(DESTDIR)$(SHARE_DIR)/lintian/overrides/tomboy-ng
	$(CP)           doc/$(METAINFOFILE) $(DESTDIR)$(SHARE_DIR)/metainfo/$(METAINFOFILE)
	$(CP)           doc/$(METAINFOFILE) $(DESTDIR)$(SHARE_DIR)/metainfo/$(METAAPPIMAGE)
	$(CP)			glyphs/icons		$(DESTDIR)$(SHARE_DIR)/
	$(INSTALL_DATA)	glyphs/tomboy-ng.desktop	$(DESTDIR)$(SHARE_DIR)/applications/tomboy-ng.desktop
	$(foreach LANG, $(LANGUAGES), $(CPLANG);)
#	$(CP)	$(APPDIR)/usr/share/icons/hicolor/256x256/apps/tomboy-ng.png $(APPDIR)/usr/share/icons/hicolor/256x256/apps/tomboy-ng.png.png		

installdirs:
	test -d $(PREFIX) || $(MKDIR) $(PREFIX)
	test -d $(PREFIX)$(SHARE_DIR)/metainfo || $(MKDIR) $(PREFIX)$(SHARE_DIR)/metainfo
	test -d $(DESTDIR)$(BIN_DIR) || $(MKDIR) $(DESTDIR)$(BIN_DIR)
	test -d $(DESTDIR)$(MAN_DIR) || $(MKDIR) $(DESTDIR)$(MAN_DIR)
	test -d $(DESTDIR)$(DOC_DIR) || $(MKDIR) $(DESTDIR)$(DOC_DIR)
	test -d $(DESTDIR)$(SHARE_DIR)/metainfo || $(MKDIR) $(DESTDIR)$(SHARE_DIR)/metainfo
	test -d $(DESTDIR)/share/applications || $(MKDIR) $(DESTDIR)$(SHARE_DIR)/applications
	test -d $(DESTDIR)/share/lintian/overrides || $(MKDIR) $(DESTDIR)$(SHARE_DIR)/lintian/overrides
	$(foreach LANG, $(LANGUAGES), $(MKDIRLANG);)

#set_appimage_dir:			# just here to set PREFIX 
#	$(eval PREFIX = $(APPDIR)/usr)	
#	echo "set PREFIX to " $(PREFIX)

#appimage: set_appimage_dir install 

appimage: tomboy-ngx86_64 install

