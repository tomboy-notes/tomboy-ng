
# A makefile specificially to assist with making a SRC Deb for tomboy.
# Might work to install as a src tarball but its not regularly tested for this.
#
# copyright David Bannon, 2020, use as you see fit, but retain this statement.


# Some not surprising template stuff

PREFIX = /usr
BIN_DIR = $(PREFIX)/bin
PROGRAM_NAME=tomboy-ng
MAN_DIR = $(PREFIX)/share/man/man1
SHARE_DIR = $(PREFIX)/share
DOC_DIR = $(SHARE_DIR)/doc/$(PROGRAM_NAME)
HELP_DIR = $(DOC_DIR)/HELP
HELP_EN_DIR = $(HELP_DIR)/EN
HELP_ES_DIR = $(HELP_DIR)/ES

RM      = rm -f
MKDIR   = mkdir -p
INSTALL = install
INSTALL_PROGRAM = $(INSTALL) -c -m 0755
INSTALL_DATA    = $(INSTALL) -c -m 0644
CP = cp -R

tomboy-ngx86_64: 
	bash ./buildit.bash
	@echo "====== Maybe we just compiled $(TBVER)"

clean:
	@echo "====== Nothing to clean, promise $(TBVER)"

install: installdirs
	$(INSTALL_PROGRAM)	tomboy-ng/tomboy-ng   	$(DESTDIR)$(BIN_DIR)/$(PROGRAM_NAME)
	$(INSTALL_DATA)		doc/tomboy-ng.1 	$(DESTDIR)$(MAN_DIR)/$(PROGRAM_NAME).1
	$(CP)			doc/HELP		$(DESTDIR)$(DOC_DIR)/
	$(CP)			glyphs/icons		$(DESTDIR)$(SHARE_DIR)/
	$(INSTALL_DATA)	glyphs/tomboy-ng.desktop	$(DESTDIR)$(SHARE_DIR)/applications/tomboy-ng.desktop

	@echo ================ called install:

installdirs:
	test -d $(DESTDIR)$(BIN_DIR) || $(MKDIR) $(DESTDIR)$(BIN_DIR)
	test -d $(DESTDIR)$(MAN_DIR) || $(MKDIR) $(DESTDIR)$(MAN_DIR)
	test -d $(DESTDIR)$(HELP_EN_DIR) || $(MKDIR) $(DESTDIR)$(HELP_EN_DIR)
	test -d $(DESTDIR)$(HELP_ES_DIR) || $(MKDIR) $(DESTDIR)$(HELP_ES_DIR)
	test -d $(DESTDIR)/share/applications || $(MKDIR) $(DESTDIR)$(SHARE_DIR)/applications
	@echo =============== Called installdirs:
