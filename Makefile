
# Some not surprising template stuff

PREFIX = /usr
BIN_DIR = $(PREFIX)/bin
PROGRAM_NAME=tomboy-ng
MAN_BASE_DIR = $(PREFIX)/share/man
MAN_DIR = $(MAN_BASE_DIR)/man1
MAN_EXT = 1
DOC_DIR = $(PREFIX)/share/doc/$(PROGRAM_NAME)
HELP_DIR = $(DOC_DIR)/HELP
HELP_EN_DIR = $(HELP_DIR)/EN
HELP_ES_DIR = $(HELP_DIR)/EN
HELP_NOTES = calculator.note recover.note sync-ng.note tomboy-ng.note tomdroid.note key-shortcuts.note
RM      = rm -f
MKDIR   = mkdir -p
INSTALL = install
INSTALL_PROGRAM = $(INSTALL) -c -m 0755
INSTALL_DATA    = $(INSTALL) -c -m 0644

tomboy-ngx86_64: 
	bash ./buildit.bash
	@echo "====== Maybe we just compiled $(TBVER)"

clean:
	@echo "====== Nothing to clean, promise $(TBVER)"

install: installdirs
	$(INSTALL_PROGRAM)	tomboy-ng/tomboy-ng   $(DESTDIR)$(BIN_DIR)/$(PROGRAM_NAME)
	$(INSTALL_DATA)		doc/tomboy-ng.1 $(DESTDIR)$(MAN_DIR)/$(PROGRAM_NAME).$(MAN_EXT)
	$(INSTALL_DATA)		doc/HELP/ES/calculator.note	$(DESTDIR)$(HELP_ES_DIR)/calculator.note
	$(INSTALL_DATA)		doc/HELP/ES/recover.note	$(DESTDIR)$(HELP_ES_DIR)/recover.note	
	$(INSTALL_DATA)		doc/HELP/ES/sync-ng.note	$(DESTDIR)$(HELP_ES_DIR)/sync-ng.note
	$(INSTALL_DATA)		doc/HELP/ES/tomboy-ng.note	$(DESTDIR)$(HELP_ES_DIR)/tomboy-ng.note
	$(INSTALL_DATA)		doc/HELP/ES/tomdroid.note	$(DESTDIR)$(HELP_ES_DIR)/tomdroid.note
	$(INSTALL_DATA)		doc/HELP/ES/key-shortcuts.note	$(DESTDIR)$(HELP_ES_DIR)/key-shortcuts.note
	$(INSTALL_DATA)		doc/HELP/EN/calculator.note	$(DESTDIR)$(HELP_EN_DIR)/calculator.note
	$(INSTALL_DATA)		doc/HELP/EN/recover.note	$(DESTDIR)$(HELP_EN_DIR)/recover.note	
	$(INSTALL_DATA)		doc/HELP/EN/sync-ng.note	$(DESTDIR)$(HELP_EN_DIR)/sync-ng.note
	$(INSTALL_DATA)		doc/HELP/EN/tomboy-ng.note	$(DESTDIR)$(HELP_EN_DIR)/tomboy-ng.note
	$(INSTALL_DATA)		doc/HELP/EN/tomdroid.note	$(DESTDIR)$(HELP_EN_DIR)/tomdroid.note
	$(INSTALL_DATA)		doc/HELP/EN/key-shortcuts.note	$(DESTDIR)$(HELP_EN_DIR)/key-shortcuts.note
	@echo ================ called install:

installdirs:
	test -d $(DESTDIR)$(BIN_DIR) || $(MKDIR) $(DESTDIR)$(BIN_DIR)
	test -d $(DESTDIR)$(MAN_DIR) || $(MKDIR) $(DESTDIR)$(MAN_DIR)
	test -d $(DESTDIR)$(HELP_EN_DIR) || $(MKDIR) $(DESTDIR)$(HELP_EN_DIR)
	test -d $(DESTDIR)$(HELP_ES_DIR) || $(MKDIR) $(DESTDIR)$(HELP_ES_DIR)
	@echo =============== Called installdirs:
