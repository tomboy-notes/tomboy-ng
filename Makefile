
# Some not surprising template stuff

PREFIX = /usr
BIN_DIR = $(PREFIX)/bin
PROGRAM_NAME=tomboy-ng
MAN_BASE_DIR = $(PREFIX)/share/man
MAN_DIR = $(MAN_BASE_DIR)/man1
MAN_EXT = 1
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
	#$(INSTALL_DATA)		fdupes.1 $(DESTDIR)$(MAN_DIR)/$(PROGRAM_NAME).$(MAN_EXT)
	@echo ================ called install:

installdirs:
	test -d $(DESTDIR)$(BIN_DIR) || $(MKDIR) $(DESTDIR)$(BIN_DIR)
	test -d $(DESTDIR)$(MAN_DIR) || $(MKDIR) $(DESTDIR)$(MAN_DIR)
	@echo =============== Called installdirs:
