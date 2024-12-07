CC=chicken-csc
PREFIX=$(HOME)/.local

BIN=clean-dload-dir
BIN_PATH=$(PREFIX)/$(BIN)

CONFIG_FILE=config.toml
export CONFIG_PATH=$(PREFIX)/share/cleandir/$(CONFIG_FILE)

SYSD_UNIT=cleandloads
SYSD_UNIT_PATH=$(PREFIX)/share/systemd/user

$(BIN): cleandir.scm
	$(CC) $^ -o $@ -vv

.Phony: install
install:
	install -m u+w,a+rx -D $(BIN) $(DESTDIR)$(BIN_PATH)
	install -m u+w,a+r  -D $(CONFIG_FILE) $(DESTDIR)$(CONFIG_PATH)

	install -m u+w,a+r  -D $(SYSD_UNIT).timer   -t $(DESTDIR)$(SYSD_UNIT_PATH)
	install -m u+w,a+r  -D $(SYSD_UNIT).service -t $(DESTDIR)$(SYSD_UNIT_PATH)

	sed -i s+ExecStart=.*+ExecStart=$(BIN_PATH)+ \
	$(DESTDIR)$(SYSD_UNIT_PATH)/$(SYSD_UNIT).service

uninstall:

	rm $(DESTDIR)$(BIN_PATH)
	rm $(DESTDIR)$(CONFIG_PATH)
	rm $(DESTDIR)$(SYSD_UNIT_PATH)/$(SYSD_UNIT).timer 
	rm $(DESTDIR)$(SYSD_UNIT_PATH)/$(SYSD_UNIT).service
