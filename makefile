CC= chicken-csc
BIN= clean-dload-dir

PREFIX= $(HOME)/.local/

$(BIN): cleandir.scm
	PREFIX=$(PREFIX) $(CC) $^ -o $@ -vv

.Phony: install
install:
	install -m u+wx,a+r $(BIN) $(PREFIX)/bin
	install -m u+w,a+r  -D cleandloads.timer -t $(PREFIX)/share/systemd/user/
	install -m u+w,a+r  -D cleandloads.service -t $(PREFIX)/share/systemd/user/
	install -m u+w,a+r  -D config.toml -t  $(PREFIX)/share/cleandir/
uninstall:
	rm "$(PREFIX)/bin/$(BIN)"
