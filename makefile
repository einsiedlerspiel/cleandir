CC= chicken-csc
BIN= clean-dload-dir

PREFIX= $(HOME)/.local/

$(BIN): cleandir.scm
	$(CC) $^ -o $@ -vv

.Phony: install
install:
	install -m u+wx,a+r $(BIN) "$(PREFIX)/bin"
	install -m u+w,a+r cleandloads.timer   $(HOME)/.config/systemd/user/
	install -m u+w,a+r cleandloads.service $(HOME)/.config/systemd/user/
	install -m u+w,a+r -D config.toml -t $(HOME)/.config/cleandir/
uninstall:
	rm "$(PREFIX)/bin/$(BIN)"
