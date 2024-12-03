CC= chicken-csc
BIN= clean-dload-dir

PREFIX= $(HOME)/.local/

$(BIN): 
	$(CC) cleandir.scm -o $@

.Phony: install
install:
	install $(BIN) "$(PREFIX)/bin"
uninstall:
	rm "$(PREFIX)/bin/$(BIN)"
