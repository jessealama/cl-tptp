.PHONY: clean test

asd-files = $(wildcard *.asd)
editable-files = $(asd-files) Makefile README.mkd .gitignore
emacs-backups = $(addsuffix ~,$(editable-files))

clean:
	rm -f $(emacs-backups)

test:
	test -d test
	test -r test/Makefile
	make -C test
