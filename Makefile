.PHONY: clean

asd-files = $(wildcard *.asd)
editable-files = $(asd-files) Makefile README.mkd .gitignore
emacs-backups = $(addsuffix ~,$(editable-files))

clean:
	rm -f $(emacs-backups)
