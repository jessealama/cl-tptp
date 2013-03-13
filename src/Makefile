.PHONY: clean

lisp-files = $(wildcard *.lisp)
editable-files = $(lisp-files) Makefile
emacs-backups = $(addsuffix ~,$(editable-files))

clean:
	rm -f $(emacs-backups)
