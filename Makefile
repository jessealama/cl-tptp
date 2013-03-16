.PHONY: clean test

asd-files = $(wildcard *.asd)
editable-files = $(asd-files) Makefile README.mkd .gitignore
emacs-backups = $(addsuffix ~,$(editable-files))
ccl-fasls = $(wildcard *.dx64fsl)
plain-fasls = $(wildcard *.fasl)
fasls = $(ccl-fasls) $(plain-fasls)

clean:
ifneq ($(strip $(emacs-backups)),)
	rm -f $(emacs-backups)
endif
ifneq ($(strip $(fasls)),)
	rm -f $(fasls)
endif

test:
	test -d test
	test -r test/Makefile
	+make -C test
