# $Id$

if BUILD_DOCUMENTATION
.PHONY: stylesheet.css

stylesheet.css:
	(test "$(srcdir)" != "." &&\
	 (test ! -e "$@" || test "$(srcdir)/$@" -nt "$@") &&\
	 cp "$(srcdir)/$@" "$@") || :

erlang.png:

all-local: stylesheet.css
	$(ERL) -noshell -run edoc_run application "'$(PACKAGE_NAME)'" '"$(top_srcdir)"'\
		'[{def, {vsn, "$(PACKAGE_VERSION)"}}, {dir, "."}, {stylesheet, "stylesheet.css"}, {preprocess, true}, no_packages]'\
		-s erlang halt

clean-local:
	(test "$(srcdir)" != "." && rm -f stylesheet.css) || :

htmldocdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/doc/html
nodist_htmldoc_DATA = *.html stylesheet.css erlang.png

CLEANFILES = *.html edoc-info
endif
