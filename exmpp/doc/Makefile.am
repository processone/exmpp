# $Id$

if BUILD_DOCUMENTATION
stylesheet.css:
	(test "$(srcdir)" != "." &&\
	 (test ! -e "$@" || test "$(srcdir)/$@" -nt "$@") &&\
	 cp "$(srcdir)/$@" "$@") || :

erlang.png:

ERL_SRCS = $(top_srcdir)/src/core/*.erl					\
	   $(top_srcdir)/src/server/*.erl				\
	   $(top_srcdir)/src/client/*.erl				\
	   $(top_srcdir)/src/compat/*.erl

index.html: overview.edoc $(ERL_SRCS) stylesheet.css
	$(ERL) -noshell\
		-run edoc_run application "'$(PACKAGE_NAME)'" '"$(top_srcdir)"'\
		'[{def, {vsn, "$(PACKAGE_VERSION)"}}, {dir, "."}, {stylesheet, "stylesheet.css"}, {preprocess, true}, no_packages]'\
		-s erlang halt

all-local: index.html

clean-local:
	(test "$(srcdir)" != "." && rm -f stylesheet.css) || :

htmldocdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/doc/html
nodist_htmldoc_DATA = *.html stylesheet.css erlang.png

CLEANFILES = *.html edoc-info
endif
