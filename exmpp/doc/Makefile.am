# $Id$

if BUILD_DOCUMENTATION

EXTRA_DIST = overview.edoc stylesheet.css

stylesheet.css:
	(test "$(srcdir)" != "." &&\
	 (test ! -e "$@" || test "$(srcdir)/$@" -nt "$@") &&\
	 cp "$(srcdir)/$@" "$@") || :

erlang.png:

ERL_SRCS = $(top_srcdir)/src/core/*.erl					\
	   $(top_srcdir)/src/server/*.erl				\
	   $(top_srcdir)/src/client/*.erl				\
	   $(top_srcdir)/src/compat/*.erl				\
	   $(top_srcdir)/src/examples/*.erl

index.html: overview.edoc $(ERL_SRCS) stylesheet.css
	$(ERL) -noshell\
		-run edoc_run application "'$(PACKAGE_NAME)'" '"$(top_srcdir)"'\
		'[{def, {vsn, "$(PACKAGE_VERSION)"}}, {dir, "."}, {stylesheet, "stylesheet.css"}, {preprocess, true}, no_packages]'\
		-s erlang halt
	$(SED) -i '' -e 's|</title>|</title><meta http-equiv="Content-Type" content="text/html; charset=UTF-8">|'  -e 's|\xa9|\&copy;|g' *.html

all-local: index.html

clean-local:
	(test "$(srcdir)" != "." && rm -f stylesheet.css) || :

htmldocdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/doc/html

htmldoc_DATA = *.html stylesheet.css erlang.png

CLEANFILES = *.html edoc-info erlang.png

endif
