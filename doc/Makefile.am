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

HTML_FILES = index.html							\
	     modules-frame.html						\
	     overview-summary.html					\
	     packages-frame.html					\
	     exmpp.html							\
	     exmpp_callbacks.html					\
	     exmpp_client_binding.html					\
	     exmpp_client_compression.html				\
	     exmpp_client_legacy_auth.html				\
	     exmpp_client_presence.html					\
	     exmpp_client_pubsub.html					\
	     exmpp_client_register.html					\
	     exmpp_client_roster.html					\
	     exmpp_client_sasl.html					\
	     exmpp_client_session.html					\
	     exmpp_client_stream.html					\
	     exmpp_client_tls.html					\
	     exmpp_error.html						\
	     exmpp_internals.html					\
	     exmpp_jid.html						\
	     exmpp_jlib.html						\
	     exmpp_server_binding.html					\
	     exmpp_server_compression.html				\
	     exmpp_server_legacy_auth.html				\
	     exmpp_server_sasl.html					\
	     exmpp_server_session.html					\
	     exmpp_server_stream.html					\
	     exmpp_server_tls.html					\
	     exmpp_session.html						\
	     exmpp_ssl.html						\
	     exmpp_stream.html						\
	     exmpp_stringprep.html					\
	     exmpp_sup.html						\
	     exmpp_tcp.html						\
	     exmpp_xml.html						\
	     exmpp_xmlstream.html					\
	     echo_client.html						\
	     ejabberd_zlib.html						\
	     iconv.html							\
	     jlib.html							\
	     sha.html							\
	     stringprep.html						\
	     tls.html							\
	     xml.html

$(HTML_FILES): overview.edoc $(ERL_SRCS) stylesheet.css
	$(ERL) -noshell\
		-run edoc_run application "'$(PACKAGE_NAME)'" '"$(top_srcdir)"'\
		'[{def, {vsn, "$(PACKAGE_VERSION)"}}, {dir, "."}, {stylesheet, "stylesheet.css"}, {preprocess, true}, no_packages]'\
		-s erlang halt
	$(SED) -i '' -e 's|</title>|</title><meta http-equiv="Content-Type" content="text/html; charset=UTF-8">|'  -e 's|\xa9|\&copy;|g' *.html

all-local: $(HTML_FILES)

clean-local:
	(test "$(srcdir)" != "." && rm -f stylesheet.css) || :

htmldocdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/doc/html

htmldoc_DATA = $(HTML_FILES) stylesheet.css erlang.png

CLEANFILES = $(HTML_FILES) edoc-info erlang.png

endif
