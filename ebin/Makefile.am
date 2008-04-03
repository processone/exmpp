# $Id$

beamdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/ebin
examplesdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/examples/ebin

BEAM_FILES = exmpp.app							\
	     exmpp.appup						\
	     exmpp.beam							\
	     exmpp_error.beam						\
	     exmpp_internals.beam					\
	     exmpp_jid.beam						\
	     exmpp_jlib.beam						\
	     exmpp_stream.beam						\
	     exmpp_stringprep.beam					\
	     exmpp_sup.beam						\
	     exmpp_xml.beam						\
	     exmpp_xmlstream.beam					\
	     exmpp_client_binding.beam					\
	     exmpp_client_compression.beam				\
	     exmpp_client_legacy_auth.beam				\
	     exmpp_client_presence.beam					\
	     exmpp_client_pubsub.beam					\
	     exmpp_client_register.beam					\
	     exmpp_client_roster.beam					\
	     exmpp_client_sasl.beam					\
	     exmpp_client_session.beam					\
	     exmpp_client_stream.beam					\
	     exmpp_client_tls.beam					\
	     exmpp_server_binding.beam					\
	     exmpp_server_compression.beam				\
	     exmpp_server_legacy_auth.beam				\
	     exmpp_server_sasl.beam					\
	     exmpp_server_session.beam					\
	     exmpp_server_stream.beam					\
	     exmpp_server_tls.beam					\
	     exmpp_callbacks.beam					\
	     exmpp_session.beam						\
	     exmpp_ssl.beam						\
	     exmpp_tcp.beam

if BUILD_COMPAT
BEAM_FILES += ejabberd_zlib.beam					\
	      iconv.beam						\
	      jlib.beam							\
	      sha.beam							\
	      stringprep.beam						\
	      tls.beam							\
	      xml.beam
endif

beam_DATA = $(BEAM_FILES)

examples_DATA = echo_client.beam

CLEANFILES = $(beam_DATA) $(examples_DATA)
