# $Id$

beamdir =  $(ERLANG_INSTALL_LIB_DIR_exmpp)/ebin

BEAM_FILES = exmpp.app							\
	     exmpp.appup						\
	     exmpp.beam							\
	     exmpp_sup.beam						\
	     exmpp_internals.beam					\
	     exmpp_xml.beam						\
	     exmpp_xmlstream.beam					\
	     exmpp_stringprep.beam					\
	     exmpp_jid.beam						\
	     exmpp_factory.beam						\
	     exmpp_jlib.beam 						\
	     exmpp_stream.beam 						\
	     exmpp_client_stream.beam 					\
	     exmpp_client_pubsub.beam					\
	     exmpp_server_stream.beam

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

CLEANFILES = $(beam_DATA)
