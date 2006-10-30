# $Id$

beamdir =  $(ERLANG_INSTALL_LIB_DIR_exmpp)/ebin

beam_DATA = exmpp.app							\
	    exmpp.appup							\
	    exmpp.beam							\
	    exmpp_xml.beam						\
	    exmpp_xmlstream.beam					\
	    exmpp_jlib.beam						\
	    exmpp_factory.beam

if BUILD_COMPAT
beam_DATA +=
	    ejabberd_zlib.beam						\
	    iconv.beam							\
	    jlib.beam							\
	    sha.beam							\
	    stringprep.beam						\
	    tls.beam							\
	    xml.beam
endif

CLEANFILES = $(beam_DATA)
