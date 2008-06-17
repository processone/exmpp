# $Id$
	     
ERL_FILES = testsuite.erl						\
	    check_xml_parser.erl					\
	    check_xml_attribute_handlers.erl				\
	    check_xml_element_handlers.erl				\
	    check_xml_cdata_handlers.erl				\
	    check_xml_path.erl						\
	    check_xml_converters.erl					\
	    check_xmlstream.erl						\
	    check_jid.erl						\
	    check_stream.erl						\
	    check_legacy_auth.erl					\
	    check_coverity.erl

BEAM_FILES = testsuite.beam						\
	     check_xml_parser.beam					\
	     check_xml_attribute_handlers.beam				\
	     check_xml_element_handlers.beam				\
	     check_xml_cdata_handlers.beam				\
	     check_xml_path.beam					\
	     check_xml_converters.beam					\
	     check_xmlstream.beam					\
	     check_jid.beam						\
	     check_stream.beam						\
	     check_legacy_auth.beam					\
	     check_coverity.beam

EXTRA_DIST = $(ERL_FILES) test.sh

CLEANFILES = $(BEAM_FILES) *.html $(TESTS) erl_crash.dump

REAL_TESTS =

if WITH_EXPAT
REAL_TESTS += check_xml_parser						\
	      check_xmlstream
endif

REAL_TESTS += check_xml_attribute_handlers				\
	      check_xml_element_handlers				\
	      check_xml_cdata_handlers					\
	      check_xml_path						\
	      check_xml_converters					\
	      check_jid							\
	      check_stream						\
	      check_legacy_auth

COVERED_MODULES = exmpp_xml						\
		  exmpp_xmlstream					\
		  exmpp_stringprep					\
		  exmpp_jid						\
		  exmpp_stream						\
		  exmpp_client_legacy_auth				\
		  exmpp_server_legacy_auth

TESTS = $(REAL_TESTS) check_coverity

TESTS_ENVIRONMENT = ERL="$(ERL)"					\
	TOP_SRCDIR="$(top_srcdir)"					\
	TOP_BUILDDIR="$(top_builddir)"					\
	TESTS="$(REAL_TESTS)"						\
	COVERED_MODULES="$(COVERED_MODULES)"

TSCRIPT = $(srcdir)/test.sh

$(BEAM_FILES): $(ERL_FILES)
	@$(ERL) -noshell -pa @top_builddir@/ebin			\
		-eval "case make:all() of				\
		         up_to_date -> halt(0);				\
		         Err ->						\
		           io:format(\"~p~n\", [Err]),			\
		           halt(1)					\
		       end"


all-local:
	@rm -f index.html

$(TESTS): $(BEAM_FILES) $(TSCRIPT)
	@(test ! -e $@ -o $(TSCRIPT) -nt $@ && echo "Create: $@" &&	\
	 cp $(TSCRIPT) $@ && chmod 755 $@) || :
