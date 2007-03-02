# $Id$

EXTRA_DIST = testsuite.erl test.sh					\
	     check_xml_parser.erl					\
	     check_xml_attribute_handlers.erl				\
	     check_xml_element_handlers.erl				\
	     check_xml_cdata_handlers.erl				\
	     check_xml_path.erl						\
	     check_xml_converters.erl					\
	     check_xmlstream.erl					\
	     check_jid.erl						\
	     check_factory_stream.erl					\
	     check_factory_legacy_auth.erl				\
	     check_coverity.erl
CLEANFILES = *.beam *.html $(TESTS) erl_crash.dump

REAL_TESTS = check_xml_attribute_handlers				\
	     check_xml_element_handlers					\
	     check_xml_cdata_handlers					\
	     check_xml_path						\
	     check_xml_converters					\
	     check_jid							\
	     check_factory_stream					\
	     check_factory_legacy_auth

if WITH_EXPAT
REAL_TESTS += check_xml_parser						\
	      check_xmlstream
endif

TESTS = $(REAL_TESTS) check_coverity

TESTS_ENVIRONMENT = ERL="$(ERL)"					\
	TOP_SRCDIR="$(top_srcdir)"					\
	TOP_BUILDDIR="$(top_builddir)"					\
	TESTS="$(REAL_TESTS)"						\
	COVERED_MODULES="exmpp_xml exmpp_xmlstream exmpp_jid exmpp_factory"

build:
	$(ERL) -make

check_xml_parser: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_xml_attribute_handlers: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_xml_element_handlers: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_xml_cdata_handlers: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_xml_path: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_xml_converters: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_xmlstream: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_jid: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_factory_stream: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_factory_legacy_auth: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_coverity: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@
