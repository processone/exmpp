# $Id: Makefile.am,v 1.1 2005/12/14 15:24:56 dumbbell Exp $

EXTRA_DIST = testsuite.erl test.sh					\
	     check_xml_parser.erl					\
	     check_xml_attribute_handlers.erl				\
	     check_xml_element_handlers.erl				\
	     check_xml_cdata_handlers.erl				\
	     check_xml_path.erl						\
	     check_xml_serializer.erl					\
	     check_coverity.erl
CLEANFILES = *.beam $(TESTS) erl_crash.dump

TESTS = check_xml_parser.sh						\
	check_xml_attribute_handlers.sh					\
	check_xml_element_handlers.sh					\
	check_xml_cdata_handlers.sh					\
	check_xml_path.sh						\
	check_xml_serializer.sh						\
	check_coverity.sh

TESTS_ENVIRONMENT = ERL="$(ERL)"					\
	TOP_SRCDIR="$(top_srcdir)"					\
	TOP_BUILDDIR="$(top_builddir)"					\
	TESTS="check_xml_parser check_xml_attribute_handlers check_xml_element_handlers check_xml_cdata_handlers check_xml_path check_xml_serializer"	\
	COVERED_MODULES="exmpp_xml exmpp_xmlstream"

build:
	$(ERL) -make

check_xml_parser.sh: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_xml_attribute_handlers.sh: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_xml_element_handlers.sh: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_xml_cdata_handlers.sh: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_xml_path.sh: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_xml_serializer.sh: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_coverity.sh: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@
