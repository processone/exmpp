# $Id: Makefile.am,v 1.1 2005/12/14 15:24:56 dumbbell Exp $

EXTRA_DIST = testsuite.erl test.sh					\
	     check_xml_parser.erl					\
	     check_xml_serializer.erl
CLEANFILES = *.beam $(TESTS) erl_crash.dump

TESTS = check_xml_parser.sh check_xml_serializer.sh

TESTS_ENVIRONMENT = ERL="$(ERL)"

build:
	$(ERL) -make

check_xml_parser.sh: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@

check_xml_serializer.sh: build $(srcdir)/test.sh
	cp $(srcdir)/test.sh $@ && chmod 755 $@
