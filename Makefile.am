# $Id$

ACLOCAL_FLAGS = -I m4
ACLOCAL_AMFLAGS = -I m4

SUBDIRS = c_src include src ebin

if ENABLE_DOCUMENTATION
SUBDIRS += doc
endif

if ENABLE_TESTSUITE
SUBDIRS += testsuite
endif

distdir = $(PACKAGE)-$(VERSION)$(EXMPP_DISTSUFFIX)

if ENABLE_TESTSUITE
check-junit: all
	cd testsuite && $(MAKE) $(AM_MAKEFLAGS) $@ || exit 1
else
check-junit:
endif

updateknown:
	cd c_src && $(MAKE) $(AM_MAKEFLAGS) $@ || exit 1

.PHONY: check-junit updateknown
