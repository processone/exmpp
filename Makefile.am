ACLOCAL_FLAGS = -I m4
ACLOCAL_AMFLAGS = -I m4

SUBDIRS = c_src include src

if ENABLE_EXAMPLES
SUBDIRS += examples
endif

SUBDIRS += ebin

if ENABLE_DOCUMENTATION
SUBDIRS += doc
endif

if ENABLE_TESTSUITE
SUBDIRS += testsuite
endif

EXTRA_DIST =
distdir = $(PACKAGE)-$(VERSION)

if ENABLE_TESTSUITE
check-junit: all
	cd testsuite && $(MAKE) $(AM_MAKEFLAGS) $@ || exit 1
else
check-junit:
endif

updateknown:
	cd c_src && $(MAKE) $(AM_MAKEFLAGS) $@ || exit 1

.PHONY: check-junit updateknown
