# $Id$

ACLOCAL_FLAGS = -I m4
ACLOCAL_AMFLAGS = -I m4

SUBDIRS = c_src include src ebin

if ENABLE_DOCUMENTATION
SUBDIRS += doc
endif

if ENABLE_TESTSUITE
SUBDIRS += testsuite

check-junit: all
	cd testsuite && $(MAKE) $(AM_MAKEFLAGS) $@
else
check-junit:
endif
