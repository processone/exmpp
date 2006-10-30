# $Id$

CLEANFILES = expat_drv.so

check-local: expat_drv.so

expat_drv.so: $(top_builddir)/c_src/.libs/$@
	cp -p $(top_builddir)/c_src/.libs/$@ $@
