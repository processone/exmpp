# $Id$

CLEANFILES = expat_drv.so stringprep_drv.so tls_drv.so iconv_erl.so

check-local: expat_drv.so iconv_erl.so stringprep_drv.so tls_drv.so

expat_drv.so: $(top_builddir)/c_src/.libs/$@
	cp -p $(top_builddir)/c_src/.libs/$@ $@

iconv_erl.so: $(top_builddir)/c_src/.libs/$@
	cp -p $(top_builddir)/c_src/.libs/$@ $@

stringprep_drv.so: $(top_builddir)/c_src/.libs/$@
	cp -p $(top_builddir)/c_src/.libs/$@ $@

tls_drv.so: $(top_builddir)/c_src/.libs/$@
	cp -p $(top_builddir)/c_src/.libs/$@ $@
