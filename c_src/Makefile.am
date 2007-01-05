# $Id$

lib_LTLIBRARIES = expat_drv.la

if BUILD_COMPAT
lib_LTLIBRARIES += ejabberd_zlib_drv.la					\
		   iconv_erl.la						\
		   stringprep_drv.la					\
		   tls_drv.la
endif

EI_CPPFLAGS = -I$(ERLANG_ROOT_DIR)/usr/include\
	      -I$(ERLANG_LIB_DIR_erl_interface)/include
EI_LDFLAGS = -L$(ERLANG_LIB_DIR_erl_interface)/lib
EI_LIBS = -lei_st

libdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/priv/lib

# ejabberd_zlib port driver
ejabberd_zlib_drv_la_CPPFLAGS = $(EI_CPPFLAGS)
ejabberd_zlib_drv_la_LDFLAGS = -module -avoid-version\
			       $(EI_LDFLAGS) $(EI_LIBS)\
			       -lz
ejabberd_zlib_drv_la_SOURCES = ejabberd_zlib_drv.c

# Expat port driver
expat_drv_la_CPPFLAGS = -I$(srcdir)/contrib\
			$(EI_CPPFLAGS) -DEI_ENCODE_STRING_BUG\
			$(EXPAT_CPPFLAGS)
expat_drv_la_LDFLAGS = -module -avoid-version\
		       $(EI_LDFLAGS) $(EI_LIBS)\
		       $(EXPAT_LDFLAGS) $(EXPAT_LIBS)
expat_drv_la_SOURCES = contrib/hashtable.c\
		       expat_drv.c

EXTRA_DIST = contrib/hashtable.h contrib/hashtable_private.h \
	     xmpp_ns.h.awk xmpp_ns.h.in \
	     xmpp_names.h.awk xmpp_names.h.in \
	     xmpp_attrs.h.awk xmpp_attrs.h.in
BUILT_SOURCES = xmpp_ns.h xmpp_names.h xmpp_attrs.h
CLEANFILES = xmpp_ns.h xmpp_names.h xmpp_attrs.h

xmpp_ns.h: $(srcdir)/xmpp_ns.h.awk $(srcdir)/xmpp_ns.h.in
	$(AWK) -f $(srcdir)/xmpp_ns.h.awk $(srcdir)/xmpp_ns.h.in > $@

xmpp_names.h: $(srcdir)/xmpp_names.h.awk $(srcdir)/xmpp_names.h.in
	$(AWK) -f $(srcdir)/xmpp_names.h.awk $(srcdir)/xmpp_names.h.in > $@

xmpp_attrs.h: $(srcdir)/xmpp_attrs.h.awk $(srcdir)/xmpp_attrs.h.in
	$(AWK) -f $(srcdir)/xmpp_attrs.h.awk $(srcdir)/xmpp_attrs.h.in > $@

# iconv port driver
iconv_erl_la_CPPFLAGS = $(EI_CPPFLAGS) $(LIBICONV_CPPFLAGS)
iconv_erl_la_LDFLAGS = -module -avoid-version\
		       $(EI_LDFLAGS) $(EI_LIBS)\
		       $(LIBICONV_LDFLAGS) $(LTLIBICONV)
iconv_erl_la_SOURCES = iconv_erl.c

# Stringprep
stringprep_drv_la_CPPFLAGS = $(EI_CPPFLAGS)
stringprep_drv_la_LDFLAGS = -module -avoid-version\
			    $(EI_LDFLAGS) $(EI_LIBS)
stringprep_drv_la_SOURCES = uni_data.c uni_norm.c stringprep_drv.c

# TLS port driver
tls_drv_la_CPPFLAGS = $(EI_CPPFLAGS)
tls_drv_la_LDFLAGS = -module -avoid-version\
		     $(EI_LDFLAGS) $(EI_LIBS)
tls_drv_la_SOURCES = tls_drv.c
