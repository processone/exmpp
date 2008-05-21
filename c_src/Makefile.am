# $Id$

lib_LTLIBRARIES = exmpp_stringprep.la

if WITH_EXPAT
lib_LTLIBRARIES += exmpp_expat.la
endif

if WITH_OPENSSL
lib_LTLIBRARIES += exmpp_tls_openssl.la
endif

if BUILD_COMPAT

if WITH_ZLIB
lib_LTLIBRARIES += ejabberd_zlib_drv.la
endif

if WITH_ICONV
lib_LTLIBRARIES += iconv_erl.la
endif

if WITH_OPENSSL
lib_LTLIBRARIES += tls_drv.la
endif

endif # BUILD_COMPAT

EXTRA_DIST = contrib/hashtable.h contrib/hashtable_private.h		\
	     xmpp_ns.h.awk xmpp_ns.h.in					\
	     xmpp_names.h.awk xmpp_names.h.in				\
	     xmpp_attrs.h.awk xmpp_attrs.h.in				\
	     stringprep_uni_data.h stringprep_uni_norm.h

BUILT_SOURCES = xmpp_ns.h xmpp_names.h xmpp_attrs.h

CLEANFILES = xmpp_ns.h xmpp_names.h xmpp_attrs.h

EI_CPPFLAGS = -I$(ERLANG_ROOT_DIR)/usr/include				\
	      -I$(ERLANG_LIB_DIR_erl_interface)/include
EI_LDFLAGS = -L$(ERLANG_LIB_DIR_erl_interface)/lib

if HOST_WIN32
EI_LIBS = -lei_md
else
EI_LIBS = -lei_st
endif

libdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/priv/lib

# --------------------------------------------------------------------
# Supported port drivers.
# --------------------------------------------------------------------

# Expat port driver
xmpp_ns.h: $(srcdir)/xmpp_ns.h.awk $(srcdir)/xmpp_ns.h.in
	$(AWK) -f $(srcdir)/xmpp_ns.h.awk $(srcdir)/xmpp_ns.h.in > $@

xmpp_names.h: $(srcdir)/xmpp_names.h.awk $(srcdir)/xmpp_names.h.in
	$(AWK) -f $(srcdir)/xmpp_names.h.awk $(srcdir)/xmpp_names.h.in > $@

xmpp_attrs.h: $(srcdir)/xmpp_attrs.h.awk $(srcdir)/xmpp_attrs.h.in
	$(AWK) -f $(srcdir)/xmpp_attrs.h.awk $(srcdir)/xmpp_attrs.h.in > $@

exmpp_expat_la_CPPFLAGS = -I$(srcdir)/contrib				\
			  $(EI_CPPFLAGS) -DEI_ENCODE_STRING_BUG		\
			  $(EXPAT_CPPFLAGS)
exmpp_expat_la_LDFLAGS = -module -avoid-version				\
			 $(ERL_DRIVER_LDFLAGS)				\
			 $(EI_LDFLAGS) $(EI_LIBS)			\
			 $(EXPAT_LDFLAGS) $(EXPAT_LIBS)
exmpp_expat_la_SOURCES = contrib/hashtable.c				\
			 exmpp_expat.c

# Stringprep
exmpp_stringprep_la_CPPFLAGS = $(EI_CPPFLAGS)
exmpp_stringprep_la_LDFLAGS = -module -avoid-version			\
			      $(ERL_DRIVER_LDFLAGS)			\
			      $(EI_LDFLAGS) $(EI_LIBS)
exmpp_stringprep_la_SOURCES = exmpp_stringprep.c

# TLS port driver (OpenSSL)
exmpp_tls_openssl_la_CPPFLAGS = $(EI_CPPFLAGS)				\
				$(OPENSSL_CPPFLAGS)
exmpp_tls_openssl_la_LDFLAGS = -module -avoid-version			\
			       $(ERL_DRIVER_LDFLAGS)			\
			       $(EI_LDFLAGS) $(EI_LIBS)			\
			       $(OPENSSL_LDFLAGS) $(OPENSSL_LIBS)
exmpp_tls_openssl_la_SOURCES = exmpp_tls_openssl.c

# --------------------------------------------------------------------
# Port drivers which are not ported yet.
# --------------------------------------------------------------------

# ejabberd_zlib port driver
ejabberd_zlib_drv_la_CPPFLAGS = $(EI_CPPFLAGS)				\
				$(ZLIB_CPPFLAGS)
ejabberd_zlib_drv_la_LDFLAGS = -module -avoid-version			\
			       $(ERL_DRIVER_LDFLAGS)			\
			       $(EI_LDFLAGS) $(EI_LIBS)			\
			       $(ZLIB_LDFLAGS) $(ZLIB_LIBS)
ejabberd_zlib_drv_la_SOURCES = ejabberd_zlib_drv.c

# iconv port driver
iconv_erl_la_CPPFLAGS = $(EI_CPPFLAGS)					\
			$(ICONV_CPPFLAGS)
iconv_erl_la_LDFLAGS = -module -avoid-version				\
		       $(ERL_DRIVER_LDFLAGS)				\
		       $(EI_LDFLAGS) $(EI_LIBS)				\
		       $(ICONV_LDFLAGS) $(ICONV_LIBS)
iconv_erl_la_SOURCES = iconv_erl.c

# TLS port driver
tls_drv_la_CPPFLAGS = $(EI_CPPFLAGS)					\
		      $(OPENSSL_CPPFLAGS)
tls_drv_la_LDFLAGS = -module -avoid-version				\
		     $(ERL_DRIVER_LDFLAGS)				\
		     $(EI_LDFLAGS) $(EI_LIBS)				\
		     $(OPENSSL_LDFLAGS) $(OPENSSL_LIBS)
tls_drv_la_SOURCES = tls_drv.c
