# $Id$

lib_LTLIBRARIES = exmpp_stringprep.la

if WITH_EXPAT
lib_LTLIBRARIES += exmpp_expat.la
endif

if WITH_OPENSSL
lib_LTLIBRARIES += exmpp_tls_openssl.la
endif

if WITH_ZLIB
lib_LTLIBRARIES += exmpp_compress_zlib.la
endif

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

# Expat port driver.
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

# Stringprep port driver.
exmpp_stringprep_la_CPPFLAGS = $(EI_CPPFLAGS)
exmpp_stringprep_la_LDFLAGS = -module -avoid-version			\
			      $(ERL_DRIVER_LDFLAGS)			\
			      $(EI_LDFLAGS) $(EI_LIBS)
exmpp_stringprep_la_SOURCES = exmpp_stringprep.c

# OpenSSL TLS port driver.
exmpp_tls_openssl_la_CPPFLAGS = $(EI_CPPFLAGS)				\
				$(OPENSSL_CPPFLAGS)
exmpp_tls_openssl_la_LDFLAGS = -module -avoid-version			\
			       $(ERL_DRIVER_LDFLAGS)			\
			       $(EI_LDFLAGS) $(EI_LIBS)			\
			       $(OPENSSL_LDFLAGS) $(OPENSSL_LIBS)
exmpp_tls_openssl_la_SOURCES = exmpp_tls.h exmpp_tls.c			\
			       exmpp_tls_openssl.c

# Zlib compression port driver.
exmpp_compress_zlib_la_CPPFLAGS = $(EI_CPPFLAGS)			\
				  $(OPENSSL_CPPFLAGS)
exmpp_compress_zlib_la_LDFLAGS = -module -avoid-version			\
				 $(ERL_DRIVER_LDFLAGS)			\
				 $(EI_LDFLAGS) $(EI_LIBS)		\
				 $(OPENSSL_LDFLAGS) $(OPENSSL_LIBS)
exmpp_compress_zlib_la_SOURCES = exmpp_compress_zlib.c
