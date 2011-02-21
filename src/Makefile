AWK = awk
ERL = /usr/bin/erl
prefix = /usr/local
exec_prefix = ${prefix}
exp_ERLANG_INSTALL_LIB_DIR_exmpp = /usr/lib/erlang/lib/exmpp-0.9.5-2-g2bf2b5d
includedir = ${prefix}/include
libdir = ${exec_prefix}/lib
libexecdir = ${exec_prefix}/libexec
mkdir_p = /bin/mkdir -p
srcdir = .
top_builddir = ..
top_srcdir = ..

KNOWN_NSS_SOURCE = $(top_srcdir)/include/internal/exmpp_known_nss.in
KNOWN_ELEMS_SOURCE = $(top_srcdir)/include/internal/exmpp_known_elems.in
KNOWN_ATTRS_SOURCE = $(top_srcdir)/include/internal/exmpp_known_attrs.in
BUILT_SOURCES = exmpp_known_nss.erl					\
		exmpp_known_elems.erl					\
		exmpp_known_attrs.erl

EXTRA_DIST = core/exmpp.erl						\
	     core/exmpp_sup.erl						\
	     core/exmpp_internals.erl					\



all: $(BUILT_SOURCES)
	@$(ERL) +B -noshell -pa ../ebin			\
		-eval "case make:all() of				\
		         up_to_date -> halt(0);				\
		         Err ->						\
		           io:format(\"~p~n\", [Err]),			\
		           halt(1)					\
		       end"

install: all

.MAKE: all

.PHONY: all clean install uninstall

# --------------------------------------------------------------------
# Known namespaces/names/attributes headers generation.
# --------------------------------------------------------------------

exmpp_known_nss.erl: $(srcdir)/core/exmpp_known_nss.erl.awk $(KNOWN_NSS_SOURCE)
	$(AWK) -f $(srcdir)/core/exmpp_known_nss.erl.awk $(KNOWN_NSS_SOURCE) > $@

exmpp_known_elems.erl: $(srcdir)/core/exmpp_known_elems.erl.awk $(KNOWN_ELEMS_SOURCE)
	$(AWK) -f $(srcdir)/core/exmpp_known_elems.erl.awk $(KNOWN_ELEMS_SOURCE) > $@

exmpp_known_attrs.erl: $(srcdir)/core/exmpp_known_attrs.erl.awk $(KNOWN_ATTRS_SOURCE)
	$(AWK) -f $(srcdir)/core/exmpp_known_attrs.erl.awk $(KNOWN_ATTRS_SOURCE) > $@

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
