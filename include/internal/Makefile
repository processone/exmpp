srcdir = .
pkglibdir = $(libdir)/exmpp
subdir = include/internal
DIST_COMMON = $(internalheaders_HEADERS)
mkinstalldirs = $(install_sh) -d
CONFIG_CLEAN_FILES =
SOURCES =
DIST_SOURCES =
am__installdirs = "$(DESTDIR)$(internalheadersdir)"
HEADERS = $(internalheaders_HEADERS)
DISTFILES = $(DIST_COMMON) $(DIST_SOURCES) $(EXTRA_DIST)
AWK = awk
RLANG_INSTALL_LIB_DIR = /usr/lib/erlang/lib
ERLANG_INSTALL_LIB_DIR_exmpp = ${ERLANG_INSTALL_LIB_DIR}/exmpp-${VERSION}
ERLANG_LIB_DIR = /usr/lib/erlang/lib
ERLANG_LIB_DIR_erl_interface = /usr/lib/erlang/lib/erl_interface-3.7
ERLANG_ROOT_DIR = /usr/lib/erlang
includedir = ${prefix}/include
sysconfdir = ${prefix}/etc
top_builddir = ../..
top_srcdir = ../..
MAKE_SPECS_LIST = $(srcdir)/make-specs-list
EXTRACT_KNOWN = $(srcdir)/extract-known-from-specs
KNOWN_NSS_SOURCE = $(srcdir)/exmpp_known_nss.in
KNOWN_ELEMS_SOURCE = $(srcdir)/exmpp_known_elems.in
KNOWN_ATTRS_SOURCE = $(srcdir)/exmpp_known_attrs.in
EXTRA_DIST = $(MAKE_SPECS_LIST) $(EXTRACT_KNOWN)			\
	     $(KNOWN_NSS_SOURCE)					\
	     $(KNOWN_ELEMS_SOURCE)					\
	     $(KNOWN_ATTRS_SOURCE)					\
	     exmpp_known_nss.hrl.awk					\
	     exmpp_known_nss.hrl.js					\
	     exmpp_known_elems.hrl.awk					\
	     exmpp_known_elems.hrl.js					\
	     exmpp_known_attrs.hrl.awk					\
	     exmpp_known_attrs.hrl.js

BUILT_SOURCES = exmpp_known_nss.hrl					\
		exmpp_known_elems.hrl					\
		exmpp_known_attrs.hrl

internalheadersdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/include/internal
internalheaders_HEADERS = $(BUILT_SOURCES) 
CLEANFILES = $(BUILT_SOURCES)
all: $(BUILT_SOURCES)
	$(MAKE) $(AM_MAKEFLAGS) all-am

install-internalheadersHEADERS: $(internalheaders_HEADERS)
	@$(NORMAL_INSTALL)
	test -z "$(internalheadersdir)" || $(MKDIR_P) "$(DESTDIR)$(internalheadersdir)"
	@list='$(internalheaders_HEADERS)'; for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f=$(am__strip_dir) \
	  echo " $(INSTALL) '$$d$$p' '$(DESTDIR)$(internalheadersdir)/$$f'"; \
	  $(INSTALL) "$$d$$p" "$(DESTDIR)$(internalheadersdir)/$$f"; \
	done

uninstall-internalheadersHEADERS:
	@$(NORMAL_UNINSTALL)
	@list='$(internalheaders_HEADERS)'; for p in $$list; do \
	  f=$(am__strip_dir) \
	  echo " rm -f '$(DESTDIR)$(internalheadersdir)/$$f'"; \
	  rm -f "$(DESTDIR)$(internalheadersdir)/$$f"; \
	done

tags: TAGS

TAGS:  $(HEADERS) $(SOURCES)  $(TAGS_DEPENDENCIES) \
		$(TAGS_FILES) $(LISP)
	tags=; \
	here=`pwd`; \
	list='$(SOURCES) $(HEADERS)  $(LISP) $(TAGS_FILES)'; \
	unique=`for i in $$list; do \
	    if test -f "$$i"; then echo $$i; else echo $(srcdir)/$$i; fi; \
	  done | \
	  $(AWK) '{ files[$$0] = 1; nonempty = 1; } \
	      END { if (nonempty) { for (i in files) print i; }; }'`; \
	if test -z "$(ETAGS_ARGS)$$tags$$unique"; then :; else \
	  test -n "$$unique" || unique=$$empty_fix; \
	  $(ETAGS) $(ETAGSFLAGS) $(AM_ETAGSFLAGS) $(ETAGS_ARGS) \
	    $$tags $$unique; \
	fi
ctags: CTAGS
CTAGS:  $(HEADERS) $(SOURCES)  $(TAGS_DEPENDENCIES) \
		$(TAGS_FILES) $(LISP)
	tags=; \
	list='$(SOURCES) $(HEADERS)  $(LISP) $(TAGS_FILES)'; \
	unique=`for i in $$list; do \
	    if test -f "$$i"; then echo $$i; else echo $(srcdir)/$$i; fi; \
	  done | \
	  $(AWK) '{ files[$$0] = 1; nonempty = 1; } \
	      END { if (nonempty) { for (i in files) print i; }; }'`; \
	test -z "$(CTAGS_ARGS)$$tags$$unique" \
	  || $(CTAGS) $(CTAGSFLAGS) $(AM_CTAGSFLAGS) $(CTAGS_ARGS) \
	     $$tags $$unique

distclean-tags:
	-rm -f TAGS ID GTAGS GRTAGS GSYMS GPATH tags

distdir: $(DISTFILES)
	@srcdirstrip=`echo "$(srcdir)" | sed 's/[].[^$$\\*]/\\\\&/g'`; \
	topsrcdirstrip=`echo "$(top_srcdir)" | sed 's/[].[^$$\\*]/\\\\&/g'`; \
	list='$(DISTFILES)'; \
	  dist_files=`for file in $$list; do echo $$file; done | \
	  sed -e "s|^$$srcdirstrip/||;t" \
	      -e "s|^$$topsrcdirstrip/|$(top_builddir)/|;t"`; \
	case $$dist_files in \
	  */*) $(MKDIR_P) `echo "$$dist_files" | \
			   sed '/\//!d;s|^|$(distdir)/|;s,/[^/]*$$,,' | \
			   sort -u` ;; \
	esac; \
	for file in $$dist_files; do \
	  if test -f $$file || test -d $$file; then d=.; else d=$(srcdir); fi; \
	  if test -d $$d/$$file; then \
	    dir=`echo "/$$file" | sed -e 's,/[^/]*$$,,'`; \
	    if test -d $(srcdir)/$$file && test $$d != $(srcdir); then \
	      cp -pR $(srcdir)/$$file $(distdir)$$dir || exit 1; \
	    fi; \
	    cp -pR $$d/$$file $(distdir)$$dir || exit 1; \
	  else \
	    test -f $(distdir)/$$file \
	    || cp -p $$d/$$file $(distdir)/$$file \
	    || exit 1; \
	  fi; \
	done
check-am: all-am
check: $(BUILT_SOURCES)
	$(MAKE) $(AM_MAKEFLAGS) check-am
all-am: Makefile $(HEADERS)
installdirs:
	for dir in "$(DESTDIR)$(internalheadersdir)"; do \
	  test -z "$$dir" || $(MKDIR_P) "$$dir"; \
	done
install: $(BUILT_SOURCES)
	$(MAKE) $(AM_MAKEFLAGS) install-am
install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am

installcheck: installcheck-am
distclean: clean
	-test -z "$(CONFIG_CLEAN_FILES)" || rm -f $(CONFIG_CLEAN_FILES)

clean:
	-test -z "$(CLEANFILES)" || rm -f $(CLEANFILES)

uninstall: uninstall-internalheadersHEADERS

.MAKE: install-am install-strip

.PHONY: CTAGS GTAGS all all-am check check-am clean clean-generic \
	clean-libtool ctags distclean distclean-generic \
	html-am info info-am install install-am install-data \
	install-data-am install-dvi install-dvi-am install-exec \
	install-exec-am install-html install-html-am install-info \
	install-info-am install-internalheadersHEADERS install-man \
	tags uninstall uninstall-am uninstall-internalheadersHEADERS


# --------------------------------------------------------------------
# Known namespaces/names/attributes headers generation.
# --------------------------------------------------------------------

exmpp_known_nss.hrl: $(srcdir)/exmpp_known_nss.hrl.awk $(KNOWN_NSS_SOURCE)
	$(AWK) -f $(srcdir)/exmpp_known_nss.hrl.awk $(KNOWN_NSS_SOURCE) > $@

exmpp_known_elems.hrl: $(srcdir)/exmpp_known_elems.hrl.awk $(KNOWN_ELEMS_SOURCE)
	$(AWK) -f $(srcdir)/exmpp_known_elems.hrl.awk $(KNOWN_ELEMS_SOURCE) > $@

exmpp_known_attrs.hrl: $(srcdir)/exmpp_known_attrs.hrl.awk $(KNOWN_ATTRS_SOURCE)
	$(AWK) -f $(srcdir)/exmpp_known_attrs.hrl.awk $(KNOWN_ATTRS_SOURCE) > $@

# --------------------------------------------------------------------
# Known namespaces/names/attributes updates.
# --------------------------------------------------------------------

updateknown: $(MAKE_SPECS_LIST) $(EXTRACT_KNOWN)
	@$(MAKE_SPECS_LIST) | $(EXTRACT_KNOWN)				\
		$(KNOWN_NSS_SOURCE)					\
		$(KNOWN_NAMES_SOURCE)					\
		$(KNOWN_ATTRS_SOURCE)

.PHONY: updateknown
# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
