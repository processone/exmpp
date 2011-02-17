VERSION = 0.9.5-30-gf14f8a9
beam_DATA = exmpp.app exmpp.appup exmpp.beam exmpp_caps.beam \
	exmpp_compress.beam exmpp_dialback.beam exmpp_dns.beam \
	exmpp_internals.beam exmpp_utils.beam exmpp_iq.beam \
	exmpp_jid.beam exmpp_message.beam exmpp_presence.beam \
	exmpp_stanza.beam exmpp_stream.beam exmpp_stringprep.beam \
	exmpp_sup.beam exmpp_tls.beam exmpp_xml.beam \
	exmpp_xmlstream.beam exmpp_known_nss.beam \
	exmpp_known_elems.beam exmpp_known_attrs.beam \
	exmpp_client_binding.beam exmpp_client_compression.beam \
	exmpp_client_disco.beam exmpp_client_legacy_auth.beam \
	exmpp_client_privacy.beam exmpp_client_pubsub.beam \
	exmpp_client_register.beam exmpp_client_roster.beam \
	exmpp_client_sasl.beam exmpp_client_session.beam \
	exmpp_client_tls.beam exmpp_client_muc.beam \
	exmpp_server_binding.beam exmpp_server_compression.beam \
	exmpp_server_legacy_auth.beam exmpp_server_privacy.beam \
	exmpp_server_sasl.beam exmpp_server_session.beam \
	exmpp_server_tls.beam exmpp_sasl_digest.beam \
	exmpp_session.beam exmpp_socket.beam exmpp_bosh.beam \
	exmpp_component.beam

COMPAT_MODULES = %
COMPAT_MODULES_START =  % Compatibility modules disabled:

ifdef BUILD_COMPAT
BEAM_FILES += ejabberd_socket.beam					\
	      ejabberd_zlib.beam					\
	      stringprep.beam						\
	      tls.beam							\
	      xml.beam							\
	      xml_stream.beam
COMPAT_MODULES = 
COMPAT_MODULES_START =  % Compatibility modules enabled:
endif

DATA = $(beam_DATA)

ifdef ENABLE_EXAMPLES
examplesdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/examples/ebin
examples_DATA = echo_client.beam
DATA += $(examples_DATA)
endif

CONFIG_CLEAN_FILES = exmpp.app exmpp.appup
ERLANG_INSTALL_LIB_DIR = /usr/lib/erlang/lib
beamdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/ebin
ERLANG_INSTALL_LIB_DIR_exmpp = ${ERLANG_INSTALL_LIB_DIR}/exmpp-${VERSION}
MKDIR_P = /bin/mkdir -p
STRIP = strip
SED = /bin/sed
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_PROGRAM = ${INSTALL}
INSTALL_STRIP_PROGRAM = $(INSTALL) -s
NORMAL_INSTALL = :
NORMAL_UNINSTALL = :
am__strip_dir = `echo $$p | sed -e 's|^.*/||'`;


all: Makefile $(DATA) $(CONFIG_CLEAN_FILES)

exmpp.app: exmpp.app.in Makefile 
	$(SED) 's/@VERSION@/$(VERSION)/; s/@COMPAT_MODULES@/$(COMPAT_MODULES)/; s/@COMPAT_MODULES_START@/$(COMPAT_MODULES_START)/' $< > $@
exmpp.appup: exmpp.appup.in Makefile 
	$(SED) 's/@VERSION@/$(VERSION)/' $< > $@

clean-generic:
	-test -z "$(DATA)" || rm -f $(DATA)

clean-libtool:
	-rm -f *.lo
	-rm -rf .libs _libs

clean: clean-generic clean-libtool

distclean: clean
	-test -z "$(CONFIG_CLEAN_FILES)" || rm -f $(CONFIG_CLEAN_FILES)



install-beamDATA: $(beam_DATA)
	@$(NORMAL_INSTALL)
	test -z "$(beamdir)" || $(MKDIR_P) "$(DESTDIR)$(beamdir)"
	@list='$(beam_DATA)'; for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f=$(am__strip_dir) \
	  echo " $(INSTALL_DATA) '$$d$$p' '$(DESTDIR)$(beamdir)/$$f'"; \
	  $(INSTALL_DATA) "$$d$$p" "$(DESTDIR)$(beamdir)/$$f"; \
	done
uninstall-beamDATA:
	@$(NORMAL_UNINSTALL)
	@list='$(beam_DATA)'; for p in $$list; do \
	  f=$(am__strip_dir) \
	  echo " rm -f '$(DESTDIR)$(beamdir)/$$f'"; \
	  rm -f "$(DESTDIR)$(beamdir)/$$f"; \
	done
install-examplesDATA: $(examples_DATA)
	@$(NORMAL_INSTALL)
	test -z "$(examplesdir)" || $(MKDIR_P) "$(DESTDIR)$(examplesdir)"
	@list='$(examples_DATA)'; for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f=$(am__strip_dir) \
	  echo " $(INSTALL_DATA) '$$d$$p' '$(DESTDIR)$(examplesdir)/$$f'"; \
	  $(INSTALL_DATA) "$$d$$p" "$(DESTDIR)$(examplesdir)/$$f"; \
	done
uninstall-examplesDATA:
	@$(NORMAL_UNINSTALL)
	@list='$(examples_DATA)'; for p in $$list; do \
	  f=$(am__strip_dir) \
	  echo " rm -f '$(DESTDIR)$(examplesdir)/$$f'"; \
	  rm -f "$(DESTDIR)$(examplesdir)/$$f"; \
	done
installdirs:
	for dir in "$(DESTDIR)$(beamdir)" "$(DESTDIR)$(examplesdir)"; do \
	  test -z "$$dir" || $(MKDIR_P) "$$dir"; \
	done
install-data: install-beamDATA install-examplesDATA
install: all
	@$(MAKE) $(AM_MAKEFLAGS) install-data
uninstall: uninstall-beamDATA uninstall-examplesDATA


installcheck: installcheck-am
install-strip:
	$(MAKE) $(AM_MAKEFLAGS) INSTALL_PROGRAM="$(INSTALL_STRIP_PROGRAM)" \
	  install_sh_PROGRAM="$(INSTALL_STRIP_PROGRAM)" INSTALL_STRIP_FLAG=-s \
	  `test -z '$(STRIP)' || \
	    echo "INSTALL_PROGRAM_ENV=STRIPPROG='$(STRIP)'"` install


.MAKE: install install-strip

.PHONY: all clean clean-generic clean-libtool distclean \
	install install-beamDATA install-data install-examplesDATA \
	installdirs install-strip installcheck \
	uninstall-beamDATA uninstall-examplesDATA

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:

