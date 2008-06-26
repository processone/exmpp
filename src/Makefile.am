# $Id$

EXTRA_DIST = core/exmpp.erl						\
	     core/exmpp_sup.erl						\
	     core/exmpp_internals.erl					\
	     core/exmpp_xml.erl						\
	     core/exmpp_xmlstream.erl					\
	     core/exmpp_stringprep.erl					\
	     core/exmpp_jid.erl						\
	     core/exmpp_stanza.erl					\
	     core/exmpp_stream.erl					\
	     core/exmpp_dialback.erl					\
	     core/exmpp_iq.erl						\
	     core/exmpp_message.erl					\
	     core/exmpp_presence.erl					\
	     core/exmpp_tls.erl						\
	     core/exmpp_compress.erl					\
	     network/exmpp_callbacks.erl				\
	     network/exmpp_session.erl					\
	     network/exmpp_ssl.erl					\
	     network/exmpp_tcp.erl					\
	     client/exmpp_client_binding.erl				\
	     client/exmpp_client_compression.erl			\
	     client/exmpp_client_legacy_auth.erl			\
	     client/exmpp_client_presence.erl				\
	     client/exmpp_client_privacy.erl				\
	     client/exmpp_client_pubsub.erl				\
	     client/exmpp_client_register.erl				\
	     client/exmpp_client_roster.erl				\
	     client/exmpp_client_sasl.erl				\
	     client/exmpp_client_session.erl				\
	     client/exmpp_client_tls.erl				\
	     server/exmpp_server_binding.erl				\
	     server/exmpp_server_compression.erl			\
	     server/exmpp_server_legacy_auth.erl			\
	     server/exmpp_server_privacy.erl				\
	     server/exmpp_server_sasl.erl				\
	     server/exmpp_server_session.erl				\
	     server/exmpp_server_tls.erl				\
	     compat/ejabberd_socket.erl					\
	     compat/ejabberd_zlib.erl					\
	     compat/jlib.erl						\
	     compat/stringprep.erl					\
	     compat/tls.erl						\
	     compat/xml.erl						\
	     compat/xml_stream.erl					\
	     examples/echo_client.erl

all-local:
	@$(ERL) -noshell -pa @top_builddir@/ebin			\
		-eval "case make:all() of				\
		         up_to_date -> halt(0);				\
		         Err ->						\
		           io:format(\"~p~n\", [Err]),			\
		           halt(1)					\
		       end"
