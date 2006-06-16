% $Id$

{application, exmpp, [
	{description, "XMPP/Jabber protocol oriented XML library"},
	{vsn, "0.2.0"},
	{modules, [
		exmpp,
		exmpp_xml,
		exmpp_xmlstream
	]},
	{applications, [kernel, stdlib]},
]}.

% vim:ft=erlang:ts=8:
