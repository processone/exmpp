% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides utilities to prepare
%% common XMPP stanzas.
%%
%% <h3>Authentication</h3>
%%
%% <p>
%% In its current version, {@module} implements only the legacy
%% authentication mechanism.
%% </p>
%%
%% <p>
%% Again, a common use case is presented in <em>table 2</em>.
%% </p>
%% <table class="illustration">
%% <caption>Table 1: stream opening and closing</caption>
%% <tr>
%% <th>Client-side</th>
%% <th>Server-side</th>
%% </tr>
%% <tr>
%% <td>
%% <p>
%% Once a stream is opened, the client call `{@module}':
%% </p>
%% <pre>Request = exmpp_factory:legacy_auth_request("jabber.example.com").</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;iq xmlns="jabber:client" type="get" to="jabber.example.com"
%%   id="auth-1905181425"&gt;
%%       &lt;query xmlns="jabber:iq:auth"/&gt;<br/>&lt;/iq&gt;</pre>
%% </td>
%% <td></td>
%% </tr>
%% <tr>
%% <td></td>
%% <td>
%% <p>
%% The server answer with the available fields:
%% </p>
%% <pre>Request_Id = exmpp_xml:get_attribute(Request, 'id'),<br/>Fields = exmpp_factory:legacy_auth_fields(Request_Id).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;iq xmlns="jabber:client" type="result" id="auth-1905181425"&gt;
%%       &lt;query xmlns="jabber:iq:auth"&gt;
%%               &lt;username/&gt;
%%               &lt;password/&gt;
%%               &lt;digest/&gt;
%%               &lt;resource/&gt;
%%       &lt;/query&gt;<br/>&lt;/iq&gt;</pre>
%% <p>
%% At this time, this function doesn't offer the possibility to choose
%% which field to include (one may not want to propose `<password/>' for
%% instance). And because {@link exmpp_xml} doesn't provide a function
%% to remove element yet, the only way to achieve this is to walk
%% through the children and remove it by hand.
%% </p>
%% </td>
%% </tr>
%% <tr>
%% <td>
%% <p>
%% The client can send its credentials; he choose `<digest/>':
%% </p>
%% <pre>Password = exmpp_factory:legacy_auth_password_digest(
%%   "johndoe",
%%   "foobar!",
%%   "home"<br/>).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;q xmlns="jabber:client" type="set" id="auth-3105434037"&gt;
%%       &lt;query xmlns="jabber:iq:auth"&gt;
%%               &lt;username&gt;johndoe&lt;/username&gt;
%%               &lt;digest&gt;
%%                       93fdad2a795c59c73a6acf68a4dbdd3ddb366239
%%               &lt;/digest&gt;
%%               &lt;resource&gt;home&lt;/resource&gt;
%%       &lt;/query&gt;<br/>&lt;/iq&gt;</pre>
%% </td>
%% <td></td>
%% </tr>
%% <tr>
%% <td></td>
%% <td>
%% <p>
%% If the password is correct, the server notify the client:
%% </p>
%% <pre>Password_Id = exmpp_xml:get_attribute(Password, 'id'),<br/>Success = exmpp_factory:legacy_auth_success(Password_Id).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;iq xmlns="jabber:client" type="result" id="auth-3105434037"/&gt;</pre>
%% </td>
%% </tr>
%% </table>

-module(exmpp_factory).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
	features_announcement/1,
	tls_support_announce/1,
	compress_support_announce/1
]).
-export([
	legacy_auth_request/1,
	legacy_auth_request/2,
	legacy_auth_request_with_user/2,
	legacy_auth_request_with_user/3,
	legacy_auth_fields/1,
	legacy_auth_password/3,
	legacy_auth_password/4,
	legacy_auth_password_digest/3,
	legacy_auth_password_digest/4,
	legacy_auth_success/1,
	legacy_auth_failure/2
]).

% --------------------------------------------------------------------
% Features anouncement.
% --------------------------------------------------------------------

features_announcement(Features) ->
	#xmlnselement{
		ns = ?NS_XMPP,
		name = 'features',
		children = Features
	}.

tls_support_announce(Required) ->
	Announce = #xmlnselement{
		ns = ?NS_TLS,
		name = 'starttls',
		children = []
	},
	case Required of
		true ->
			Child = #xmlnselement{
				ns = ?NS_TLS,
				name = 'required',
				children = []
			},
			exmpp_xml:append_child(Announce, Child);
		_ ->
			Announce
	end.

compress_support_announce(Method) ->
	Method_El = #xmlnselement{
		ns = ?NS_COMPRESS,
		name = 'method',
		children = [#xmlcdata{cdata = Method}]
	},
	#xmlnselement{
		ns = ?NS_COMPRESS,
		name = 'compression',
		children = [Method_El]
	}.

% --------------------------------------------------------------------
% Authentication elements.
% --------------------------------------------------------------------

%% @spec (To) -> Auth_Iq
%%     To = string()
%%     Auth_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for requesting legacy authentication.
%%
%% The stanza `id' is generated automatically.

legacy_auth_request(To) ->
	legacy_auth_request(auth_id(), To).

%% @spec (Id, To) -> Auth_Iq
%%     Id = string()
%%     To = string()
%%     Auth_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for requesting legacy authentication.

legacy_auth_request(Id, To) ->
	% Make empty query.
	Query = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'query',
	    children = []},
	% Make IQ.
	Iq = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'}, [
	    {'type', "get"},
	    {'to',   To},
	    {'id',   Id}]),
	exmpp_xml:append_child(Iq, Query).

%% @spec (To, Username) -> Auth_Iq
%%     To = string()
%%     Username = string()
%% @doc Make an `<iq>' for requesting legacy authentication.
%%
%% The stanza `id' is generated automatically.

legacy_auth_request_with_user(To, Username) ->
	legacy_auth_request_with_user(auth_id(), To, Username).

%% @spec (Id, To, Username) -> Auth_Iq
%%     Id = string()
%%     To = string()
%%     Username = string()
%%     Auth_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for requesting legacy authentication.

legacy_auth_request_with_user(Id, To, Username_S) ->
	% Make empty query.
	Username = exmpp_xml:set_cdata(
	    #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'username'},
	    Username_S),
	Query = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'query',
	    children = [Username]},
	% Make IQ.
	Iq = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'}, [
	    {'type', "get"},
	    {'to',   To},
	    {'id',   Id}]),
	exmpp_xml:append_child(Iq, Query).

%% @spec (Id) -> Auth_Iq
%%     Id = string()
%%     Auth_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for advertising fields.

legacy_auth_fields(Id) ->
	% Prepare fields.
	Username = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'username',
	    children = []},
	Password = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'password',
	    children = []},
	Digest = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'digest',
	    children = []},
	Resource = #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'resource',
	    children = []},
	% Make query.
	Query = exmpp_xml:append_children(
	    #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'query'},
	    [Username, Password, Digest, Resource]),
	% Make IQ.
	Iq = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'}, [
	    {'type', "result"},
	    {'id',   Id}]),
	exmpp_xml:append_child(Iq, Query).

%% @spec (Username, Password, Resource) -> Auth_Iq
%%     Username = string()
%%     Password = string() | nil()
%%     Resource = string()
%%     Auth_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% The stanza `id' is generated automatically.

legacy_auth_password(Username_S, Password_S, Resource_S) ->
	legacy_auth_password(auth_id(),
	    Username_S, Password_S, Resource_S).

%% @spec (Id, Username, Password, Resource) -> Auth_Iq
%%     Id = string()
%%     Username = string()
%%     Password = string() | nil()
%%     Resource = string()
%%     Auth_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% `Password' is in clear plain text in the stanza.
%%
%% For an anonymous authentication, `Password' may be the empty string.

legacy_auth_password(Id, Username_S, Password_S, Resource_S) ->
	% Fill fields.
	Username = exmpp_xml:set_cdata(
	    #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'username'},
	    Username_S),
	Password = exmpp_xml:set_cdata(
	    #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'password'},
	    Password_S),
	Resource = exmpp_xml:set_cdata(
	    #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'resource'},
	    Resource_S),
	% Make query.
	Query = exmpp_xml:set_children(
	    #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'query'},
	    [Username, Password, Resource]),
	% Make IQ.
	Iq = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'}, [
	    {'type', "set"},
	    {'id', Id}]),
	exmpp_xml:append_child(Iq, Query).

%% @spec (Username, Password, Resource) -> Auth_Iq
%%     Username = string()
%%     Password = string()
%%     Resource = string()
%%     Auth_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% The stanza `id' is generated automatically.

legacy_auth_password_digest(Username_S, Password_S, Resource_S) ->
	legacy_auth_password_digest(auth_id(),
	    Username_S, Password_S, Resource_S).

%% @spec (Id, Username, Password, Resource) -> Auth_Iq
%%     Id = string()
%%     Username = string()
%%     Password = string()
%%     Resource = string()
%%     Auth_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% `Password' is encoded as specified in the JEP-078.

legacy_auth_password_digest(Id, Username_S, Password_S, Resource_S) ->
	% Fill fields.
	Username = exmpp_xml:set_cdata(
	    #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'username'},
	    Username_S),
	Digest = exmpp_xml:set_cdata(
	    #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'digest'},
	    password_digest(Id, Password_S)),
	Resource = exmpp_xml:set_cdata(
	    #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'resource'},
	    Resource_S),
	% Make query.
	Query = exmpp_xml:set_children(
	    #xmlnselement{ns = ?NS_JABBER_AUTH, name = 'query'},
	    [Username, Digest, Resource]),
	% Make IQ.
	Iq = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'}, [
	    {'type', "set"},
	    {'id', Id}]),
	exmpp_xml:append_child(Iq, Query).

%% @spec (Id) -> Auth_Iq
%%     Id = string()
%%     Auth_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to notify a successfull authentication.

legacy_auth_success(Id) ->
	exmpp_xml:set_attributes(
	#xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq', children = []}, [
	    {'type', "result"},
	    {'id', Id}]).

%% @spec (Id, Reason) -> Auth_Iq
%%     Id = string()
%%     Reason = not_authorized | conflict | not_acceptable
%%     Auth_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' to notify a successfull authentication.

legacy_auth_failure(Id, Reason) ->
	% Make <error>.
	Error0 = #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'error'},
	Error = case Reason of
		not_authorized ->
			Child = #xmlnselement{ns = ?NS_XMPP_STANZAS,
			    name = 'not-authorized', children = []},
			exmpp_xml:append_child(
			    exmpp_xml:set_attributes(Error0, [
			    {'code', "401"},
			    {'type', "auth"}]), Child);
		conflict ->
			Child = #xmlnselement{ns = ?NS_XMPP_STANZAS,
			    name = 'conflict', children = []},
			exmpp_xml:append_child(
			    exmpp_xml:set_attributes(Error0, [
			    {'code', "409"},
			    {'type', "cancel"}]), Child);
		not_acceptable ->
			Child = #xmlnselement{ns = ?NS_XMPP_STANZAS,
			    name = 'not-acceptable', children = []},
			exmpp_xml:append_child(
			    exmpp_xml:set_attributes(Error0, [
			    {'code', "406"},
			    {'type', "modify"}]), Child)
	end,
	% Make IQ.
	Iq = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'}, [
	    {'type', "result"},
	    {'id', Id}]),
	exmpp_xml:append_child(Iq, Error).

%% @spec () -> Auth_ID
%%     Auth_ID = string()
%% @doc Generate a random authentication iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

auth_id() ->
	"auth-" ++ integer_to_list(random:uniform(65536 * 65536)).

%% @spec (Id, Passwd) -> Digest
%%     Id = string()
%%     Passwd = string()
%%     Digest = string()
%% @doc Produce a password digest for legacy auth, according to JEP-078.

password_digest(Id, Passwd) ->
	Token = Id ++ Passwd,
	crypto:start(),
	hex(binary_to_list(crypto:sha(Token))).

% --------------------------------------------------------------------
% Internal functions.
% --------------------------------------------------------------------

hex(L) when is_list(L) ->
	lists:flatten([hex(I) || I <- L]);
hex(I) when I > 16#f ->
	[hex0((I band 16#f0) bsr 4), hex0((I band 16#0f))];
hex(I) ->
	[$0, hex0(I)].

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I)  -> $0 +I.
