% $Id: $

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%% The module <strong>{@module}</strong> implements a simple XMPP echo client.
%%
%%
%% <p>
%% This is a example use of the exmpp framework.
%% </p>
%%
%% <p>Usage:
%%    {ok, session} = echo_client:start().
%%    echo_client:stop(Session).
%%
%% <p>This code is copyright Process-one (http://www.process-one.net/)</p>
%% 

-module(echo_client).

-include("exmpp.hrl").

-export([start/0, stop/1]).

%% exmpp_session callbacks:
-export([presence/5, message/7, iq/7]).

start() ->
    %% Start XMPP session: Needed to start service (Like
    %% exmpp_stringprep):
    MySession = exmpp_session:start(),
    %% Create XMPP ID (Session Key):
    MyJID = exmpp_jid:make_jid("echo", "localhost", random),
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(MySession, MyJID, "password"),
    %% Define callback module to use on incoming packets:
    exmpp_session:add_callback_module(MySession, ?MODULE),
    %% Connect in standard TCP:
    _StreamId = exmpp_session:connect_TCP(MySession, "localhost", 5222),
    session(MySession, MyJID),
    MySession.

session(MySession, MyJID) ->
    %% Login with defined JID / Authentication:
    try exmpp_session:login(MySession)
    catch
	throw:{auth_error, 'not-authorized'} ->
	    %% Try creating a new user:
	    io:format("Register~n",[]),
	    %% In a real life client, we should trap error case here
	    %% and print the correct message.
	    exmpp_session:register_account(MySession, "password"),
	    %% After registration, retry to login:
	    exmpp_session:login(MySession)
    end,
    %% We explicitely send presence:
    exmpp_session:send_packet(MySession,
			      exmpp_client_presence:presence(?P_AVAILABLE,
							     "Echo Ready")),
    MySession.

stop(Session) ->
    exmpp_session:stop(Session).

presence(XMPP, Type, _From, Attrs, Elts) ->
    io:format("Presence ~p: ~s (~p) (~p)", [XMPP, Type, Attrs, Elts]).

message(XMPP, Type, _From, _Subject, _Body, Attrs, Elts) ->
    io:format("Message ~p: ~s (~p) (~p)", [XMPP, Type, Attrs, Elts]).

iq(XMPP, Type, _From, _QueryNS, _PacketID, Attrs, Elts) ->
    io:format("IQ ~p: ~s (~p) (~p)", [XMPP, Type, Attrs, Elts]).
