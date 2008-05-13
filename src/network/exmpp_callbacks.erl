%%% File    : exmpp_callbacks.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Description : Default exmpp callbacks
%%% This code is copyright Process-one (http://www.process-one.net/)

-module(exmpp_callbacks).

-export([presence/5, message/7, iq/7]).

-define(INFO_MSG(Format, Args),
	error_logger:info_msg("I(~p:~p:~p): "++Format++"~n",
			      [self(),?MODULE,?LINE]++Args)).

%% TODO: Use State in callbacks ?

presence(XMPP, Type, _From, Attrs, Elts) ->
    ?INFO_MSG("Presence ~p: ~s (~p) (~p)", [XMPP, Type, Attrs, Elts]).

message(XMPP, Type, _From, _Subject, _Body, Attrs, Elts) ->
    ?INFO_MSG("Message ~p: ~s (~p) (~p)", [XMPP, Type, Attrs, Elts]).

iq(XMPP, Type, _From, _QueryNS, _PacketID, Attrs, Elts) ->
    ?INFO_MSG("IQ ~p: ~s (~p) (~p)", [XMPP, Type, Attrs, Elts]).
