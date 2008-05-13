% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> is a wrapper around {@link
%% crypto:sha/1}.

-module(sha).
-vsn('$Revision$').

-export([start/0, sha/1]).

%% @spec () -> ok
%% @doc Start the crypto server.
%% @see crypto:start/0.
start() ->
	crypto:start().

digit_to_xchar(D) when (D >= 0) and (D < 10) ->
	D + 48;
digit_to_xchar(D) ->
	D + 87.

%% @spec (Text) -> Checksum
%%     Text = string()
%%     Checksum = string()
sha(Text) ->
	Bin = crypto:sha(Text),
	lists:reverse(ints_to_rxstr(binary_to_list(Bin), [])).

ints_to_rxstr([], Res) ->
	Res;
ints_to_rxstr([N | Ns], Res) ->
	ints_to_rxstr(Ns, [digit_to_xchar(N rem 16),
	    digit_to_xchar(N div 16) | Res]).
