% $Id$

-module(testsuite).

-export([ok/1, is/2]).
-export([pass/0, skip/0, fail/0]).

% --------------------------------------------------------------------
% Tests.
% --------------------------------------------------------------------

ok(Result) ->
	is(Result, ok).

is(Result, Ref) ->
	case Result of
		Ref ->
			ok;
		_   ->
			%io:format(
			%    "Doesn't match:~nReference: ~p~nResult: ~p~n",
			%    [Ref, Result]),
			fail({no_match, Ref, Result})
	end.

% --------------------------------------------------------------------
% Résultat.
% --------------------------------------------------------------------

pass() ->
	io:format("PASS~n").

skip() ->
	io:format("SKIP~n").

fail() ->
	fail('FAIL').

fail(Reason) ->
	io:format("Aborting...~nReason: ~p~n", [Reason]),
	erlang:error(Reason).
