% $Id: testsuite.erl,v 1.1 2005/12/13 14:02:20 dumbbell Exp $

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
		Ref -> ok;
		_   -> fail({no_match, Ref, Result})
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
	erlang:error(Reason).
