% $Id$

-module(testsuite).

-export([run/1]).
-export([ok/1, is/2]).
-export([pass/0, skip/0, fail/0]).

% --------------------------------------------------------------------
% Run.
% --------------------------------------------------------------------

run(Fun) ->
    try
        Fun(),
        testsuite:pass()
    catch
        throw:skip ->
            io:format("SKIP~n");
        throw:Exception ->
            case Exception of
                failure -> ok;
                _       -> io:format("Exception:~n~p~n~n", [Exception])
            end,
            print_stacktrace(),
            io:format("FAIL~n")
    end.

print_stacktrace() ->
    ST = erlang:get_stacktrace(),
    io:format("Stack trace:~n"),
    print_stacktrace2(ST, 0).

print_stacktrace2([{M, F, A} | Rest], Count) ->
    io:format("  ~b.  ~s:~s/~b~n", [Count, M, F, A]),
    print_stacktrace2(Rest, Count + 1);
print_stacktrace2([], _Count) ->
    ok.

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
            io:format("Doesn't match:~nReference: ~p~nResult: ~p~n",
              [Ref, Result]),
            fail()
    end.

% --------------------------------------------------------------------
% Results.
% --------------------------------------------------------------------

pass() ->
    io:format("PASS~n").

skip() ->
    fail(skip).

fail() ->
    fail(failure).

fail(Reason) ->
    throw(Reason).
