% $Id$

-module(check_coverity).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([check/0, do_check/0]).

check() ->
	do_check(),
	testsuite:pass().

do_check() ->
	{ok, [[Top_Srcdir]]} = init:get_argument(top_srcdir),
	{ok, [Tests]} = init:get_argument(tests),
	{ok, [Covered_Modules]} = init:get_argument(covered_modules),
	cover:start(),
	cover:compile_directory(Top_Srcdir ++ "/src", [
	    {i, Top_Srcdir ++ "/include"},
	    {d, 'WITH_DEPRECATED_API'}]),
	run_tests(Tests),
	print_coverage(Covered_Modules),
	cover:stop(),
	ok.

run_tests([Test | Rest]) ->
	Mod = list_to_atom(Test),
	Mod:do_check(),
	run_tests(Rest);
run_tests([]) ->
	ok.

print_coverage([Module | Rest]) ->
	Mod = list_to_atom(Module),
	{ok, {_Module, {Cov, Not_Cov}}} = cover:analyse(Mod, module),
	if
		Cov > 0; Not_Cov > 0 ->
			io:format("- ~s: ~.1f%#NL#",
			    [Mod, Cov * 100 / (Cov + Not_Cov)]);
		true ->
			io:format("- ~s: n/a~n#NL#", [Mod])
	end,
	cover:analyse_to_file(Mod, "cover_" ++ Module ++ ".html", [html]),
	print_coverage(Rest);
print_coverage([]) ->
	ok.
