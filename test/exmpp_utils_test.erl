-module(exmpp_utils_test).

-include_lib("eunit/include/eunit.hrl").

strip_test() ->
	?assertEqual(<<"aa">>, exmpp_utils:strip(<<" \t aa\n ">>)).

random_test() ->
	?assert(is_binary(exmpp_utils:random_id())).
