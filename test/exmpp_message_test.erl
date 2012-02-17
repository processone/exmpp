-module(exmpp_message_test).

-include_lib("eunit/include/eunit.hrl").

-include("exmpp.hrl").


normal_test() ->
	M = exmpp_message:normal(),
	?assertMatch({xmlel, <<"message">>, _ ,_}, M),
	?assertEqual(<<"normal">>, exxml:get_attr(M, <<"type">>)),
	ok.
normal1_test() ->
	M = exmpp_message:normal(<<"hello">>),
	?assertMatch({xmlel, <<"message">>, _ ,[_|_]}, M),
	?assertEqual(<<"normal">>, exxml:get_attr(M, <<"type">>)),
	?assertEqual(<<"hello">>, exxml:get_path(M, [{element, <<"body">>}, cdata])),
	ok.
normal2_test() ->
	M = exmpp_message:normal(<<"subject">>,<<"hello">>),
	?assertMatch({xmlel, <<"message">>, _ ,[_|_]}, M),
	?assertEqual(<<"normal">>, exxml:get_attr(M, <<"type">>)),
	?assertEqual(<<"hello">>, exxml:get_path(M, [{element, <<"body">>}, cdata])),
	?assertEqual(<<"subject">>, exxml:get_path(M, [{element, <<"subject">>}, cdata])),
	ok.

chat_test() ->
	M = exmpp_message:chat(),
	?assertMatch({xmlel, <<"message">>, _ ,_}, M),
	?assertEqual(<<"chat">>, exxml:get_attr(M, <<"type">>)),
	ok.
chat1_test() ->
	M = exmpp_message:chat(<<"hello">>),
	?assertMatch({xmlel, <<"message">>, _ ,[_|_]}, M),
	?assertEqual(<<"chat">>, exxml:get_attr(M, <<"type">>)),
	?assertEqual(<<"hello">>, exxml:get_path(M, [{element, <<"body">>}, cdata])),
	ok.
chat2_test() ->
	M = exmpp_message:chat(<<"subject">>,<<"hello">>),
	?assertMatch({xmlel, <<"message">>, _ ,[_|_]}, M),
	?assertEqual(<<"chat">>, exxml:get_attr(M, <<"type">>)),
	?assertEqual(<<"hello">>, exxml:get_path(M, [{element, <<"body">>}, cdata])),
	?assertEqual(<<"subject">>, exxml:get_path(M, [{element, <<"subject">>}, cdata])),
	ok.
groupchat_test() ->
	M = exmpp_message:groupchat(),
	?assertMatch({xmlel, <<"message">>, _ ,_}, M),
	?assertEqual(<<"groupchat">>, exxml:get_attr(M, <<"type">>)),
	ok.
groupchat1_test() ->
	M = exmpp_message:groupchat(<<"hello">>),
	?assertMatch({xmlel, <<"message">>, _ ,[_|_]}, M),
	?assertEqual(<<"groupchat">>, exxml:get_attr(M, <<"type">>)),
	?assertEqual(<<"hello">>, exxml:get_path(M, [{element, <<"body">>}, cdata])),
	ok.
groupchat2_test() ->
	M = exmpp_message:groupchat(<<"subject">>,<<"hello">>),
	?assertMatch({xmlel, <<"message">>, _ ,[_|_]}, M),
	?assertEqual(<<"groupchat">>, exxml:get_attr(M, <<"type">>)),
	?assertEqual(<<"hello">>, exxml:get_path(M, [{element, <<"body">>}, cdata])),
	?assertEqual(<<"subject">>, exxml:get_path(M, [{element, <<"subject">>}, cdata])),
	ok.
headline_test() ->
	M = exmpp_message:headline(),
	?assertMatch({xmlel, <<"message">>, _ ,_}, M),
	?assertEqual(<<"headline">>, exxml:get_attr(M, <<"type">>)),
	ok.
headline1_test() ->
	M = exmpp_message:headline(<<"hello">>),
	?assertMatch({xmlel, <<"message">>, _ ,[_|_]}, M),
	?assertEqual(<<"headline">>, exxml:get_attr(M, <<"type">>)),
	?assertEqual(<<"hello">>, exxml:get_path(M, [{element, <<"body">>}, cdata])),
	ok.
headline2_test() ->
	M = exmpp_message:headline(<<"subject">>,<<"hello">>),
	?assertMatch({xmlel, <<"message">>, _ ,[_|_]}, M),
	?assertEqual(<<"headline">>, exxml:get_attr(M, <<"type">>)),
	?assertEqual(<<"hello">>, exxml:get_path(M, [{element, <<"body">>}, cdata])),
	?assertEqual(<<"subject">>, exxml:get_path(M, [{element, <<"subject">>}, cdata])),
	ok.


error_test() ->
	E = exmpp_message:error(exmpp_message:chat(<<"hello">>), <<"forbidden">>),
	?assertEqual(<<"error">>, exxml:get_attr(E, <<"type">>)),
	?assertMatch({xmlel, <<"message">>, _, _}, E),
	?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-stanzas">>, 
		exxml:get_path(E, [{element, <<"error">>}, {element, <<"forbidden">>}, {attribute, <<"xmlns">>}])),
	ok.

is_message_test() ->
	?assert(exmpp_message:is_message(exmpp_message:normal())),
	ok.

get_type_test() ->
	?assertEqual(<<"normal">>, exmpp_message:get_type(exmpp_message:normal())),
	ok.

set_type_test() ->
	M = exmpp_message:normal(),
	?assertEqual(<<"chat">>, exmpp_message:get_type(exmpp_message:set_type(M, <<"chat">>))),
	ok.

get_subject_test() ->
	M = exmpp_message:normal(<<"subject">>, <<"body">>),
	?assertEqual(<<"subject">>, exmpp_message:get_subject(M)),
	ok.

set_subject_test() ->
	M = exmpp_message:normal(<<"subject">>, <<"body">>),
	?assertEqual(<<"a">>, exmpp_message:get_subject(exmpp_message:set_subject(M, <<"a">>))),
	ok.

get_body_test() ->
	M = exmpp_message:normal(<<"subject">>, <<"body">>),
	?assertEqual(<<"body">>, exmpp_message:get_body(M)),
	ok.

set_body_test() ->
	M = exmpp_message:normal(<<"subject">>, <<"body">>),
	?assertEqual(<<"a">>, exmpp_message:get_body(exmpp_message:set_body(M, <<"a">>))),
	ok.

get_thread_test() ->
	M = exmpp_message:normal(<<"subject">>, <<"body">>),
	?assertEqual(undefined, exmpp_message:get_thread(M)),
	ok.

set_thread_test() ->
	M = exmpp_message:normal(<<"subject">>, <<"body">>),
	?assertEqual(<<"a">>, exmpp_message:get_thread(exmpp_message:set_thread(M, <<"a">>))),
	ok.
