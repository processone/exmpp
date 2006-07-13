% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_jlib).
-vsn('$Revision$').

-include("exmpp.hrl").

-export([
	exchange_attrs_from_and_to_in_list/1,
	exchange_attrs_from_and_to/1,
	rename_attr_to_to_from_in_list/1,
	rename_attr_to_to_from/1
]).

% Deprecated API
-export([get_iq_namespace/1, iq_query_info/1]).
-export([is_iq_request_type/1, iq_type_to_string/1, iq_to_xml/1]).

-export([make_result_iq_reply/1]).
-export([make_error_element/2, make_error_reply/2, make_error_reply/3]).
-export([make_correct_from_to_attrs/3]).
-export([replace_from_to_attrs/3, replace_from_to/3]).
-export([remove_attr/2]).

-export([is_nodename/1, tolower/1]).
-export([nodeprep/1, nameprep/1, resourceprep/1]).

-export([make_jid/1, make_jid/3]).
-export([string_to_jid/1, jid_to_string/1, jid_tolower/1]).
-export([jid_remove_resource/1, jid_remove_resource/1, jid_replace_resource/2]).

-export([parse_xdata_submit/1]).
-export([timestamp_to_iso/1, timestamp_to_xml/1]).
-export([now_to_utc_string/1, now_to_local_string/1]).
-export([datetime_string_to_timestamp/1]).
-export([decode_base64/1, encode_base64/1]).

% --------------------------------------------------------------------
% Helpers to handle common attribute operations.
% --------------------------------------------------------------------

%% @spec (Attrs) -> New_Attrs
%%     Attrs = [exmpp_xml:xmlnsattribute() | exmpp_xml:xmlattribute()]
%%     New_Attrs = [exmpp_xml:xmlnsattribute() | exmpp_xml:xmlattribute()]
%% @doc Exchange values of attributes `From' and `To' in the list.
%%
%% This function expects that names are encoded as atom().

exchange_attrs_from_and_to_in_list(Attrs) ->
	To = exmpp_xml:get_attribute_from_list(Attrs, 'to'),
	From = exmpp_xml:get_attribute_from_list(Attrs, 'from'),
	Attrs1 = exmpp_xml:set_attribute_in_list(Attrs, 'to', From),
	Attrs2 = exmpp_xml:set_attribute_in_list(Attrs1, 'from', To),
	Attrs2.

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = exmpp_xml:xmlnselement() | exmpp_xml:xmlelement()
%%     New_XML_Element = exmpp_xml:xmlnselement() | exmpp_xml:xmlelement()
%% @doc Exchange values of attributes `From' and `To'.
%%
%% This function expects that names are encoded as atom().

exchange_attrs_from_and_to(#xmlnselement{attrs = Attrs} = XML_Element) ->
	New_Attrs = exchange_attrs_from_and_to_in_list(Attrs),
	XML_Element#xmlnselement{attrs = New_Attrs};

exchange_attrs_from_and_to(#xmlelement{attrs = Attrs} = XML_Element) ->
	New_Attrs = exchange_attrs_from_and_to_in_list(Attrs),
	XML_Element#xmlelement{attrs = New_Attrs}.

%% @spec (Attrs) -> New_Attrs
%%     Attrs = [exmpp_xml:xmlnsattribute() | exmpp_xml:xmlattribute()]
%%     New_Attrs = [exmpp_xml:xmlnsattribute() | exmpp_xml:xmlattribute()]
%% @doc Rename `To' attribute to `From' in the list.
%%
%% This function expects that names are encoded as atom().

rename_attr_to_to_from_in_list(Attrs) ->
	To = exmpp_xml:get_attribute_from_list(Attrs, 'to'),
	Attrs1 = exmpp_xml:remove_attribute_from_list(Attrs, 'to'),
	Attrs2 = exmpp_xml:set_attribute_in_list(Attrs1, 'from', To),
	Attrs2.

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = exmpp_xml:xmlnselement() | exmpp_xml:xmlelement()
%%     New_XML_Element = exmpp_xml:xmlnselement() | exmpp_xml:xmlelement()
%% @doc Rename `To' attribute to `From'.
%%
%% This function expects that names are encoded as atom().

rename_attr_to_to_from(#xmlnselement{attrs = Attrs} = XML_Element) ->
	New_Attrs = rename_attr_to_to_from_in_list(Attrs),
	XML_Element#xmlnselement{attrs = New_Attrs};

rename_attr_to_to_from(#xmlelement{attrs = Attrs} = XML_Element) ->
	New_Attrs = rename_attr_to_to_from_in_list(Attrs),
	XML_Element#xmlelement{attrs = New_Attrs}.

% --------------------------------------------------------------------
% Helpers to handle IQ stanza.
% --------------------------------------------------------------------

%% @spec (IQ_Stanza) -> Namespace
%%     IQ_Stanza = exmpp_xml:xmlelement()
%%     Namespace = string()
get_iq_namespace(#xmlelement{name = Name, children = Els}) when Name == "iq" ->
	case exmpp_xml:remove_cdata(Els) of
		[#xmlelement{attrs = Attrs}] ->
			exmpp_xml:get_attr_s("xmlns", Attrs);
		_ ->
			""
	end;
get_iq_namespace(_) ->
	"".

%% @spec (IQ_Stanza) -> IQ | invalid | not_iq
%%     IQ_Stanza = exmpp_xml:xmlelement()
%%     IQ = iq()
iq_query_info(#xmlelement{name = Name, attrs = Attrs, children = Els})
    when Name == "iq" ->
	ID   = exmpp_xml:get_attr_s("id", Attrs),
	Type = exmpp_xml:get_attr_s("type", Attrs),
	Lang = exmpp_xml:get_attr_s("xml:lang", Attrs),
	Type1 = case Type of
		"set"    -> set;
		"get"    -> get;
		"result" -> reply;
		"error"  -> reply;
		_        -> invalid
	end,
	if
		(Type1 /= invalid) and (Type1 /= reply) ->
			case exmpp_xml:remove_cdata(Els) of
				[#xmlelement{attrs = Attrs2} = Sub_El] ->
					XMLNS = exmpp_xml:get_attr_s("xmlns",
					    Attrs2),
					if
						XMLNS /= "" ->
							#iq{id = ID,
							    type = Type1,
							    xmlns = XMLNS,
							    lang = Lang,
							    sub_el = Sub_El
							};
						true ->
							invalid
					end;
				_ ->
					invalid
			end;
		true ->
			Type1
	end;
iq_query_info(_) ->
	not_iq.

is_iq_request_type(set) -> true;
is_iq_request_type(get) -> true;
is_iq_request_type(_)   -> false.

iq_type_to_string(set)    -> "set";
iq_type_to_string(get)    -> "get";
iq_type_to_string(result) -> "result";
iq_type_to_string(error)  -> "error";
iq_type_to_string(_)      -> invalid.


iq_to_xml(#iq{id = ID, type = Type, sub_el = Sub_El}) ->
	if
		ID /= "" ->
			#xmlelement{
			    name = "iq",
			    attrs = [{"id", ID},
			        {"type", iq_type_to_string(Type)}],
			    children = Sub_El};
		true ->
			#xmlelement{
			    name = "iq",
			    attrs = [{"type", iq_type_to_string(Type)}],
			    children = Sub_El}
	end.

% --------------------------------------------------------------------
% Building standard response.
% --------------------------------------------------------------------

%% @spec (IQ_Stanza) -> IQ_Stanza_Reply
%%     IQ_Stanza = exmpp_xml:xmlelement()
%%     IQ_Stanza_Reply = exmpp_xml:xmlelement()
%% @doc Prepare a reply to an IQ stanza.
make_result_iq_reply(#xmlelement{attrs = Attrs} = IQ_Stanza) ->
	New_Attrs = make_result_iq_reply_attrs(Attrs),
	IQ_Stanza#xmlelement{attrs = New_Attrs}.

make_result_iq_reply_attrs(Attrs) ->
	Attrs1 = switch_to_and_from_attrs(Attrs),
	Attrs2 = exmpp_xml:replace_attr("type", "result", Attrs1),
	Attrs2.

%% @spec (Code, Description) -> Error_Element
%%     Code = string()
%%     Description = string() | binary()
%%     Error_Element = xmlelement()
%% @doc Build an error element from the given code and description.
make_error_element(Code, Desc) ->
	#xmlelement{
	    name  = "error",
	    attrs = [{"code", Code}],
	    children = [#xmlcdata{cdata = Desc}]}.

%% @spec (Stanza, Error) -> Stanza_Reply
%%     Stanza = exmpp_xml:xmlelement()
%%     Error = exmpp_xml:xmlelement()
%%     Stanza_Reply = exmpp_xml:xmlelement()
%% @doc Add an error element to a stanza.
make_error_reply(#xmlelement{attrs = Attrs} = Stanza, Error) ->
	New_Attrs = make_error_reply_attrs(Attrs),
	exmpp_xml:add_child(Stanza#xmlelement{attrs = New_Attrs}, Error).

make_error_reply_attrs(Attrs) ->
	Attrs1 = switch_to_and_from_attrs(Attrs),
	Attrs2 = exmpp_xml:replace_attr("type", "error", Attrs1),
	Attrs2.

%% @spec (Stanza, Code, Desc) -> Stanza_Reply
%%     Stanza = exmpp_xml:xmlelement()
%%     Code = string()
%%     Desc = string() | binary()
%%     Stanza_Reply = exmpp_xml:xmlelement()
%% @doc Prepare a error reply to a stanza with given code and description.
make_error_reply(Stanza, Code, Desc) ->
	Error = make_error_element(Code, Desc),
	make_error_reply(Stanza, Error).

%% @spec (From, To, Attrs) -> New_Attrs
%%     From = string()
%%     To = string()
%%     Attrs = [exmpp_xml:xmlattribute()]
%%     New_Attrs = [exmpp_xml:xmlattribute()]
make_correct_from_to_attrs(From, To, Attrs) ->
	Attrs1 = exmpp_xml:replace_attr("from", From, Attrs),
	Attrs2 = case exmpp_xml:get_attr("to", Attrs1) of
		{value, _Attr_Value} -> Attrs1;
		_                    -> [{"to", To} | Attrs1]
	end,
	Attrs2.

%% @spec (From, To, Attrs) -> New_Attrs
%%     From = string()
%%     To = string()
%%     Attrs = [exmpp_xml:xmlattribute()]
%%     New_Attrs = [exmpp_xml:xmlattribute()]
%% @doc Replace `from' and `to' attributes value by `From' and `To'
%% respectively.
replace_from_to_attrs(From, To, Attrs) ->
	Attrs1 = exmpp_xml:replace_attr("from", From, Attrs),
	Attrs2 = exmpp_xml:replace_attr("to", To, Attrs1),
	Attrs2.

%% @spec (From, To, Stanza) -> New_Stanza
%%     From = jid()
%%     To = jid()
%%     Stanza = exmpp_xml:xmlelement()
%%     New_Stanza = exmpp_xml:xmlelement()
%% @doc Replace `from' and `to' attributes value by `From' and `To'
%% respectively.
replace_from_to(From, To, #xmlelement{attrs = Attrs} = Stanza) ->
	New_Attrs = replace_from_to_attrs(
	    jid_to_string(From),
	    jid_to_string(To),
	    Attrs),
	Stanza#xmlelement{attrs = New_Attrs}.

switch_to_and_from_attrs(Attrs) ->
	% Get `from' and `to' values.
	To   = exmpp_xml:get_attribute_from_list("to", Attrs),
	From = exmpp_xml:get_attr("from", Attrs),
	% Remove them from the list.
	Attrs1 = exmpp_xml:remove_attr("to", Attrs),
	Attrs2 = exmpp_xml:remove_attr("from", Attrs1),
	% Set their value.
	Attrs3 = case To of
		{value, ToVal} -> [{"from", ToVal} | Attrs2];
		_              -> Attrs2
	end,
	Attrs4 = case From of
		{value, FromVal} -> [{"to", FromVal} | Attrs3];
		_                -> Attrs3
	end,
	Attrs4.

% --------------------------------------------------------------------
% Stringprep.
% --------------------------------------------------------------------

%% @spec (String) -> Lowercased_String
%%     String = string()
%%     Lowercased_String = string()
%% @doc Lowercase a string.
tolower([C | Cs]) ->
	if
		C >= $A, C =< $Z -> [C + 32 | tolower(Cs)];
		true             -> [C      | tolower(Cs)]
	end;
tolower([]) ->
	[].

%% @spec (Nodename) -> boolean()
%%     Nodename = string()
is_nodename([]) ->
	false;
is_nodename(J) ->
	nodeprep(J) /= error.

%% @spec (Nodename) -> New_Nodename | error
%%     Nodename = string()
%%     New_Nodename = string()
nodeprep(S) when length(S) < 1024 ->
	R = stringprep:nodeprep(S),
	if
		length(R) < 1024 -> R;
		true             -> error
	end;
nodeprep(_) ->
	error.

%% @spec (Name) -> New_Name | error
%%     Name = string()
%%     New_Name = string()
nameprep(S) when length(S) < 1024 ->
	R = stringprep:nameprep(S),
	if
		length(R) < 1024 -> R;
		true             -> error
	end;
nameprep(_) ->
	error.

%% @spec (Resource) -> New_Resource | error
%%     Resource = string()
%%     New_Resource = string()
resourceprep(S) when length(S) < 1024 ->
	R = stringprep:resourceprep(S),
	if
		length(R) < 1024 -> R;
		true             -> error
	end;
resourceprep(_) ->
	error.

% --------------------------------------------------------------------
% Handle JID.
% --------------------------------------------------------------------

make_jid({User, Server, Resource}) ->
	make_jid(User, Server, Resource).

make_jid(User, Server, Resource) ->
	case stringprep_jid({User, Server, Resource}) of
		{LUser, LServer, LResource} ->
			#jid{user = User,
			    server = Server,
			    resource = Resource,
			    luser = LUser,
			    lserver = LServer,
			    lresource = LResource
			};
		error ->
			error
	end.

stringprep_jid({User, Server, Resource}) ->
	case nodeprep(User) of
		error ->
			error;
		LUser ->
			case nameprep(Server) of
				error ->
					error;
				LServer ->
					case resourceprep(Resource) of
						error ->
							error;
						LRes ->
							{LUser, LServer, LRes}
					end
			end
	end.

%% @spec (String) -> Jid
%%     String = string()
%%     Jid = jid()
%% @doc Parse a JID string and make a {@link jid()}.
string_to_jid(J) ->
	string_to_jid1(J, "").

string_to_jid1([$@ | _J], "") ->
	error;
string_to_jid1([$@ | J], N) ->
	string_to_jid2(J, lists:reverse(N), "");
string_to_jid1([$/ | _J], "") ->
	error;
string_to_jid1([$/ | J], N) ->
	string_to_jid3(J, "", lists:reverse(N), "");
string_to_jid1([C | J], N) ->
	string_to_jid1(J, [C | N]);
string_to_jid1([], "") ->
	error;
string_to_jid1([], N) ->
	make_jid("", lists:reverse(N), "").

string_to_jid2([$/ | _J], _N, "") ->
	error;
string_to_jid2([$/ | J], N, S) ->
	string_to_jid3(J, N, lists:reverse(S), "");
string_to_jid2([C | J], N, S) ->
	string_to_jid2(J, N, [C | S]);
string_to_jid2([], _N, "") ->
	error;
string_to_jid2([], N, S) ->
	make_jid(N, lists:reverse(S), "").

string_to_jid3([C | J], N, S, R) ->
	string_to_jid3(J, N, S, [C | R]);
string_to_jid3([], N, S, R) ->
	make_jid(N, S, lists:reverse(R)).

%% @spec (Jid) -> String
%%     Jid = jid() | {User, Server, Resource}
%%     User = string()
%%     Server = string()
%%     Resource = string()
%% @doc Serialize a JID to plain text.
jid_to_string(#jid{user = User, server = Server, resource = Resource}) ->
	jid_to_string({User, Server, Resource});
jid_to_string({Node, Server, Resource}) ->
	S1 = case Node of
		"" -> "";
		_  -> Node ++ "@"
	end,
	S2 = S1 ++ Server,
	S3 = case Resource of
		"" -> S2;
		_  -> S2 ++ "/" ++ Resource
	end,
	S3.

%% @spec (Jid) -> {LUser, LServer, LResource}
%%     Jid = jid() | {User, Server, Resource}
%%     User = string()
%%     Server = string()
%%     Resource = string()
%%     LUser = string()
%%     LServer = string()
%%     LResource = string()
jid_tolower(#jid{luser = U, lserver = S, lresource = R}) ->
	{U, S, R};
jid_tolower({User, Server, Resource}) ->
	stringprep_jid({User, Server, Resource}).

%% @spec (Jid) -> New_Jid
%%     Jid = jid() | {User, Server, Resource}
%%     User = string()
%%     Server = string()
%%     Resource = string()
%%     New_Jid = jid() | {User, Server, nil()}
jid_remove_resource(#jid{} = JID) ->
	JID#jid{resource = "", lresource = ""};
jid_remove_resource({U, S, _R}) ->
	{U, S, ""}.

%% @spec (Jid, Resource) -> New_Jid | error
%%     Jid = jid()
%%     Resource = string()
%%     New_Jid = jid()
jid_replace_resource(JID, Resource) ->
	case resourceprep(Resource) of
		error ->
			error;
		LResource ->
			JID#jid{resource = Resource, lresource = LResource}
	end.

% --------------------------------------------------------------------
% .
% --------------------------------------------------------------------

parse_xdata_submit(#xmlelement{attrs = Attrs, children = Els}) ->
	case exmpp_xml:get_attr_s("type", Attrs) of
		"submit" ->
			lists:reverse(parse_xdata_fields(Els, []));
		_ ->
			invalid
	end.

parse_xdata_fields([], Res) ->
	Res;
parse_xdata_fields([#xmlelement{name = Name, attrs = Attrs,
    children = Sub_Els} | Els], Res) ->
	case Name of
		"field" ->
			case exmpp_xml:get_attr_s("var", Attrs) of
				"" ->
					parse_xdata_fields(Els, Res);
				Var ->
					Field =
					    {Var, lists:reverse(
					        parse_xdata_values(Sub_Els, [])
					    )},
					parse_xdata_fields(Els, [Field | Res])
			end;
		_ ->
			parse_xdata_fields(Els, Res)
	end;
parse_xdata_fields([_ | Els], Res) ->
	parse_xdata_fields(Els, Res).

parse_xdata_values([], Res) ->
	Res;
parse_xdata_values([#xmlelement{name = Name, children = Sub_Els} | Els], Res) ->
	case Name of
		"value" ->
			Val = exmpp_xml:get_cdata(Sub_Els),
			parse_xdata_values(Els, [Val | Res]);
		_ ->
			parse_xdata_values(Els, Res)
	end;
parse_xdata_values([_ | Els], Res) ->
	parse_xdata_values(Els, Res).

timestamp_to_iso({{Year, Month, Day}, {Hour, Minute, Second}}) ->
	lists:flatten(io_lib:format(
	    "~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",
	    [Year, Month, Day, Hour, Minute, Second])).

timestamp_to_xml({{Year, Month, Day}, {Hour, Minute, Second}}) ->
	#xmlelement{
	    name = "x",
	    attrs = [{"xmlns", ?NS_DELAY},
	        {"stamp", lists:flatten(io_lib:format(
		    "~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",
		    [Year, Month, Day, Hour, Minute, Second]))}],
	    children = []
	}.

now_to_utc_string({MegaSecs, Secs, MicroSecs}) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} =
	    calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
	lists:flatten(io_lib:format(
	    "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ",
	    [Year, Month, Day, Hour, Minute, Second, MicroSecs])).

now_to_local_string({MegaSecs, Secs, MicroSecs}) ->
	LocalTime = calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
	UTCTime = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
	Seconds = calendar:datetime_to_gregorian_seconds(LocalTime) -
	    calendar:datetime_to_gregorian_seconds(UTCTime),
	{{H, M, _}, Sign} = if
		Seconds < 0 -> {calendar:seconds_to_time(-Seconds), "-"};
		true        -> {calendar:seconds_to_time(Seconds), "+"}
	end,
	{{Year, Month, Day}, {Hour, Minute, Second}} = LocalTime,
	lists:flatten(io_lib:format(
	    "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0w~s~2..0w:~2..0w",
	    [Year, Month, Day, Hour, Minute, Second, MicroSecs, Sign, H, M])).

% yyyy-mm-ddThh:mm:ss[.sss]{Z|{+|-}hh:mm} -> {MegaSecs, Secs, MicroSecs}
datetime_string_to_timestamp(TimeStr) ->
	case catch parse_datetime(TimeStr) of
		{'EXIT', _Err} -> undefined;
		TimeStamp      -> TimeStamp
	end.

parse_datetime(TimeStr) ->
	[Date, Time] = string:tokens(TimeStr, "T"),
	D = parse_date(Date),
	{T, MS, TZH, TZM} = parse_time(Time),
	S = calendar:datetime_to_gregorian_seconds({D, T}),
	S1 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
	Seconds = (S - S1) - TZH * 60 * 60 - TZM * 60,
	{Seconds div 1000000, Seconds rem 1000000, MS}.

% yyyy-mm-dd
parse_date(Date) ->
	[Y, M, D] = string:tokens(Date, "-"),
	Date1 = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
	case calendar:valid_date(Date1) of
		true -> Date1;
		_    -> false
	end.

% hh:mm:ss[.sss]TZD
parse_time(Time) ->
	case string:str(Time, "Z") of
		0 ->
			parse_time_with_timezone(Time);
		_ ->
			[T | _] = string:tokens(Time, "Z"),
			{TT, MS} = parse_time1(T),
			{TT, MS, 0, 0}
	end.

parse_time_with_timezone(Time) ->
	case string:str(Time, "+") of
		0 ->
			case string:str(Time, "-") of
				0 -> false;
				_ -> parse_time_with_timezone(Time, "-")
			end;
		_ ->
			parse_time_with_timezone(Time, "+")
	end.

parse_time_with_timezone(Time, Delim) ->
	[T, TZ] = string:tokens(Time, Delim),
	{TZH, TZM} = parse_timezone(TZ),
	{TT, MS} = parse_time1(T),
	case Delim of
		"-" -> {TT, MS, -TZH, -TZM};
		"+" -> {TT, MS, TZH, TZM}
	end.

parse_timezone(TZ) ->
	[H, M] = string:tokens(TZ, ":"),
	{[H1, M1], true} = check_list([{H, 12}, {M, 60}]),
	{H1, M1}.

parse_time1(Time) ->
	[HMS | T] =  string:tokens(Time, "."),
	MS = case T of
		[]    -> 0;
		[Val] -> list_to_integer(string:left(Val, 6, $0))
	end,
	[H, M, S] = string:tokens(HMS, ":"),
	{[H1, M1, S1], true} = check_list([{H, 24}, {M, 60}, {S, 60}]),
	{{H1, M1, S1}, MS}.

check_list(List) ->
	lists:mapfoldl(
	fun({L, N}, B)->
		V = list_to_integer(L),
		if
			(V >= 0) and (V =< N) -> {V, B};
			true                  -> {false, false}
		end
	end, true, List).

%
% Base64 stuff (based on httpd_util.erl)
%

decode_base64(S) ->
	decode1_base64([C || C <- S,
	    C /= $ ,
	    C /= $\t,
	    C /= $\n,
	    C /= $\r]).

decode1_base64([]) ->
	[];
decode1_base64([Sextet1, Sextet2, $=, $= | Rest]) ->
	Bits2x6 =
		(d(Sextet1) bsl 18) bor
		(d(Sextet2) bsl 12),
	Octet1 = Bits2x6 bsr 16,
	[Octet1 | decode_base64(Rest)];
decode1_base64([Sextet1, Sextet2, Sextet3, $= | Rest]) ->
	Bits3x6 =
	    (d(Sextet1) bsl 18) bor
	    (d(Sextet2) bsl 12) bor
	    (d(Sextet3) bsl 6),
	Octet1 = Bits3x6 bsr 16,
	Octet2 = (Bits3x6 bsr 8) band 16#ff,
	[Octet1, Octet2 | decode_base64(Rest)];
decode1_base64([Sextet1, Sextet2, Sextet3, Sextet4 | Rest]) ->
	Bits4x6 =
	    (d(Sextet1) bsl 18) bor
	    (d(Sextet2) bsl 12) bor
	    (d(Sextet3) bsl 6) bor
	    d(Sextet4),
	Octet1 = Bits4x6 bsr 16,
	Octet2 = (Bits4x6 bsr 8) band 16#ff,
	Octet3 = Bits4x6 band 16#ff,
	[Octet1, Octet2, Octet3 | decode_base64(Rest)];
decode1_base64(_CatchAll) ->
	"".

d(X) when X >= $A, X =<$Z -> X-65;
d(X) when X >= $a, X =<$z -> X-71;
d(X) when X >= $0, X =<$9 -> X+4;
d($+)                     -> 62;
d($/)                     -> 63;
d(_)                      -> 63.


encode_base64([]) ->
	[];
encode_base64([A]) ->
	[e(A bsr 2), e((A band 3) bsl 4), $=, $=];
encode_base64([A,B]) ->
	[e(A bsr 2), e(((A band 3) bsl 4) bor (B bsr 4)),
	    e((B band 15) bsl 2), $=];
encode_base64([A,B,C|Ls]) ->
	encode_base64_do(A,B,C, Ls).
encode_base64_do(A, B, C, Rest) ->
	BB = (A bsl 16) bor (B bsl 8) bor C,
	[e(BB bsr 18), e((BB bsr 12) band 63), 
     e((BB bsr 6) band 63), e(BB band 63)|encode_base64(Rest)].

e(X) when X >= 0, X < 26 -> X+65;
e(X) when X>25, X<52     -> X+71;
e(X) when X>51, X<62     -> X-4;
e(62)                    -> $+;
e(63)                    -> $/;
e(X)                     -> exit({bad_encode_base64_token, X}).

% --------------------------------------------------------------------
% Deprecated.
% --------------------------------------------------------------------

%% @spec(Attr_Name, XML_Element) -> New_XML_Element
%%     Attr_Name = string()
%%     XML_Element = xmlelement()
%%     New_XML_Element = xmlelement()
%% @deprecated Please use {@link exmpp_xml:remove_tag_attr/2}.
%% @doc Remove an attribute and return the new element.
remove_attr(Attr_Name, XML_Element) ->
	exmpp_xml:remove_tag_attr(Attr_Name, XML_Element).

% --------------------------------------------------------------------
% Documentation / type definitions.
% --------------------------------------------------------------------

%% @type jid() = {jid, User, Server, Resource, LUser, LServer, LResource}
%%     User = string()
%%     Server = string()
%%     Resource = string()
%%     LUser = string()
%%     LServer = string()
%%     LResource = string().
%% Record representing an XMPP/Jabber ID.

%% @type iq() = {iq, Id, Type, XML_NS, Lang, Sub_Element}
%%     Id = string()
%%     Type = set | get | reply
%%     XML_NS = string()
%%     Lang = string()
%%     Sub_Element = exmpp_xml:xmlelement().
%% Record reprensenting and XMPP/Jabber <iq/> stanza.
