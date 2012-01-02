%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Mickael Remond <mickael.remond@process-one.net>
%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides helpers to manipulate
%% standard stanza.

-module(exmpp_stanza).

-include("exmpp.hrl").

%% avoid name clash with local error/2 function
-compile({no_auto_import,[error/2]}).

%% Stanza common components.
-export([
	 get_error/1
	]).

%% Stanza standard attributes.
-export([
	 get_sender/1,
	 set_sender/2,
	 remove_sender/1,
	 get_recipient/1,
	 set_recipient/2,
	 remove_recipient/1,
	 set_jids/3,
	 get_id/1,
	 set_id/2,
	 get_type/1,
	 set_type/2,
	 get_lang/1,
	 set_lang/2
	]).

%% Common operations.
-export([
	 reply/1,
	 reply_without_content/1,
	 reply_with_error/2
	]).

%% Stanza-level errors.
-export([
	 error/1,
	 error/2,
	 stanza_error/2,
	 stanza_error_without_original/2,
	 is_stanza_error/1,
	 get_error_type/1,
	 set_error_type/2,
	 set_error_type_from_condition/2,
	 get_condition/1,
	 get_text/1
	]).

%% Serialization wrappers.
-export([
	 to_iolist/1
	]).

%% --------------------------------------------------------------------
%% Documentation / type definition.
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Stanza common components.
%% --------------------------------------------------------------------

%% @spec (Stanza) -> Error | undefined
%%     Stanza = exxml:xmlel() | iq()
%%     Error = exxml:xmlel()
%% @doc Return the error element from `Stanza'.
%%
%% The error element is supposed to have the name `error' and the same
%% namespace as the stanza.

-spec(get_error/1 :: (exxml:xmlel()) -> exxml:xmlel() | undefined).

get_error({xmlel, _, _, _} = Stanza) ->
    exxml:get_element(Stanza, <<"error">>);
get_error(#iq{type = <<"error">>, error = Error}) ->
    Error;
get_error(#iq{}) ->
    undefined.

%% --------------------------------------------------------------------
%% Stanza standard attributes.
%% --------------------------------------------------------------------

%% @spec (Stanza) -> Sender | undefined
%%     Stanza = exxml:xmlel()
%%     Sender = binary()
%% @doc Return the sender.
%%
%% The return value should be a JID and may be parsed with
%% {@link exmpp_jid:parse/1}.

-spec(get_sender/1 :: (exxml:xmlel()) -> binary() | undefined).

get_sender(Stanza) ->
    exxml:get_attribute(Stanza, <<"from">>, undefined).


%% @spec (Stanza, Sender) -> New_Stanza
%%     Stanza = exxml:xmlel()
%%     Sender =  binary() 
%%     New_Stanza = exxml:xmlel()
%% @doc Set the sender.
%%
-spec(set_sender/2 ::
	(exxml:xmlel(), binary() ) -> exxml:xmlel()).

set_sender(Stanza, Sender) ->
    exxml:set_attribute(Stanza, <<"from">>, Sender).


%% @spec (Stanza) -> New_Stanza
%%     Stanza = exxml:xmlel()
%%     New_Stanza = exxml:xmlel()
%% @doc Remove the sender.

-spec(remove_sender/1 :: (exxml:xmlel()) -> exxml:xmlel()).

remove_sender(Stanza) ->
    exxml:remove_attribute(Stanza, <<"from">>).


%% @spec (Stanza) -> Recipient | undefined
%%     Stanza = exxml:xmlel()
%%     Recipient = binary()
%% @doc Return the recipient.
%%
%% The return value should be a JID and may be parsed with
%% {@link exmpp_jid:parse/1}.

-spec(get_recipient/1 :: (exxml:xmlel()) -> binary() | undefined).

get_recipient(Stanza) ->
    exxml:get_attribute(Stanza, <<"to">>, undefined).


%% @spec (Stanza, Recipient) -> New_Stanza
%%     Stanza = exxml:xmlel()
%%     Recipient =  binary() 
%%     New_Stanza = exxml:xmlel()
%% @doc Set the recipient.
%%

-spec(set_recipient/2 ::
	(exxml:xmlel(), binary() ) -> exxml:xmlel()).

set_recipient(Stanza, Recipient) ->
    exxml:set_attribute(Stanza, <<"to">>, Recipient).


%% @spec (Stanza) -> New_Stanza
%%     Stanza = exxml:xmlel()
%%     New_Stanza = exxml:xmlel()
%% @doc Remove the recipient.

-spec(remove_recipient/1 :: (exxml:xmlel()) -> exxml:xmlel()).

remove_recipient(Stanza) -> 
	exxml:remove_attribute(Stanza, <<"to">>).


%% @spec (Stanza, Sender, Recipient) -> New_Stanza
%%     Stanza = exxml:xmlel()
%%     Sender =  binary() 
%%     Recipient = binary() 
%%     New_Stanza = exxml:xmlel()
%% @doc Set the sender and the recipient at the same time.
%%

-spec(set_jids/3 ::
	(exxml:xmlel(), binary(), binary()) -> exxml:xmlel()).

set_jids(Stanza, From, To) ->
    set_recipient(set_sender(Stanza, From), To).



%% @spec (Stanza) -> ID | undefined
%%     Stanza = exxml:xmlel() | exmpp_iq:iq()
%%     ID = binary()
%% @doc Return the stanza ID.

-spec(get_id/1 :: (exxml:xmlel() | #iq{}) -> binary() | undefined).

get_id(#iq{id = ID}) ->
    ID;
get_id(Stanza) ->
    exxml:get_attribute(Stanza, <<"id">>).


%% @spec (Stanza, ID) -> New_Stanza
%%     Stanza = exxml:xmlel() | exmpp_iq:iq()
%%     ID = binary() | random 
%%     New_Stanza = exxml:xmlel() | exmpp_iq:iq()
%% @doc Set the ID.
%%

-spec(set_id/2 :: (exxml:xmlel() | #iq{}, binary() | random) -> exxml:xmlel() | #iq{}).

set_id({xmlel, Name, _, _} = Stanza, random) ->
    set_id(Stanza, exmpp_utils:random_id(Name));
set_id({xmlel, _, _, _} = Stanza, ID) ->
	exxml:set_attribute(Stanza, <<"id">>, ID);
set_id(#iq{} = Stanza, random) ->
    ID = exmpp_utils:random_id("iq"),
    set_id(Stanza, ID);
set_id(#iq{} = Stanza, ID) ->
    Stanza#iq{id = ID}.

%% @spec (Stanza) -> Type | undefined
%%     Stanza = exxml:xmlel() | exmpp_iq:iq()
%%     Type = binary()
%% @doc Return the type of the stanza.

-spec(get_type/1 :: (exxml:xmlel() | #iq{}) -> binary() | undefined).

get_type({xmlel, _N, _Attr, _Child}=El) ->
    exxml:get_attribute(El, <<"type">>, undefined);

get_type(#iq{type = Type}) ->
    Type.



%% @spec (Stanza, Type) -> New_Stanza
%%     Stanza = exxml:xmlel() | exmpp_iq:iq()
%%     Type = binary() 
%%     New_Stanza = exxml:xmlel() | exmpp_iq:iq()
%% @doc Set the type of the stanza.

-spec(set_type/2 :: (exxml:xmlel() | #iq{}, binary()) -> exxml:xmlel() | #iq{}).

set_type({xmlel, _, _, _} = Stanza, Type) ->
    exxml:set_attribute(Stanza, <<"type">>, Type);
set_type(#iq{} = Stanza, <<"get">>) ->
    Stanza#iq{type = <<"get">>, kind = request};
set_type(#iq{} = Stanza, <<"set">>) ->
    Stanza#iq{type = <<"set">>, kind = request};
set_type(#iq{} = Stanza, <<"result">>) ->
    Stanza#iq{type = <<"result">>, kind = response};
set_type(#iq{} = Stanza, <<"error">>) ->
    Stanza#iq{type = <<"error">>, kind = response}.


%% @spec (Stanza) -> Lang | undefined
%%     Stanza = exxml:xmlel() | exmpp_iq:iq()
%%     Lang = binary()
%% @doc Return the language of the stanza.

-spec(get_lang/1 :: (exxml:xmlel() | #iq{}) -> binary() | undefined).

get_lang({xmlel, _, _, _} = Stanza) ->
    case exxml:get_attribute(Stanza, <<"lang">>) of
	    undefined -> exxml:get_attribute(Stanza, <<"xml:lang">>);
	    Lang -> Lang
    end;
get_lang(#iq{lang = Lang}) ->
    Lang.


%% @spec (Stanza, Lang) -> New_Stanza
%%     Stanza = exxml:xmlel() | exmpp_iq:iq()
%%     Lang = binary() | undefined
%%     New_Stanza = exxml:xmlel() | exmpp_iq:iq()
%% @doc Set the lang.
%%
%% If `Lang' is `undefined' or empty, it's removed.

-spec(set_lang/2 :: (exxml:xmlel() | #iq{}, binary()) -> exxml:xmlel() | #iq{}).

set_lang({xmlel, _,_,_} = Stanza, undefined) ->
	exxml:remove_attribute(exxml:remove_attribute(Stanza, <<"lang">>), <<"xml:lang">>);
set_lang({xmlel, _,_,_} = Stanza, Lang) ->
    exxml:set_attribute(Stanza, <<"lang">>, Lang);
set_lang(#iq{} = Stanza, Lang) ->
    Stanza#iq{lang = Lang}.


%% --------------------------------------------------------------------
%% Common operations.
%% --------------------------------------------------------------------

%% @spec (Stanza) -> Stanza_Reply
%%     Stanza = exxml:xmlel()
%%     Stanza_Reply = exxml:xmlel()
%% @doc Prepare a reply to `Stanza'.
%%
%% @see reply_from_attrs/1.

-spec(reply/1 :: (exxml:xmlel()) -> exxml:xmlel()).
reply(Stanza) ->
    From = get_sender(Stanza),
    To = get_recipient(Stanza),
    S1 = case From of
	    undefined ->
		    exxml:remove_attribute(Stanza, <<"to">>);
	     _ ->
		     set_recipient(Stanza, From)
    end,
    case To of
	    undefined ->
		    exxml:remove_attribute(S1, <<"from">>);
	    _ ->
		    set_sender(S1, To)
    end.


%% @spec (Stanza) -> Stanza_Reply
%%     Stanza = exxml:xmlel()
%%     Stanza_Reply = exxml:xmlel()
%% @doc Prepare a reply to `Stanza' with children removed.
%%

-spec(reply_without_content/1 :: (exxml:xmlel()) -> exxml:xmlel()).

reply_without_content(Stanza) ->
    {xmlel, Name, Attrs, _} = reply(Stanza),
    {xmlel, Name, Attrs, []}.


%% @spec (Stanza, Error) -> Stanza_Reply
%%     Stanza = exxml:xmlel()
%%     Error = exxml:xmlel() | binary()
%%     Stanza_Reply = exxml:xmlel()
%% @doc Prepare an error reply to `Stanza'.
%%
%% If `Error' is an atom, it must be a standard condition defined by
%% XMPP Core.

-spec(reply_with_error/2 :: (exxml:xmlel(), exxml:xmlel() | binary()) -> exxml:xmlel()).

reply_with_error(Stanza, Condition) when is_binary(Condition) ->
    Error = exmpp_staza:error(Condition),
    reply_with_error(Stanza, Error);
reply_with_error(Stanza, Error) ->
    Reply = reply(Stanza),
    stanza_error(Reply, Error).

%% --------------------------------------------------------------------
%% Stanza-level errors.
%% --------------------------------------------------------------------

standard_conditions() ->
    [
     {<<"bad-request">>,             <<"modify">> },
     {<<"conflict">>,                <<"cancel">> },
     {<<"feature-not-implemented">>, <<"cancel">> },
     {<<"forbidden">>,               <<"auth">>   },
     {<<"gone">>,                    <<"modify">> },
     {<<"internal-server-error">>,   <<"wait">>   },
     {<<"item-not-found">>,          <<"cancel">> },
     {<<"jid-malformed">>,           <<"modify">> },
     {<<"not-acceptable">>,          <<"modify">> },
     {<<"not-allowed">>,             <<"cancel">> },
     {<<"not-authorized">>,          <<"auth">>   },
     {<<"payment-required">>,        <<"auth">>   },
     {<<"recipient-unavailable">>,   <<"wait">>   },
     {<<"redirect">>,                <<"modify">> },
     {<<"registration-required">>,   <<"auth">>   },
     {<<"remote-server-not-found">>, <<"cancel">> },
     {<<"remote-server-timeout">>,   <<"wait">>   },
     {<<"resource-constraint">>,     <<"wait">>   },
     {<<"service-unavailable">>,     <<"cancel">> },
     {<<"subscription-required">>,   <<"auth">>   },
     {<<"unexpected-request">>,      <<"wait">>   },
     {<<"undefined-condition">>,     undefined}
    ].

%% @spec (Condition) -> Stanza_Error
%%     Condition = binary()
%%     Stanza_Error = exxml:xmlel()
%% @doc Create an `<error/>' element based on the given `Condition'.
%%
-spec(error/1 :: (binary()) -> exxml:xmlel()).

error(Condition) ->
    error(Condition, {undefined, undefined}).

%% @spec (Condition, Text_Spec) -> Stanza_Error
%%     Condition = binary()
%%     Text_Spec = {Lang, Text} | Text | undefined
%%     Lang = binary() | | undefined
%%     Text = binary() | | undefined
%%     Stanza_Error = exxml:xmlel()
%% @doc Create an `<error/>' element based on the given `Condition'.
%%

-spec(error/2 ::
	(binary(), {binary() | undefined, binary() | undefined}) -> exxml:xmlel()).

error(Condition, {Lang, Text}) ->
    Condition_El = {xmlel, Condition, [{<<"xmlns">>, ?NS_STANZA_ERRORS}], []},
    Error_El0 = {xmlel, <<"error">>, [], [Condition_El]},
    Error_El = case Text of
		   undefined ->
		       Error_El0;
		   _ ->
		   	Text_El0 = {xmlel, <<"text">>, [{<<"xmlns">>, ?NS_STANZA_ERRORS}], [{cdata, Text}]},
            		Text_El = case Lang of
                	undefined ->
                    		Text_El0;
                	_ ->
                    		exxml:set_attribute(Text_El0, <<"xml:lang">>, Lang)
		       end,
            	       exxml:append_child(Error_El0, Text_El)
    end,
    set_error_type_from_condition_in_error(Error_El, Condition);
error(Condition, Text) ->
    error(Condition, {undefined, Text}).

%% @spec (Stanza, Error) -> Stanza_Error
%%     Stanza = exxml:xmlel()
%%     Error = exxml:xmlel()
%%     Stanza_Error = exxml:xmlel()
%% @doc Transform `Stanza' in a stanza error.
%%
%% The `type' attribute is set and an error condition is added. The
%% caller is still responsible to set or modify the `to' attribute
%% correctly.
%%
%% @see error/2.
%% @see error/3.

-spec(stanza_error/2 :: (exxml:xmlel(), exxml:xmlel()) -> exxml:xmlel()).

stanza_error(Stanza, Error) ->
    Stanza_Error = exxml:append_child(Stanza, Error),
    set_type(Stanza_Error, <<"error">>).

%% @spec (Stanza, Error) -> Stanza_Error
%%     Stanza = exxml:xmlel()
%%     Error = exxml:xmlel()
%%     Stanza_Error = exxml:xmlel()
%% @doc Transform `Stanza' in a stanza error.
%%
%% Previous child elements from `Stanza' are not kept.
%%
%% @see stanza_error/2.

-spec(stanza_error_without_original/2 :: (exxml:xmlel(), exxml:xmlel()) -> exxml:xmlel()).

stanza_error_without_original({xmlel, Name, Attrs, _}, Error) ->
    set_type({xmlel, Name, Attrs, [Error]}, <<"error">>).

%% @spec (Stanza) -> boolean()
%%     Stanza = exxml:xmlel()
%% @doc Tell if the stanza transports an error.

-spec(is_stanza_error/1 :: (exxml:xmlel()) -> boolean()).

is_stanza_error(Stanza) ->
    case get_type(Stanza) of
        <<"error">> -> true;
        _           -> false
    end.

%% @spec (Stanza) -> Type
%%     Stanza = exxml:xmlel()
%%     Type = binary()
%% @throws {stanza_error, error_type, no_error_element_found, Stanza}
%% @doc Return the type of the error element.

-spec(get_error_type/1 :: (exxml:xmlel()) -> binary()).

get_error_type(Stanza) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, error_type, no_error_element_found, Stanza});
        Error ->
            get_error_type_from_error(Error)
    end.

get_error_type_from_error(Error) ->
    exxml:get_attribute(Error, <<"type">>, <<>>).

%% @spec (Stanza, Type) -> New_Stanza
%%     Stanza = exxml:xmlel()
%%     Type = binary() 
%%     New_Stanza = exxml:xmlel()
%% @throws {stanza_error, error_type, no_error_element_found, Stanza}
%% @doc Set the type of the error element.

-spec(set_error_type/2 :: (exxml:xmlel(), binary()) -> exxml:xmlel()).

set_error_type(Stanza, Type) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, error_type, no_error_element_found, Stanza});
        Error ->
            New_Error = set_error_type_in_error(Error, Type),
            exxml:set_or_replace_child(Stanza, New_Error)
    end.

set_error_type_in_error(Error, Type) ->
    exxml:set_attribute(Error, <<"type">>, Type).

%% @spec (Stanza, Condition) -> New_Stanza
%%     Stanza = exxml:xmlel()
%%     Condition = binary()
%%     New_Stanza = exxml:xmlel()
%% @throws {stanza_error, error_type, no_error_element_found, Stanza} |
%%         {stanza_error, error_type, invalid_condition, {NS, Condition}}
%% @doc Set the type of the error element, based on the given condition.
%%

-spec(set_error_type_from_condition/2 :: (exxml:xmlel(), binary()) -> exxml:xmlel()).

set_error_type_from_condition(Stanza, Condition) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, error_type, no_error_element_found, Stanza});
        Error ->
            New_Error = set_error_type_from_condition_in_error(Error,
              Condition),
            exxml:set_or_replace_child(Stanza, New_Error)
    end.

set_error_type_from_condition_in_error(Error, Condition) ->
    case lists:keysearch(Condition, 1, standard_conditions()) of
        {value, {_, undefined}} ->
            Error;
        {value, {_, Type}} ->
            set_error_type_in_error(Error, Type);
        false ->
            throw({stanza_error, error_type, invalid_condition, Condition})
    end.

%% @spec (Stanza) -> Condition | undefined
%%     Stanza = exxml:xmlel()
%%     Condition = binary()
%% @throws {stanza_error, condition, no_error_element_found, Stanza} |
%%         {stanza_error, condition, no_condition_found, Error}
%% @doc Return the child element name corresponding to the stanza error
%% condition.
%%

-spec(get_condition/1 :: (exxml:xmlel()) -> binary()).

get_condition(Stanza) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, condition, no_error_element_found, Stanza});
        Error ->
            get_condition_in_error(Error)
    end.

get_condition_in_error({xmlel, _Name, _Attrs, _Children} = Error)  ->
	case exxml:get_elements(Error) of
		[{xmlel, Condition, _Attrs2, _C}|_] when Condition /= <<"text">> ->
			Condition;
		_ ->
            	% This <error/> element is invalid because the condition must be
	         % present (and first).
        	    throw({stanza_error, condition, no_condition_found, Error})
        end.

%% @spec (Stanza) -> Text | undefined
%%     Stanza = exxml:xmlel()
%%     Text = binary()
%% @throws {stanza_error, text, no_error_element_found, Stanza}
%% @doc Return the text that describes the error.
%%
%% If there is no `<text/>' element, an empty string is returned.

-spec(get_text/1 :: (exxml:xmlel()) -> binary()).

get_text(Stanza) ->
    case get_error(Stanza) of
        undefined ->
            throw({stanza_error, text, no_error_element_found, Stanza});
        Error ->
            get_text_in_error(Error)
    end.

get_text_in_error(Error) ->
	exxml:get_path(Error, [{element, <<"text">>}, cdata]).

%% --------------------------------------------------------------------
%% Serialization wrappers.
%% --------------------------------------------------------------------


%% @spec (El) -> XML_Text
%%     El = exxml:xmlel() | exmpp_iq:iq()  
%%     XML_Text = iolist()
%% @doc Serialize a stanza using common XMPP default namespaces.
%%

-spec(to_iolist/1 :: (exxml:xmlel() | #iq{}) -> iolist()).

to_iolist(#iq{} = IQ) ->
	to_iolist(exmpp_iq:iq_to_xmlel(IQ));
to_iolist({xmlel, _, _, _} =  El) ->
	exxml:document_to_iolist(El).

