% $Id$

%% @author Jean-Sebastien Pedron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides functions which
%% doesn't deserve a dedicated module.

-module(exmpp_utils).
-vsn('$Revision$').

% Base64 helpers.
-export([
  encode_base64/1,
  decode_base64/1
]).

% List helpers.
-export([
  keystore/4
]).

% Binary and string helpers.
-export([
  strip/1,
  strip/2
]).

% Utils.
-export([
  random_id/0,
  random_id/1
]).

% --------------------------------------------------------------------
% Base64 helpers.
% --------------------------------------------------------------------

%% @spec (Data) -> Base64
%%     Data = string()
%%     Base64 = string()
%% @doc Encode `Data' in Base64.

-ifdef(ENABLE_HTTP_BASE_64).
% Starting with inets 5.0, http_base_64 doesn't exist anymore.
encode_base64(Data) ->
    case catch http_base_64:encode(Data) of
        {'EXIT', _} -> base64:encode_to_string(Data);
        Base64      -> Base64
    end.
-else.
encode_base64(Data) ->
    base64:encode_to_string(Data).
-endif.

%% @spec (Base64) -> Data
%%     Base64 = string()
%%     Data = string()
%% @doc Decode `Base64' to get `Data' back.

-ifdef(ENABLE_HTTP_BASE_64).
% Starting with inets 5.0, http_base_64 doesn't exist anymore.
decode_base64(Base64) ->
    case catch http_base_64:decode(Base64) of
        {'EXIT', _} -> base64:decode_to_string(Base64);
        Data        -> Data
    end.
-else.
decode_base64(Base64) ->
    base64:decode_to_string(Base64).
-endif.

% --------------------------------------------------------------------
% List helpers.
% --------------------------------------------------------------------

%% @spec (Key, N, Tuples_List, New_Tuple) -> New_Tuples_List
%%     Key = term()
%%     N = integer()
%%     Tuples_List = [Tuple]
%%     New_Tuple = Tuple
%%     New_Tuples_List = Tuple
%%     Tuple = tuple()
%% @doc Returns a copy of `Tuple_List' where the first occurrence of a
%% tuple T whose `N'th element compares equal to `Key' is replaced with
%% `New_Tuple', if there is such a tuple T.
%%
%% This feature appeared in Erlang R12B.
%%
%% @see lists:keystore/4.

keystore(Key, N, List, New) when is_integer(N), N > 0, is_tuple(New) ->
    keystore2(Key, N, List, New).

keystore2(Key, N, [Tuple | Rest], New) when element(N, Tuple) == Key ->
    [New | Rest];
keystore2(Key, N, [Tuple | Rest], New) ->
    [Tuple | keystore2(Key, N, Rest, New)];
keystore2(_Key, _N, [], New) ->
    [New].

% --------------------------------------------------------------------
% Binary and string helpers.
% --------------------------------------------------------------------

%% @spec strip(Stream) -> Stripped
%%     Stream = binary() | string()
%%     Stripped = binary() | string()
%% @doc Strip leading and trailing blanks.
%%
%% @see strip/3.

strip(Stream) ->
    strip(Stream, both).

%% @spec strip(Stream, Direction) -> Stripped
%%     Stream = binary() | string()
%%     Direction = left | right | both
%%     Stripped = binary() | string()
%% @doc Strip leading and/or trailing blanks, depending on the `Direction'.
%%
%% Blanks characters are `\s', `\t', `\n' and `\r'.
%%
%% The binary version was made by Christopher Faulet in his
%% <a href="http://www.capflam.org/?p=9">stream module</a>.
%%
%% @see strip/3.

strip(Stream, left) ->
    strip_left(Stream);
strip(Stream, right) ->
    strip_right(Stream);
strip(Stream, both) ->
    strip_right(strip_left(Stream)).

strip_left(<<C:8, Rest/binary>>) when C == $\s; C == $\t; C == $\n; C == $\r ->
    strip_left(Rest);
strip_left([C | Rest]) when C == $\s; C == $\t; C == $\n; C == $\r ->
    strip_left(Rest);
strip_left(Stripped) ->
    Stripped.

strip_right(<<C:8, Rest/binary>>) when C == $\s; C == $\t; C == $\n; C == $\r ->
    case strip_right(Rest) of
        <<>> -> <<>>;
        T    -> <<C:8, T/binary>>
    end;
strip_right(<<C:8, Rest/binary>>) ->
    T = strip_right(Rest),
    <<C:8, T/binary>>;
strip_right(<<>>) ->
    <<>>;
strip_right([C | Rest]) when C == $\s; C == $\t; C == $\n; C == $\r ->
    case strip_right(Rest) of
        [] -> [];
        T  -> [C | T]
    end;
strip_right([C | Rest]) ->
    [C | strip_right(Rest)];
strip_right([]) ->
    [].

% --------------------------------------------------------------------
% Utils.
% --------------------------------------------------------------------

%% @spec () -> ID
%%     ID = string()
%% @doc Generate a random ID.
%%
%% Use the `exmpp' prefix.
%%
%% @see random_id/1.

random_id() ->
    random_id("exmpp").

%% @spec (Prefix) -> ID
%%     Prefix = string()
%%     ID = string()
%% @doc Generate a random stanza ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.
%%
%% The ID is not guaranted to be unique.

random_id(undefined) ->
    integer_to_list(random:uniform(65536 * 65536));
random_id("") ->
    random_id(undefined);
random_id(Prefix) when is_atom(Prefix) ->
    random_id(atom_to_list(Prefix));
random_id(Prefix) when is_list(Prefix) ->
    Prefix ++ "-" ++ random_id(undefined).
