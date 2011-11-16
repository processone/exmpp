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

%% @author Jean-Sebastien Pedron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides functions which
%% doesn't deserve a dedicated module.

-module(exmpp_utils).

%% Binary and string helpers.
-export([
	 strip/1,
	 strip/2
	]).

%% Utils.
-export([
	 random_id/0,
	 random_id/1
	]).

%% @spec strip(Stream) -> Stripped
%%     Stream = binary() 
%%     Stripped = binary() 
%% @doc Strip leading and trailing blanks.
%%
%% @see strip/3.

-spec(strip/1 ::
      (binary()) -> binary()).

strip(Stream) ->
    strip(Stream, both).

%% @spec strip(Stream, Direction) -> Stripped
%%     Stream = binary() 
%%     Direction = left | right | both
%%     Stripped = binary() 
%% @doc Strip leading and/or trailing blanks, depending on the `Direction'.
%%
%% Blanks characters are `\s', `\t', `\n' and `\r'.
%%
%% The binary version was made by Christopher Faulet in his
%% <a href="http://www.capflam.org/?p=9">stream module</a>.
%%
%% @see strip/3.

-spec(strip/2 ::
      (binary(), left | right | both) -> binary()).

strip(Stream, left) ->
    strip_left(Stream);
strip(Stream, right) ->
    strip_right(Stream);
strip(Stream, both) ->
    strip_right(strip_left(Stream)).

strip_left(<<C:8, Rest/binary>>) when C == $\s; C == $\t; C == $\n; C == $\r ->
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
    <<>>.

%% --------------------------------------------------------------------
%% Utils.
%% --------------------------------------------------------------------

%% @spec () -> ID
%%     ID = string()
%% @doc Generate a random ID.
%%
%% Use the `exmpp' prefix.
%%
%% @see random_id/1.

-spec(random_id/0 :: () -> binary()).

random_id() ->
    random_id(<<"exmpp">>).

%% @spec (Prefix) -> ID
%%     Prefix = binary()
%%     ID = binary()
%% @doc Generate a random stanza ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.
%%
%% The ID is not guaranted to be unique.

-spec(random_id/1 :: (binary() | undefined) -> binary()).

random_id(undefined) ->
	list_to_binary(integer_to_list(random:uniform(65536 * 65536)));
random_id(Prefix) ->
	<<Prefix/binary, (random_id(undefined))/binary>>.
