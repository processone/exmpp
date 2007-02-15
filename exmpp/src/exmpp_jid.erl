% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides functions to handle JID.

-module(exmpp_jid).
-vsn('$Revision$').

-include("exmpp.hrl").

% Conversion.
-export([
  make_jid/3,
  make_bare_jid/2,
  jid_to_bare_jid/1,
  bare_jid_to_jid/2
]).

% Parsing.
-export([
  string_to_jid/1,
  string_to_bare_jid/1
]).

% Serialization.
-export([
  jid_to_string/1,
  bare_jid_to_string/1
]).

% --------------------------------------------------------------------
% JID creation & conversion.
% --------------------------------------------------------------------

make_bare_jid(User, Server) ->
    case exmpp_stringprep:nodeprep(User) of
        error ->
            {error, bad_user};
        LUser ->
            case exmpp_stringprep:nameprep(Server) of
                error ->
                    {error, bad_server};
                LServer ->
                    #jid{
                      user = User,
                      server = Server,
                      resource = "",
                      luser = LUser,
                      lserver = LServer,
                      lresource = ""
                    }
            end
    end.

make_jid(User, Server, Resource) ->
    case make_bare_jid(User, Server) of
        {error, Reason} ->
            {error, Reason};
        Jid ->
            bare_jid_to_jid(Jid, Resource)
    end.

jid_to_bare_jid(Jid) ->
    Jid#jid{
      resource = "",
      lresource = ""
    }.

bare_jid_to_jid(Jid, Resource) ->
    case exmpp_stringprep:resourceprep(Resource) of
        error ->
            {error, bad_resource};
        LResource ->
            Jid#jid{
              resource = Resource,
              lresource = LResource
            }
    end.

% --------------------------------------------------------------------
% JID parsing.
% --------------------------------------------------------------------

string_to_jid(String) ->
    string_to_jid1(String, "").

string_to_bare_jid(String) ->
    case string_to_jid(String) of
        {error, Reason} ->
            {error, Reason};
        Jid ->
            jid_to_bare_jid(Jid)
    end.

% Parse user/node.
string_to_jid1([$@ | _Jid_S], "") ->
    {error, node_or_user_expected};
string_to_jid1([$@ | Jid_S], User) ->
    string_to_jid2(Jid_S, lists:reverse(User), "");
string_to_jid1([$/ | _Jid_S], "") ->
    {error, server_expected};
string_to_jid1([$/ | Jid_S], User) ->
    string_to_jid3(Jid_S, "", lists:reverse(User), "");
string_to_jid1([C  | Jid_S], User) ->
    string_to_jid1(Jid_S, [C | User]);
string_to_jid1([], User) ->
    make_jid("", lists:reverse(User), "").

% Parser server.
string_to_jid2([$@ | _Jid_S], _User, _Server) ->
    {error, at_character_not_allowed};
string_to_jid2([$/ | _Jid_S], _User, "") ->
    {error, server_expected};
string_to_jid2([$/ | Jid_S], User, Server) ->
    string_to_jid3(Jid_S, User, lists:reverse(Server), "");
string_to_jid2([C  | Jid_S], User, Server) ->
    string_to_jid2(Jid_S, User, [C | Server]);
string_to_jid2([], User, Server) ->
    make_jid(User, lists:reverse(Server), "").

% Parse resource.
string_to_jid3([C  | Jid_S], User, Server, Res) ->
    string_to_jid3(Jid_S, User, Server, [C | Res]);
string_to_jid3([], User, Server, Res) ->
    make_jid(User, Server, lists:reverse(Res)).

% --------------------------------------------------------------------
% JID serialization.
% --------------------------------------------------------------------

jid_to_string(#jid{user = User, server = Server, resource = Res}) ->
    jid_to_string(User, Server, Res).

jid_to_string(User, Server, Res) ->
    S1 = bare_jid_to_string(User, Server),
    case Res of
        "" -> S1;
        _  -> S1 ++ "/" ++ Res
    end.

bare_jid_to_string(#jid{user = User, server = Server}) ->
    bare_jid_to_string(User, Server).

bare_jid_to_string(User, Server) ->
    S1 = case User of
        "" -> "";
        _  -> User ++ "@"
    end,
    S1 ++ Server.
