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

make_bare_jid(undefined, Server) ->
    case exmpp_stringprep:nameprep(Server) of
        error ->
            {error, bad_server};
        LServer ->
            #jid{
              user = undefined,
              server = Server,
              resource = undefined,
              luser = undefined,
              lserver = LServer,
              lresource = undefined
            }
    end;
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
                      resource = undefined,
                      luser = LUser,
                      lserver = LServer,
                      lresource = undefined
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
      resource = undefined,
      lresource = undefined
    }.

bare_jid_to_jid(Jid, undefined) ->
    Jid;
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
    parse_jid(full, String, "").

string_to_bare_jid(String) ->
    parse_jid(bare, String, "").

parse_jid(_Type, [$@ | _Rest], "") ->
    % Invalid JID of the form "@Domain".
    {error, unexpected_node_separator};
parse_jid(Type, [$@ | Rest], Node) ->
    % JID of the form "Node@Domain".
    parse_jid(Type, Rest, lists:reverse(Node), "");
parse_jid(_Type, [$/ | _Rest], "") ->
    % Invalid JID of the form "/Resource".
    {error, unexpected_resource_separator};
parse_jid(full, [$/], _Domain) ->
    % Invalid JID of the form "Domain/".
    {error, unexpected_end_of_string};
parse_jid(bare, [$/ | _Rest], Domain) ->
    % Valid JID of the form "Domain/Resource" (resource is dropped).
    make_bare_jid(undefined, lists:reverse(Domain));
parse_jid(full, [$/ | Rest], Domain) ->
    % Valid JID of the form "Domain/Resource".
    make_jid(undefined, lists:reverse(Domain), Rest);
parse_jid(Type, [C | Rest], Node_Or_Domain) ->
    % JID of the form "Node@Domain" or "Node@Domain/Resource".
    parse_jid(Type, Rest, [C | Node_Or_Domain]);
parse_jid(_Type, [], "") ->
    % Invalid JID of the form "".
    {error, unexpected_end_of_string};
parse_jid(bare, [], Domain) ->
    % Valid JID of the form "Domain".
    make_bare_jid(undefined, lists:reverse(Domain));
parse_jid(full, [], Domain) ->
    % Valid JID of the form "Domain".
    make_jid(undefined, lists:reverse(Domain), undefined).

parse_jid(_Type, [$@ | _Rest], _Node, _Domain) ->
    % Invalid JID of the form "Node@Domain@Domain".
    {error, unexpected_node_separator};
parse_jid(_Type, [$/ | _Rest], _Node, "") ->
    % Invalid JID of the form "Node@/Resource".
    {error, unexpected_resource_separator};
parse_jid(full, [$/], _Node, _Domain) ->
    % Invalid JID of the form "Node@Domain/".
    {error, unexpected_end_of_string};
parse_jid(bare, [$/ | _Rest], Node, Domain) ->
    % Valid JID of the form "Node@Domain/Resource" (resource is dropped).
    make_bare_jid(Node, lists:reverse(Domain));
parse_jid(full, [$/ | Rest], Node, Domain) ->
    % Valid JID of the form "Node@Domain/Resource".
    make_jid(Node, lists:reverse(Domain), Rest);
parse_jid(Type, [C | Rest], Node, Domain) ->
    % JID of the form "Node@Domain" or "Node@Domain/Resource".
    parse_jid(Type, Rest, Node, [C | Domain]);
parse_jid(_Type, [], _Node, "") ->
    % Invalid JID of the form "Node@".
    {error, unexpected_end_of_string};
parse_jid(bare, [], Node, Domain) ->
    % Valid JID of the form "Node@Domain".
    make_bare_jid(Node, lists:reverse(Domain));
parse_jid(full, [], Node, Domain) ->
    % Valid JID of the form "Node@Domain".
    make_jid(Node, lists:reverse(Domain), undefined).

% --------------------------------------------------------------------
% JID serialization.
% --------------------------------------------------------------------

jid_to_string(#jid{user = User, server = Server, resource = Res}) ->
    jid_to_string(User, Server, Res).

jid_to_string(User, Server, Res) ->
    S1 = bare_jid_to_string(User, Server),
    case Res of
        undefined -> S1;
        _         -> S1 ++ "/" ++ Res
    end.

bare_jid_to_string(#jid{user = User, server = Server}) ->
    bare_jid_to_string(User, Server).

bare_jid_to_string(User, Server) ->
    S1 = case User of
        undefined -> "";
        _         -> User ++ "@"
    end,
    S1 ++ Server.
