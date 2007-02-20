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

-define(NODE_MAX_LENGTH,     1023).
-define(DOMAIN_MAX_LENGTH,   1023).
-define(RESOURCE_MAX_LENGTH, 1023).
-define(BARE_JID_MAX_LENGTH, ?NODE_MAX_LENGTH + 1 + ?DOMAIN_MAX_LENGTH).
-define(JID_MAX_LENGTH,      ?BARE_JID_MAX_LENGTH + 1 + ?RESOURCE_MAX_LENGTH).

% --------------------------------------------------------------------
% JID creation & conversion.
% --------------------------------------------------------------------

make_bare_jid(_Node, Domain)
  when length(Domain) > ?DOMAIN_MAX_LENGTH ->
    {error, domain_too_long};
make_bare_jid(undefined, Domain) ->
    case exmpp_stringprep:nameprep(Domain) of
        error ->
            {error, bad_domain};
        LDomain ->
            #jid{
              user = undefined,
              server = Domain,
              resource = undefined,
              luser = undefined,
              lserver = LDomain,
              lresource = undefined
            }
    end;
make_bare_jid(Node, _Domain)
  when length(Node) > ?NODE_MAX_LENGTH ->
    {error, node_too_long};
make_bare_jid(Node, Domain) ->
    case exmpp_stringprep:nodeprep(Node) of
        error ->
            {error, bad_node};
        LNode ->
            case exmpp_stringprep:nameprep(Domain) of
                error ->
                    {error, bad_domain};
                LDomain ->
                    #jid{
                      user = Node,
                      server = Domain,
                      resource = undefined,
                      luser = LNode,
                      lserver = LDomain,
                      lresource = undefined
                    }
            end
    end.

make_jid(Node, Domain, undefined) ->
    make_bare_jid(Node, Domain);
make_jid(_Node, _Domain, Resource)
  when length(Resource) > ?RESOURCE_MAX_LENGTH ->
    {error, resource_too_long};
make_jid(Node, Domain, Resource) ->
    case make_bare_jid(Node, Domain) of
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
bare_jid_to_jid(_Jid, Resource)
  when length(Resource) > ?RESOURCE_MAX_LENGTH ->
    {error, resource_too_long};
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

string_to_jid(String)
  when length(String) > ?JID_MAX_LENGTH ->
    {error, jid_too_long};
string_to_jid(String) ->
    parse_jid(full, String, "").

string_to_bare_jid(String)
  when length(String) > ?BARE_JID_MAX_LENGTH ->
    {error, jid_too_long};
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
parse_jid(bare, [$/ | _Resource], Domain) ->
    % Valid JID of the form "Domain/Resource" (resource is dropped).
    make_bare_jid(undefined, lists:reverse(Domain));
parse_jid(full, [$/ | Resource], Domain) ->
    % Valid JID of the form "Domain/Resource".
    make_jid(undefined, lists:reverse(Domain), Resource);
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
parse_jid(full, [$/ | Resource], Node, Domain) ->
    % Valid JID of the form "Node@Domain/Resource".
    make_jid(Node, lists:reverse(Domain), Resource);
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

jid_to_string(#jid{user = Node, server = Domain, resource = Resource}) ->
    jid_to_string(Node, Domain, Resource).

jid_to_string(Node, Domain, Resource) ->
    S1 = bare_jid_to_string(Node, Domain),
    case Resource of
        undefined -> S1;
        _         -> S1 ++ "/" ++ Resource
    end.

bare_jid_to_string(#jid{user = Node, server = Domain}) ->
    bare_jid_to_string(Node, Domain).

bare_jid_to_string(Node, Domain) ->
    S1 = case Node of
        undefined -> "";
        _         -> Node ++ "@"
    end,
    S1 ++ Domain.
