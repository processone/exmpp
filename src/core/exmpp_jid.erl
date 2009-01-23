% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides functions to handle
%% JID.

-module(exmpp_jid).
-vsn('$Revision$').

-include("exmpp.hrl").
-include("internal/exmpp_xmpp.hrl").

-export([binary_split/2]).

% Conversion.
-export([
  make_jid/0,
  make_jid/1,
  make_jid/2,
  make_jid/3,
  jid_to_bare_jid/1,
  bare_jid_to_jid/2
]).

% Parsing.
-export([
  parse_jid/1
]).

% Serialization.
-export([
  jid_to_list/1,
  jid_to_list/2,
  jid_to_list/3,
  prepd_jid_to_list/1,
  bare_jid_to_list/1,
  bare_jid_to_list/2,
  prepd_bare_jid_to_list/1,
  jid_to_binary/1,
  jid_to_binary/2,
  jid_to_binary/3,
  prepd_jid_to_binary/1,
  bare_jid_to_binary/1,
  bare_jid_to_binary/2,
  prepd_bare_jid_to_binary/1
]).

% Comparison.
-export([
  compare_jids/2,
  compare_bare_jids/2,
  compare_domains/2
]).

% Checks.
-export([
  is_jid/1
]).

% List accessors.
-export([
  node_as_list/1,
  lnode_as_list/1,
  domain_as_list/1,
  ldomain_as_list/1,
  resource_as_list/1,
  lresource_as_list/1
]).

% Raw binary() accessors.
-export([
  node/1,
  lnode/1,
  domain/1,
  ldomain/1,
  resource/1,
  lresource/1
]).

-define(NODE_MAX_LENGTH,     1023).
-define(DOMAIN_MAX_LENGTH,   1023).
-define(RESOURCE_MAX_LENGTH, 1023).
-define(BARE_JID_MAX_LENGTH, ?NODE_MAX_LENGTH + 1 + ?DOMAIN_MAX_LENGTH).
-define(JID_MAX_LENGTH,      ?BARE_JID_MAX_LENGTH + 1 + ?RESOURCE_MAX_LENGTH).

% --------------------------------------------------------------------
% JID creation & conversion.
% --------------------------------------------------------------------

%% @spec () -> Jid
%%     Jid = jid()
%% @doc Create a blank JID.

make_jid() ->
    #jid{}.

%% @spec (Domain) -> Bare_Jid
%%     Domain = binary() | string()
%%     Bare_Jid = jid()
%% @throws {jid, make, too_long, {domain, Domain}} |
%%         {jid, make, invalid,  {domain, Domain}}
%% @doc Create a bare JID.

make_jid(Domain) ->
    make_jid(undefined, Domain).

%% @spec (Node, Domain) -> Bare_Jid
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     Bare_Jid = jid()
%% @throws {jid, make, too_long, {domain, Domain}} |
%%         {jid, make, invalid,  {domain, Domain}} |
%%         {jid, make, too_long, {node, Node}} |
%%         {jid, make, invalid,  {node, Node}}
%% @doc Create a bare JID.

make_jid(_Node, Domain)
  when is_list(Domain), length(Domain) > ?DOMAIN_MAX_LENGTH ->
    throw({jid, make, too_long, {domain, Domain}});
make_jid(_Node, Domain)
  when is_binary(Domain), size(Domain) > ?DOMAIN_MAX_LENGTH ->
    throw({jid, make, too_long, {domain, Domain}});
make_jid("", Domain) ->
    % This clause is here because ejabberd uses empty string.
    make_jid(undefined, Domain);
make_jid(<<>>, Domain) ->
    % This clause is here because ejabberd uses empty string.
    make_jid(undefined, Domain);
make_jid(undefined, Domain) ->
    try
        LDomain = exmpp_stringprep:nameprep(Domain),
        #jid{
          orig_jid = to_binary(Domain),
          lnode = undefined,
          ldomain = to_binary(LDomain),
          lresource = undefined
        }
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
        throw:{stringprep, nameprep, invalid_string, _} ->
            throw({jid, make, invalid, {domain, Domain}})
    end;
make_jid(Node, _Domain)
  when is_list(Node), length(Node) > ?NODE_MAX_LENGTH ->
    throw({jid, make, too_long, {node, Node}});
make_jid(Node, _Domain)
  when is_binary(Node), size(Node) > ?NODE_MAX_LENGTH ->
    throw({jid, make, too_long, {node, Node}});
make_jid(Node, Domain) ->
    try
        LNode = exmpp_stringprep:nodeprep(Node),
        LDomain = exmpp_stringprep:nameprep(Domain),
        #jid{
          orig_jid =
            <<(to_binary(Node))/binary, $@, (to_binary(Domain))/binary >>,
          lnode = to_binary(LNode),
          ldomain = to_binary(LDomain),
          lresource = undefined
        }
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
        throw:{stringprep, nodeprep, invalid_string, _} ->
            throw({jid, make, invalid, {node, Node}});
        throw:{stringprep, nameprep, invalid_string, _} ->
            throw({jid, make, invalid, {domain, Domain}})
    end.

%% @spec (Node, Domain, Resource) -> Jid
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     Resource = binary() | string() | random | undefined
%%     Jid = jid()
%% @doc Create a full JID.

make_jid(Node, Domain, undefined) ->
    make_jid(Node, Domain);
make_jid(Node, Domain, "") ->
    % This clause is here because ejabberd uses empty string.
    make_jid(Node, Domain);
make_jid(Node, Domain, <<>>) ->
    % This clause is here because ejabberd uses empty string.
    make_jid(Node, Domain);
make_jid(Node, Domain, Resource) ->
    Jid = make_jid(Node, Domain),
    try
        bare_jid_to_jid(Jid, Resource)
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
        throw:{jid, convert, Reason, Infos} ->
            throw({jid, make, Reason, Infos})
    end.

%% @spec (Orig, Node, Domain, Resource) -> Jid
%%     Orig = binary()
%%     Node = binary() | string() | undefined
%%     Resource = binary() | string() | undefined
%%     Jid = jid()
%% @doc Create a full JID.
%%
%% The first parameter is the original JID `Node@Domain/Resource' (eg.
%% as extracted from a stanza recipient).
%%
%% We reuse this value here. The intention is to save some memory, see
%% comments on `include/internal/exmpp_xmpp.hrl'

make_jid(Orig, Node, Domain, Resource) ->
    try
        LNode = case Node of
            undefined -> undefined;
            _         -> to_binary(exmpp_stringprep:nodeprep(Node))
        end,
        LDomain = to_binary(exmpp_stringprep:nameprep(Domain)),
        LResource = case Resource of
            undefined -> undefined;
            _         -> to_binary(exmpp_stringprep:resourceprep(Resource))
        end,
        #jid{
          orig_jid = Orig,
          lnode = LNode,
          ldomain = LDomain,
          lresource = LResource
        }
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
        throw:{stringprep, nodeprep, invalid_string, _} ->
            throw({jid, make, invalid, {node, Node}});
        throw:{stringprep, nameprep, invalid_string, _} ->
            throw({jid, make, invalid, {domain, Domain}});
        throw:{stringprep, resourceprep, invalid_string, _} ->
            throw({jid, make, invalid, {resource, Resource}})
    end.

%% @spec (Jid) -> Bare_Jid
%%     Jid = jid()
%%     Bare_Jid = jid()
%% @doc Convert a full JID to its bare version.

jid_to_bare_jid(#jid{orig_jid = Orig_Jid} = Jid) ->
    New_Orig_Jid = case binary_split(Orig_Jid, $/) of
        [Bare_Jid, _] -> Bare_Jid;
        [Bare_Jid]    -> Bare_Jid
    end,
    Jid#jid{
      orig_jid = New_Orig_Jid,
      lresource = undefined
    }.

%% @spec (Bare_Jid, Resource) -> Jid
%%     Bare_Jid = jid()
%%     Resource = binary() | string() | random
%%     Jid = jid()
%% @throws {jid, convert, too_long, {resource, Resource}} |
%%         {jid, convert, invalid,  {resource, Resource}}
%% @doc Convert a bare JID to its full version.

bare_jid_to_jid(Jid, undefined) ->
    Jid;
bare_jid_to_jid(Jid, "") ->
    % This clause is here because ejabberd uses empty string.
    Jid;
bare_jid_to_jid(Jid, <<>>) ->
    % This clause is here because ejabberd uses empty string.
    Jid;
bare_jid_to_jid(Jid, random) ->
    Resource = generate_resource(),
    bare_jid_to_jid(Jid, Resource);
bare_jid_to_jid(_Jid, Resource)
  when is_list(Resource), length(Resource) > ?RESOURCE_MAX_LENGTH ->
    throw({jid, convert, too_long, {resource, Resource}});
bare_jid_to_jid(_Jid, Resource)
  when is_binary(Resource), size(Resource) > ?RESOURCE_MAX_LENGTH ->
    throw({jid, convert, too_long, {resource, Resource}});
bare_jid_to_jid(#jid{orig_jid = Orig_Jid} = Jid, Resource) ->
    try
        LResource = exmpp_stringprep:resourceprep(Resource),
        Resource_B = to_binary(Resource),
        New_Orig_Jid = case binary_split(Orig_Jid, $/) of
            [Bare_Jid, _] -> <<Bare_Jid/binary, $/, Resource_B/binary>>;
            [Bare_Jid]    -> <<Bare_Jid/binary, $/, Resource_B/binary>>
        end,
        Jid#jid{
          orig_jid = New_Orig_Jid,
          lresource = to_binary(LResource)
        }
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
        throw:{stringprep, resourceprep, invalid_string, _} ->
            throw({jid, convert, invalid, {resource, Resource}})
    end.

% --------------------------------------------------------------------
% JID parsing.
% --------------------------------------------------------------------

%% @spec (String) -> Jid
%%     String = binary() | string()
%%     Jid = jid()
%% @throws {jid, parse, Reason, {jid, String}}
%% @doc Parse a string and create a full JID.

parse_jid(String) when is_binary(String) ->
    case parse_binary(String, String, <<>>) of
        {error, Reason} ->
            throw({jid, parse, Reason, {jid, String}});
        Jid ->
            Jid
    end;
parse_jid(String) when is_list(String) ->
    B = list_to_binary(String),
    case parse_binary(B, B, <<>>) of
        {error, Reason} ->
            throw({jid, parse, Reason, {jid, String}});
        Jid ->
            Jid
    end.

parse_binary(_Original, String, _) when size(String) > ?JID_MAX_LENGTH ->
    % Invalid JID: too long.
    {error, too_long};
parse_binary(_Original, <<$@, _Rest/binary>>, <<>>) ->
    % Invalid JID of the form "@Domain".
    {error, unexpected_node_separator};
parse_binary(Original, <<$@, Rest/binary>>, Node) ->
    % JID of the form "Node@Domain".
    parse_binary(Original, Rest, Node, <<>>);
parse_binary(_Original, <<$/, _Rest/binary>>, <<>>) ->
    % Invalid JID of the form "/Resource".
    {error, unexpected_resource_separator};
parse_binary(_Original, <<$/>>, _Domain) ->
    % Invalid JID of the form "Domain/".
    {error, unexpected_end_of_string};
parse_binary(Original, <<$/ , Resource/binary>>, Domain) ->
    % Valid JID of the form "Domain/Resource".
    make_jid(Original, undefined, Domain, Resource);
parse_binary(Original, <<C, Rest/binary>>, Node_Or_Domain) ->
    % JID of the form "Node@Domain" or "Node@Domain/Resource".
    parse_binary(Original, Rest, <<Node_Or_Domain/binary, C>>);
parse_binary(_Original, <<>>, <<>>) ->
    % Invalid JID of the form "".
    {error, unexpected_end_of_string};
parse_binary(Original, <<>>, Domain) ->
    % Valid JID of the form "Domain".
    make_jid(Original, undefined, Domain, undefined).
parse_binary(_Original, <<$@,  _Rest/binary>>, _Node, _Domain) ->
    % Invalid JID of the form "Node@Domain@Domain".
    {error, unexpected_node_separator};
parse_binary(_Original, <<$/, _Rest/binary>>, _Node, <<>>) ->
    % Invalid JID of the form "Node@/Resource".
    {error, unexpected_resource_separator};
parse_binary(_Original, <<$/>>, _Node, _Domain) ->
    % Invalid JID of the form "Node@Domain/".
    {error, unexpected_end_of_string};
parse_binary(Original, <<$/, Resource/binary>>, Node, Domain) ->
    % Valid JID of the form "Node@Domain/Resource".
    make_jid(Original, Node, Domain, Resource);
parse_binary(Original, <<C, Rest/binary>>, Node, Domain) ->
    % JID of the form "Node@Domain" or "Node@Domain/Resource".
    parse_binary(Original, Rest, Node, <<Domain/binary, C>>);
parse_binary(_Original, <<>>, _Node, <<>>) ->
    % Invalid JID of the form "Node@".
    {error, unexpected_end_of_string};
parse_binary(Original, <<>>, Node, Domain) ->
    % Valid JID of the form "Node@Domain".
    make_jid(Original, Node, Domain, undefined).

% --------------------------------------------------------------------
% JID serialization.
% --------------------------------------------------------------------

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = string()
%% @doc Stringify a full JID.

jid_to_list(#jid{} = JID) ->
    binary_to_list(jid_to_binary(JID)).

%% @spec (Node, Domain) -> String
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     String = string()
%% @doc Stringify a bare JID.

jid_to_list(Node, Domain) ->
    bare_jid_to_list(Node, Domain).

%% @spec (Node, Domain, Resource) -> String
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     Resource = binary() | string() | undefined
%%     String = string()
%% @doc Stringify a full JID.

jid_to_list(Node, Domain, Resource) ->
    binary_to_list(jid_to_binary(Node, Domain, Resource)).

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = string()
%% @doc Stringify a full JID with STRINGPREP profiles applied.

prepd_jid_to_list(
  #jid{lnode = Node, ldomain = Domain, lresource = Resource}) ->
    jid_to_list(Node, Domain, Resource).

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = string()
%% @doc Stringify a bare JID.

bare_jid_to_list(#jid{} = JID) ->
    binary_to_list(bare_jid_to_binary(JID)).

%% @spec (Node, Domain) -> String
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     String = string()
%% @doc Stringify a full JID.

bare_jid_to_list(Node, Domain) ->
    binary_to_list(bare_jid_to_binary(Node, Domain)).

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = string()
%% @doc Stringify a bare JID with STRINGPREP profiles applied.

prepd_bare_jid_to_list(
  #jid{lnode = Node, ldomain = Domain}) ->
    bare_jid_to_list(Node, Domain).

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = binary()
%% @doc Stringify a full JID.

jid_to_binary(#jid{orig_jid = Orig_Jid}) ->
    Orig_Jid.

%% @spec (Node, Domain) -> String
%%     Node = bianry() | string() | undefined
%%     Domain = binary() | string()
%%     String = binary()
%% @doc Stringify a bare JID.

jid_to_binary(Node, Domain) ->
    bare_jid_to_binary(Node, Domain).

%% @spec (Node, Domain, Resource) -> String
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     Resource = binary() | string() | undefined
%%     String = binary()
%% @doc Stringify a full JID.

jid_to_binary(Node, Domain, Resource) when is_list(Resource) ->
    jid_to_binary(Node, Domain, as_binary_or_undefined(Resource));

jid_to_binary(Node, Domain, Resource)
  when Resource == undefined orelse is_binary(Resource) ->
    S1 = bare_jid_to_binary(Node, Domain),
    if
        Resource == <<>> orelse Resource == undefined ->
            S1;
        true ->
            <<S1/binary, "/", Resource/binary>>
    end.

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = binary()
%% @doc Stringify a full JID with STRINGPREP profiles applied.

prepd_jid_to_binary(
  #jid{lnode = Node, ldomain = Domain, lresource = Resource}) ->
    jid_to_binary(Node, Domain, Resource).

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = binary()
%% @doc Stringify a bare JID.

bare_jid_to_binary(#jid{orig_jid = Orig_Jid, lresource = LResource} = Jid) ->
    case LResource of
        undefined -> Orig_Jid;
        _         -> bare_jid_to_binary(exmpp_jid:node(Jid), domain(Jid))
    end.

%% @spec (Node, Domain) -> String
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     String = binary()
%% @doc Stringify a full JID.

bare_jid_to_binary(Node, Domain) when is_list(Node) ->
    bare_jid_to_binary(as_binary_or_undefined(Node), Domain);
bare_jid_to_binary(Node, Domain) when is_list(Domain) ->
    bare_jid_to_binary(Node, as_binary_or_undefined(Domain));

bare_jid_to_binary(Node, Domain)
  when (Node == undefined orelse is_binary(Node)) andalso
  (Domain == undefined orelse is_binary(Domain)) ->
    if
        Node == <<>> orelse Node == undefined ->
            Domain;
        true ->
            <<Node/binary, "@", Domain/binary>>
    end.

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = binary()
%% @doc Stringify a bare JID with STRINGPREP profiles applied.

prepd_bare_jid_to_binary(#jid{lnode = Node, ldomain = Domain}) ->
    bare_jid_to_binary(Node, Domain).

% --------------------------------------------------------------------
% JID comparison.
% --------------------------------------------------------------------

%% @spec (Jid1, Jid2) -> bool()
%%     Jid1 = jid()
%%     Jid2 = jid()
%% @doc Compare full JIDs.

compare_jids(
  #jid{lnode = LNode, ldomain = LDomain, lresource = LResource},
  #jid{lnode = LNode, ldomain = LDomain, lresource = LResource}) ->
    true;
compare_jids(_Jid1, _Jid2) ->
    false.

%% @spec (Jid1, Jid2) -> bool()
%%     Jid1 = jid()
%%     Jid2 = jid()
%% @doc Compare bare JIDs.

compare_bare_jids(
  #jid{lnode = LNode, ldomain = LDomain},
  #jid{lnode = LNode, ldomain = LDomain}) ->
    true;
compare_bare_jids(_Jid1, _Jid2) ->
    false.

%% @spec (Jid1, Jid2) -> bool()
%%     Jid1 = jid()
%%     Jid2 = jid()
%% @doc Compare JID's domain.

compare_domains(
  #jid{ldomain = LDomain},
  #jid{ldomain = LDomain}) ->
    true;
compare_domains(_Jid1, _Jid2) ->
    false.

% --------------------------------------------------------------------
% JID checks.
% --------------------------------------------------------------------

%% @spec (Jid) -> bool()
%%     Jid = jid()
%% @doc Tell if the argument is a JID.
%%
%% You should probably use the `IS_JID(Jid)' guard expression.

is_jid(Jid) when ?IS_JID(Jid) ->
    true;
is_jid(_) ->
    false.

% --------------------------------------------------------------------
% JID members accessors.
% --------------------------------------------------------------------

%% @spec (Jid) -> Node | undefined
%%     Jid = jid()
%%     Node = binary()
%% @doc Return the node part of a JID.

node(#jid{orig_jid = Orig_Jid}) ->
    case binary_split(Orig_Jid, $@) of
        [Node, _] -> Node;
        _         -> undefined
    end.

%% @spec (Jid) -> Node | undefined
%%     Jid = jid()
%%     Node = binary()
%% @doc Return the node part of a JID with NODEPREP profile applied.

lnode(#jid{lnode = N}) -> N.

%% @spec (Jid) -> Domain
%%     Jid = jid()
%%     Domain = binary()
%% @doc Return the domain part of a JID.

domain(#jid{orig_jid = Orig_Jid}) -> 
    Domain_And_Resource = case binary_split(Orig_Jid, $@) of
        [_, Domain1] -> Domain1;
        _            -> Orig_Jid
    end,
    case binary_split(Domain_And_Resource, $/) of
        [Domain2, _] -> Domain2;
        _            -> Domain_And_Resource
    end.

%% @spec (Jid) -> Domain
%%     Jid = jid()
%%     Domain = binary()
%% @doc Return the domain part of a JID with NAMEPREP profile applied.

ldomain(#jid{ldomain = D}) -> D.

%% @spec (Jid) -> Resource | undefined
%%     Jid = jid()
%%     Resource = binary()
%% @doc Return the resource part of a JID.

resource(#jid{orig_jid = Orig_Jid}) ->
    case binary_split(Orig_Jid, $/) of
        [_, Resource] -> Resource;
        _             -> undefined
    end.

%% @spec (Jid) -> Resource
%%     Jid = jid()
%%     Resource = binary()
%% @doc Return the resource part of a JID with RESOURCEPREP profile applied.

lresource(#jid{lresource = R}) -> R.

%% @spec (Jid) -> Node | undefined
%%     Jid = jid()
%%     Node = string()
%% @doc Return the node part of a JID as a list.

node_as_list(Jid) -> as_list_or_undefined(exmpp_jid:node(Jid)).

%% @spec (Jid) -> Node | undefined
%%     Jid = jid()
%%     Node = string()
%% @doc Return the node part of a JID as a list with NODEPREP profile
%% applied.

lnode_as_list(Jid) -> as_list_or_undefined(lnode(Jid)).

%% @spec (Jid) -> Domain
%%     Jid = jid()
%%     Domain = string()
%% @doc Return the domain part of a JID as a list.

domain_as_list(Jid) -> as_list_or_undefined(domain(Jid)).

%% @spec (Jid) -> Domain
%%     Jid = jid()
%%     Domain = string()
%% @doc Return the domain part of a JID as a list with NAMEPREP profile
%% applied.

ldomain_as_list(Jid) -> as_list_or_undefined(ldomain(Jid)).

%% @spec (Jid) -> Resource | undefined
%%     Jid = jid()
%%     Resource = string()
%% @doc Return the resource part of a JID as a list.

resource_as_list(Jid) -> as_list_or_undefined(resource(Jid)).

%% @spec (Jid) -> Resource | undefined
%%     Jid = jid()
%%     Resource = string()
%% @doc Return the domain part of a JID as a list with RESOURCEPREP
%% profile applied.

lresource_as_list(Jid) -> as_list_or_undefined(lresource(Jid)).

as_list_or_undefined(undefined) ->
    undefined;
as_list_or_undefined(V) when is_binary(V) ->
    binary_to_list(V).

as_binary_or_undefined(undefined) ->
    undefined;
as_binary_or_undefined(V) when is_list(V) ->
    list_to_binary(V).

% --------------------------------------------------------------------
% Helper functions
% --------------------------------------------------------------------

% We do not use random generator to avoid having to decide when and 
% how to seed the Erlang random number generator.
generate_resource() ->
    {A, B, C} = erlang:now(),
    lists:flatten(["exmpp#",
      integer_to_list(A),
      integer_to_list(B),
      integer_to_list(C)]
    ).

% If both lists are equal, don't waste memory creating two separate
% binary copies.
to_binary(A) when is_list(A) ->
    list_to_binary(A);
to_binary(B) when is_binary(B) ->
    B.

binary_split(B, C) -> binary_split(B, C, <<>>, []).

binary_split(<<C, Rest/binary>>, C, Acc, Tokens) ->
    binary_split(Rest, C, <<>>, [Acc | Tokens]);
binary_split(<<C1, Rest/binary>>, C, Acc, Tokens) ->
    binary_split(Rest, C, <<Acc/binary, C1>>, Tokens);
binary_split(<<>>, _C, Acc, Tokens) ->
    lists:reverse([Acc | Tokens]).

% --------------------------------------------------------------------
% Documentation / type definitions.
% --------------------------------------------------------------------

%% @type jid() = {jid, Orig_Jid, Prepd_Node, Prepd_Domain, Prepd_Resource}
%%     Orig_Jid = binary() | undefined
%%     Prepd_Node = binary() | undefined
%%     Prepd_Domain = binary() | undefined
%%     Prepd_Resource = binary() | undefined.
%% Represents a JID.
%%
%% <strong>`jid()' is an internal type and the structure documented
%% herein may be changed without notice</strong>. Please use only the
%% accessors exported by this module to get each component of a JID.
%%
%% `Prepd_Node' is set to the value of `Node' passed through the
%% NODEPREP stringprep profile.
%%
%% `Prepd_Domain' is set to the value of `Domain' passed through the
%% NAMEPREP stringprep profile.
%%
%% `Prepd_Resource' is set to the value of `Resource' passed through the
%% RESOURCEPREP stringprep profile.
