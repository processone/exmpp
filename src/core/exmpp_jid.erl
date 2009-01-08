% $Id$

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides functions to handle
%% JID.

-module(exmpp_jid).
-vsn('$Revision$').

-include("exmpp.hrl").
-include("exmpp_jid.hrl").

% Conversion.
-export([
  make_jid/0,
  make_jid/1,
  make_jid/2,
  make_jid/3,
  make_bare_jid/1,
  make_bare_jid/2,
  jid_to_bare_jid/1,
  bare_jid_to_jid/2
]).

% Parsing.
-export([
  list_to_jid/1,
  list_to_bare_jid/1,
  binary_to_jid/1,
  binary_to_bare_jid/1
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

% List Accessors.
-export([
    node_as_list/1,
    lnode_as_list/1,
    domain_as_list/1,
    ldomain_as_list/1,
    resource_as_list/1,
    lresource_as_list/1]).

% raw binary() accessors.
-export([
    node/1,
    lnode/1,
    domain/1,
    ldomain/1,
    resource/1,
    lresource/1]).


% Checks.
-export([
  is_jid/1
]).

-define(NODE_MAX_LENGTH,     1023).
-define(DOMAIN_MAX_LENGTH,   1023).
-define(RESOURCE_MAX_LENGTH, 1023).
-define(BARE_JID_MAX_LENGTH, ?NODE_MAX_LENGTH + 1 + ?DOMAIN_MAX_LENGTH).
-define(JID_MAX_LENGTH,      ?BARE_JID_MAX_LENGTH + 1 + ?RESOURCE_MAX_LENGTH).

% --------------------------------------------------------------------
% JID creation & conversion.
% --------------------------------------------------------------------

%% @spec (Domain) -> Bare_Jid
%%     Domain = string()
%%     Bare_Jid = jid()
%% @throws {jid, make, domain_too_long, {Node, Domain, undefined}} |
%%         {jid, make, invalid_domain,  {Node, Domain, undefined}}
%% @doc Create a bare JID.

make_bare_jid(Domain) ->
    make_bare_jid(undefined, Domain).

%% @spec (Node, Domain) -> Bare_Jid
%%     Node = string()
%%     Domain = string()
%%     Bare_Jid = jid()
%% @throws {jid, make, domain_too_long, {Node, Domain, undefined}} |
%%         {jid, make, invalid_domain,  {Node, Domain, undefined}} |
%%         {jid, make, node_too_long,   {Node, Domain, undefined}} |
%%         {jid, make, invalid_node,    {Node, Domain, undefined}}
%% @doc Create a bare JID.

make_bare_jid(Node, Domain)
  when length(Domain) > ?DOMAIN_MAX_LENGTH ->
    throw({jid, make, domain_too_long, {Node, Domain, undefined}});
make_bare_jid("", Domain) ->
    % This clause is here because ejabberd uses empty string.
    make_bare_jid(undefined, Domain);
make_bare_jid(<<>>, Domain) ->
    % This clause is here because ejabberd uses empty string.
    make_bare_jid(undefined, Domain);
make_bare_jid(undefined, Domain) ->
    try
        LDomain = exmpp_stringprep:nameprep(Domain),
        #jid{
          node = undefined,
          domain = to_binary(Domain),
          resource = undefined,
          lnode = undefined,
          ldomain = to_binary(LDomain),
          lresource = undefined
        }
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
        throw:{stringprep, nameprep, invalid_string, _} ->
            throw({jid, make, invalid_domain, {undefined, Domain, undefined}})
    end;
make_bare_jid(Node, Domain)
  when is_list(Node), length(Node) > ?NODE_MAX_LENGTH ->
    throw({jid, make, node_too_long, {Node, Domain, undefined}});
make_bare_jid(Node, Domain)
  when is_binary(Node), size(Node) > ?NODE_MAX_LENGTH ->
    throw({jid, make, node_too_long, {Node, Domain, undefined}});
make_bare_jid(Node, Domain) ->
    try
        LNode = exmpp_stringprep:nodeprep(Node),
        LDomain = exmpp_stringprep:nameprep(Domain),

        #jid{
          node = to_binary(Node),
          domain = to_binary(Domain),
          resource = undefined,
          lnode = to_binary(LNode),
          ldomain = to_binary(LDomain),
          lresource = undefined
        }
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
        throw:{stringprep, nodeprep, invalid_string, _} ->
            throw({jid, make, invalid_node, {Node, Domain, undefined}});
        throw:{stringprep, nameprep, invalid_string, _} ->
            throw({jid, make, invalid_domain, {Node, Domain, undefined}})
    end.

%% @spec () -> Jid
%%   Jid = jid()
make_jid() ->
    #jid{}.

%% @spec (Domain) -> Jid
%%     Domain = string()
%%     Jid = jid()
%% @doc Create a bare JID.

make_jid(Domain) ->
    make_bare_jid(Domain).

%% @spec (Node, Domain) -> Jid
%%     Node = string() | undefined
%%     Domain = string()
%%     Jid = jid()
%% @doc Create a bare JID.

make_jid(Node, Domain) ->
    make_bare_jid(Node, Domain).

%% @spec (Node, Domain, Resource) -> Jid
%%     Node = string() | undefined
%%     Domain = string()
%%     Resource = string() | random | undefined
%%     Jid = jid()
%% @doc Create a full JID.

make_jid(Node, Domain, undefined) ->
    make_bare_jid(Node, Domain);
make_jid(Node, Domain, "") ->
    % This clause is here because ejabberd uses empty string.
    make_bare_jid(Node, Domain);
make_jid(Node, Domain, <<>>) ->
    % This clause is here because ejabberd uses empty string.
    make_bare_jid(Node, Domain);
make_jid(Node, Domain, random) ->
    Resource = generate_resource(),
    make_jid(Node, Domain, Resource);
make_jid(Node, Domain, Resource) ->
    Jid = make_bare_jid(Node, Domain),
    try
        bare_jid_to_jid(Jid, Resource)
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
        throw:{jid, convert, Reason, Infos} ->
            throw({jid, make, Reason, Infos})
    end.

%% @spec (Jid) -> Bare_Jid
%%     Jid = jid()
%%     Bare_Jid = jid()
%% @doc Convert a full JID to its bare version.

jid_to_bare_jid(Jid) ->
    Jid#jid{
      resource = undefined,
      lresource = undefined
    }.

%% @spec (Bare_Jid, Resource) -> Jid
%%     Bare_Jid = jid()
%%     Resource = string()
%%     Jid = jid()
%% @throws {jid, convert, resource_too_long, {Node, Domain, Resource}} |
%%         {jid, convert, invalid_resource,  {Node, Domain, Resource}}
%% @doc Convert a bare JID to its full version.

bare_jid_to_jid(Jid, undefined) ->
    Jid;
bare_jid_to_jid(Jid, "") ->
    % This clause is here because ejabberd uses empty string.
    Jid;
bare_jid_to_jid(Jid, <<>>) ->
    % This clause is here because ejabberd uses empty string.
    Jid;
bare_jid_to_jid(Jid, Resource)
  when is_list(Resource), length(Resource) > ?RESOURCE_MAX_LENGTH ->
    throw({jid, convert, resource_too_long,
        {Jid#jid.node, Jid#jid.domain, Resource}});
bare_jid_to_jid(Jid, Resource)
  when is_binary(Resource), size(Resource) > ?RESOURCE_MAX_LENGTH ->
    throw({jid, convert, resource_too_long,
        {Jid#jid.node, Jid#jid.domain, Resource}});
bare_jid_to_jid(Jid, Resource) ->
    try
        LResource = exmpp_stringprep:resourceprep(Resource),
        Jid#jid{
          resource = to_binary(Resource),
          lresource = to_binary(LResource)
        }
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
        throw:{stringprep, resourceprep, invalid_string, _} ->
            throw({jid, convert, invalid_resource,
                {Jid#jid.node, Jid#jid.domain, Resource}})
    end.

% --------------------------------------------------------------------
% JID parsing.
% --------------------------------------------------------------------

%% @spec (String) -> Jid
%%     String = string()
%%     Jid = jid()
%% @throws {jid, parse, jid_too_long, {String, undefined, undefined}} |
%%         {jid, parse, Reason,       {String, undefined, undefined}}
%% @doc Parse a string and create a full JID.

list_to_jid(String)
  when length(String) > ?JID_MAX_LENGTH ->
    throw({jid, parse, jid_too_long, {String, undefined, undefined}});
list_to_jid(String) ->
    case parse_jid(full, String, "") of
        {error, Reason} ->
            throw({jid, parse, Reason, {String, undefined, undefined}});
        Jid ->
            Jid
    end.

%% @spec (String) -> Bare_Jid
%%     String = string()
%%     Bare_Jid = jid()
%% @throws {jid, parse, jid_too_long, {String, undefined, undefined}} |
%%         {jid, parse, Reason,       {String, undefined, undefined}}
%% @doc Parse a string and create a bare JID.

list_to_bare_jid(String)
  when length(String) > ?BARE_JID_MAX_LENGTH ->
    throw({jid, parse, jid_too_long, {String, undefined, undefined}});
list_to_bare_jid(String) ->
    case parse_jid(bare, String, "") of
        {error, Reason} ->
            throw({jid, parse, Reason, {String, undefined, undefined}});
        Bare_Jid ->
            Bare_Jid
    end.

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
% JID parsing (Binary).
% --------------------------------------------------------------------

%% @spec (Binary) -> Jid
%%     Binary = binary()
%%     Jid = jid()
%% @throws {jid, parse, jid_too_long, {Binary, undefined, undefined}} |
%%         {jid, parse, Reason,       {Binary, undefined, undefined}}
%% @doc Parse a string and create a full JID.

binary_to_jid(String)
  when size(String) > ?JID_MAX_LENGTH ->
    throw({jid, parse, jid_too_long, {String, undefined, undefined}});
binary_to_jid(String) ->
    case b_parse_jid(String, <<>>) of
        {error, Reason} ->
            throw({jid, parse, Reason, {String, undefined, undefined}});
        Jid ->
            Jid
    end.

%% @spec (String) -> Bare_Jid
%%     String = string()
%%     Bare_Jid = jid()
%% @throws {jid, parse, jid_too_long, {String, undefined, undefined}} |
%%         {jid, parse, Reason,       {String, undefined, undefined}}
%% @doc Parse a string and create a bare JID.

binary_to_bare_jid(String) ->
  jid_to_bare_jid(binary_to_jid(String)).
  %% we discard the resource, but still need to parse it to check that
  %% the jid is valid. So it's basically the same than for binary_to_jid/1.


    % Invalid JID of the form "Node@Domain/".

b_parse_jid(<<$@, _Rest/binary>>, <<>>) ->
    % Invalid JID of the form "@Domain".
    {error, unexpected_node_separator};
b_parse_jid(<<$@, Rest/binary>>, Node) ->
    % JID of the form "Node@Domain".
    b_parse_jid(Rest, Node, <<>>);
b_parse_jid(<<$/, _Rest/binary>>, <<>>) ->
    % Invalid JID of the form "/Resource".
    {error, unexpected_resource_separator};
b_parse_jid(<<$/>>, _Domain) ->
    % Invalid JID of the form "Domain/".
    {error, unexpected_end_of_string};
b_parse_jid(<<$/ , Resource/binary>>, Domain) ->
    % Valid JID of the form "Domain/Resource".
    make_jid(undefined, Domain, Resource);
b_parse_jid(<<C, Rest/binary>>, Node_Or_Domain) ->
    % JID of the form "Node@Domain" or "Node@Domain/Resource".
    b_parse_jid(Rest, <<Node_Or_Domain/binary, C>>);
b_parse_jid(<<>>, <<>>) ->
    % Invalid JID of the form "".
    {error, unexpected_end_of_string};
b_parse_jid(<<>>, Domain) ->
    % Valid JID of the form "Domain".
    make_jid(undefined, Domain, undefined).

b_parse_jid(<<$@,  _Rest/binary>>, _Node, _Domain) ->
    % Invalid JID of the form "Node@Domain@Domain".
    {error, unexpected_node_separator};
b_parse_jid(<<$/, _Rest/binary>>, _Node, <<>>) ->
    % Invalid JID of the form "Node@/Resource".
    {error, unexpected_resource_separator};
b_parse_jid(<<$/>>, _Node, _Domain) ->
    % Invalid JID of the form "Node@Domain/".
    {error, unexpected_end_of_string};
b_parse_jid(<<$/, Resource/binary>>, Node, Domain) ->
    % Valid JID of the form "Node@Domain/Resource".
    make_jid(Node, Domain, Resource);
b_parse_jid(<<C, Rest/binary>>, Node, Domain) ->
    % JID of the form "Node@Domain" or "Node@Domain/Resource".
    b_parse_jid(Rest, Node, <<Domain/binary, C>>);
b_parse_jid(<<>>, _Node, <<>>) ->
    % Invalid JID of the form "Node@".
    {error, unexpected_end_of_string};
b_parse_jid(<<>>, Node, Domain) ->
    % Valid JID of the form "Node@Domain".
    make_jid(Node, Domain, undefined).



% --------------------------------------------------------------------
% JID serialization.
% --------------------------------------------------------------------

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = string()
%% @doc Stringify a full JID.

jid_to_list(#jid{node = Node, domain = Domain, resource = Resource}) ->
    jid_to_list(Node, Domain, Resource).

%% @spec (Node, Domain) -> String
%%     Node = string() | undefined
%%     Domain = string()
%%     String = string()
%% @doc Stringify a bare JID.

jid_to_list(Node, Domain) ->
    bare_jid_to_list(Node, Domain).

%% @spec (Node, Domain, Resource) -> String
%%     Node = string() | undefined
%%     Domain = string()
%%     Resource = string() | undefined
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

bare_jid_to_list(#jid{node = Node, domain = Domain}) ->
    bare_jid_to_list(Node, Domain).

%% @spec (Node, Domain) -> String
%%     Node = string() | undefined
%%     Domain = string()
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

jid_to_binary(#jid{node = Node, domain = Domain, resource = Resource}) ->
    jid_to_binary(Node, Domain, Resource).

%% @spec (Node, Domain) -> String
%%     Node = string() | undefined
%%     Domain = string()
%%     String = binary()
%% @doc Stringify a bare JID.

jid_to_binary(Node, Domain) ->
    bare_jid_to_binary(Node, Domain).

%% @spec (Node, Domain, Resource) -> String
%%     Node = string() | undefined
%%     Domain = string()
%%     Resource = string() | undefined
%%     String = binary()
%% @doc Stringify a full JID.
jid_to_binary(N, D, R) when is_list(N) ; is_list(D) ; is_list(R) ->
    jid_to_binary(as_binary_or_undefined(N),
                  as_binary_or_undefined(D),
                  as_binary_or_undefined(R));
jid_to_binary(Node, Domain, Resource) ->
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

bare_jid_to_binary(#jid{node = Node, domain = Domain}) ->
    bare_jid_to_binary(Node, Domain).

%% @spec (Node, Domain) -> String
%%     Node = string() | undefined
%%     Domain = string()
%%     String = binary()
%% @doc Stringify a full JID.

bare_jid_to_binary(Node, Domain) when is_list(Node); is_list(Domain) ->
    bare_jid_to_binary(as_binary_or_undefined(Node),
                       as_binary_or_undefined(Domain));
    
bare_jid_to_binary(Node, Domain) ->
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

prepd_bare_jid_to_binary(
  #jid{lnode = Node, ldomain = Domain}) ->
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

is_jid(JID) when ?IS_JID(JID) ->
    true;
is_jid(_) ->
    false.


% --------------------------------------------------------------------
% JID members getters.
% --------------------------------------------------------------------
%%
%% @spec (Jid) -> Node
%%  Jid = jid()
%%  Node = binary()
node(#jid{node = N}) -> N.

%% @spec (Jid) -> Node
%%  Jid = jid()
%%  Node = binary()
lnode(#jid{lnode = N}) -> N.

%% @spec (Jid) -> Domain
%%  Jid = jid()
%%  Domain = binary()
domain(#jid{domain = D}) -> D.

%% @spec (Jid) -> Domain
%%  Jid = jid()
%%  Domain = binary()
ldomain(#jid{ldomain = D}) -> D.

%% @spec (Jid) -> Resource
%%  Jid = jid()
%%  Resource = binary()
resource(#jid{resource = R}) -> R.

%% @spec (Jid) -> Resource
%%  Jid = jid()
%%  Resource = binary()
lresource(#jid{lresource = R}) -> R.

%%
%% @spec (Jid) -> Node
%%  Jid = jid()
%%  Node = list()
node_as_list(JID) -> as_list_or_undefined(exmpp_jid:node(JID)).

%% @spec (Jid) -> Node
%%  Jid = jid()
%%  Node = list()
lnode_as_list(JID) -> as_list_or_undefined(lnode(JID)).

%% @spec (Jid) -> Domain
%%  Jid = jid()
%%  Domain = list()
domain_as_list(JID) -> as_list_or_undefined(domain(JID)).

%% @spec (Jid) -> Domain
%%  Jid = jid()
%%  Domain = list()
ldomain_as_list(JID) -> as_list_or_undefined(ldomain(JID)).

%% @spec (Jid) -> Resource
%%  Jid = jid()
%%  Resource = list()
resource_as_list(JID) -> as_list_or_undefined(resource(JID)).

%% @spec (Jid) -> Resource
%%  Jid = jid()
%%  Resource = list()
lresource_as_list(JID) -> as_list_or_undefined(lresource(JID)).


as_list_or_undefined(V) when is_binary(V) ->
    binary_to_list(V);
as_list_or_undefined(V) ->
    V.

as_binary_or_undefined(V) when is_list(V) ->
    list_to_binary(V);
as_binary_or_undefined(V) ->
    V.
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

%% If both lists are equal, don't waste memory creating two separate
%% binary copies.
to_binary(A) when is_list(A) ->
    list_to_binary(A);
to_binary(B) when is_binary(B) ->
    B.

% --------------------------------------------------------------------
% Documentation / type definitions.
% --------------------------------------------------------------------

%% @type jid() = {jid, Node, Domain, Resource, Prepd_Node, Prepd_Domain, Prepd_Resource}
%%     Node = string() | undefined
%%     Domain = string() | undefined
%%     Resource = string() | undefined
%%     Prepd_Node = string() | undefined
%%     Prepd_Domain = string() | undefined
%%     Prepd_Resource = string() | undefined.
%% Represents JID.
%%
%% `Prepd_Node' is set to the value of `Node' passed through the
%% NODEPREP stringprep profile.
%%
%% `Prepd_Domain' is set to the value of `Domain' passed through the
%% NAMEPREP stringprep profile.
%%
%% `Prepd_Resource' is set to the value of `Resource' passed through the
%% RESOURCEPREP stringprep profile.
