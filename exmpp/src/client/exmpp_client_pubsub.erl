% $Id: exmpp_factory.erl 210 2007-03-06 13:18:40Z cromain $

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

-module(exmpp_client_pubsub).
-vsn('$Revision: 210 $').

-include("exmpp.hrl").

-export([
	pubsub_create_node/2,
	pubsub_create_node/3,
	pubsub_delete_node/2,
	pubsub_delete_node/3,
	pubsub_subscribe/3,
	pubsub_subscribe/4,
	pubsub_publish/3,
	pubsub_publish/4
]).

% --------------------------------------------------------------------
% Publish/subscribe containers.
% --------------------------------------------------------------------

%% @spec (Service, Node) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for creating a node on a pubsub service.
%%
%% The stanza `id' is generated automatically.

pubsub_create_node(Service, Node) ->
	pubsub_create_node(pubsub_id(), Service, Node).

%% @spec (Id, Service, Node) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for creating a node on a pubsub service.

pubsub_create_node(Id, Service, Node) ->
	% Make the <create/> element.
	Create = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_PUBSUB, name = 'create', children = []}, [
	    {'node', Node}]),
	% Prepare the final <iq/>.
	Pubsub = exmpp_xml:append_child(
	    #xmlnselement{ns = ?NS_PUBSUB, name = 'pubsub', children = []},
	    Create),
	Iq = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'}, [
	    {'type', "set"},
	    {'to', Service},
	    {'id', Id}]),
	exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service, Node) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for deleting a node on a pubsub service.
%%
%% The stanza `id' is generated automatically.

pubsub_delete_node(Service, Node) ->
	pubsub_delete_node(pubsub_id(), Service, Node).

%% @spec (Id, Service, Node) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for deleting a node on a pubsub service.

pubsub_delete_node(Id, Service, Node) ->
	% Make the <delete/> element.
	Create = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_PUBSUB, name = 'delete', children = []}, [
	    {'node', Node}]),
	% Prepare the final <iq/>.
	Pubsub = exmpp_xml:append_child(
	    #xmlnselement{ns = ?NS_PUBSUB, name = 'pubsub', children = []},
	    Create),
	Iq = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'}, [
	    {'type', "set"},
	    {'to', Service},
	    {'id', Id}]),
	exmpp_xml:append_child(Iq, Pubsub).

%% @spec (From, Service, Node) -> Pubsub_Iq
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for subscribing to a node on a pubsub service.
%%
%% The stanza `id' is generated automatically.

pubsub_subscribe(From, Service, Node) ->
	pubsub_subscribe(pubsub_id(), From, Service, Node).

%% @spec (Id, From, Service, Node) -> Pubsub_Iq
%%     Id = string()
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for creating a node on a pubsub service.

pubsub_subscribe(Id, From, Service, Node) ->
	% Make the <subscribe/> element.
	Subscribe = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_PUBSUB, name = 'subscribe', children = []}, [
	    {'node', Node},
	    {'jid', From}]),
	% Prepare the final <iq/>.
	Pubsub = exmpp_xml:append_child(
	    #xmlnselement{ns = ?NS_PUBSUB, name = 'pubsub', children = []},
	    Subscribe),
	Iq = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'}, [
	    {'type', "set"},
	    {'to', Service},
	    {'id', Id}]),
	exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service, Node, Items) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Items = [exmpp_xml:xmlnselement() | exmpp_xml:xmlcdata()]
%%     Pubsub_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for publishing an item to a node on a pubsub service.
%%
%% The stanza `id' is generated automatically.

pubsub_publish(Service, Node, Item_Child) when is_tuple(Item_Child) ->
	pubsub_publish(pubsub_id(), Service, Node, [Item_Child]);

pubsub_publish(Service, Node, Item_Children) ->
	pubsub_publish(pubsub_id(), Service, Node, Item_Children).

%% @spec (Id, Service, Node, Items) -> Pubsub_Iq
%%     Id = string()
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     Items = [exmpp_xml:xmlnselement() | exmpp_xml:xmlcdata()]
%%     Pubsub_Iq = exmpp_xml:xmlnselement()
%% @doc Make an `<iq>' for creating a node on a pubsub service.

pubsub_publish(Id, Service, Node, Item_Child) when is_tuple(Item_Child) ->
	pubsub_publish(Id, Service, Node, [Item_Child]);

pubsub_publish(Id, Service, Node, Item_Children) ->
	% Prepare item.
	Item = #xmlnselement{ns = ?NS_PUBSUB, name = 'item',
	    children = Item_Children},
	% Make the <publish/> element.
	Publish = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_PUBSUB, name = 'publish',
	    children = [Item]}, [
	    {'node', Node}]),
	% Prepare the final <iq/>.
	Pubsub = exmpp_xml:append_child(
	    #xmlnselement{ns = ?NS_PUBSUB, name = 'pubsub', children = []},
	    Publish),
	Iq = exmpp_xml:set_attributes(
	    #xmlnselement{ns = ?NS_JABBER_CLIENT, name = 'iq'}, [
	    {'type', "set"},
	    {'to', Service},
	    {'id', Id}]),
	exmpp_xml:append_child(Iq, Pubsub).

%% @spec () -> Pubsub_ID
%%     Pubsub_ID = string()
%% @doc Generate a random pubsub iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

pubsub_id() ->
	"pubsub-" ++ integer_to_list(random:uniform(65536 * 65536)).

