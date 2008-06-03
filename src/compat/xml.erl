% $Id$

%%%----------------------------------------------------------------------
%%% File    : xml.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : XML utils
%%% Created : 20 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   Process-one
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%                         
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(xml).
-author('alexey@process-one.net').

-include("exmpp.hrl").

-export([element_to_string/1,
	 crypt/1, make_text_node/1,
	 remove_cdata/1,
	 get_cdata/1, get_tag_cdata/1,
	 get_attr/2, get_attr_s/2,
	 get_tag_attr/2, get_tag_attr_s/2,
	 get_subtag/2, get_subtag_cdata/2,
	 get_path_s/2,
	 replace_tag_attr/3]).

%% Select at compile time how to escape characters in binary text
%% nodes.
%% Can be choosen with ./configure --enable-full-xml
-ifdef(FULL_XML_SUPPORT).
-define(ESCAPE_BINARY(CData), make_text_node(CData)).
-else.
-define(ESCAPE_BINARY(CData), crypt(CData)).
-endif.

element_to_string(El) ->
    exmpp_xml:document_to_list(El).

crypt(S) ->
    exmpp_xml:encode_entities(S).

%% Make a cdata_binary depending on what characters it contains
make_text_node(CData) ->
    case cdata_need_escape(CData) of
	cdata ->
	    CDATA1 = <<"<![CDATA[">>,
	    CDATA2 = <<"]]>">>,
	    concat_binary([CDATA1, CData, CDATA2]);
	none ->
	    CData;
	{cdata, EndTokens} ->
	    EscapedCData = escape_cdata(CData, EndTokens),
	    concat_binary(EscapedCData)
    end.

%% Returns escape type needed for the text node
%% none, cdata, {cdata, [Positions]}
%% Positions is a list a integer containing positions of CDATA end
%% tokens, so that they can be escaped
cdata_need_escape(CData) ->
    cdata_need_escape(CData, 0, false, []).
cdata_need_escape(<<>>, _, false, _) ->
    none;
cdata_need_escape(<<>>, _, true, []) ->
    cdata;
cdata_need_escape(<<>>, _, true, CDataEndTokens) ->
    {cdata, lists:reverse(CDataEndTokens)};
cdata_need_escape(<<$],$],$>,Rest/binary>>, CurrentPosition,
                  _XMLEscape, CDataEndTokens) ->
    NewPosition = CurrentPosition + 3,
    cdata_need_escape(Rest, NewPosition, true,
                      [CurrentPosition+1|CDataEndTokens]);
%% Only <, & need to be escaped in XML text node
%% See reference: http://www.w3.org/TR/xml11/#syntax
cdata_need_escape(<<$<,Rest/binary>>, CurrentPosition,
                  _XMLEscape, CDataEndTokens) ->
    cdata_need_escape(Rest, CurrentPosition+1, true, CDataEndTokens);
cdata_need_escape(<<$&,Rest/binary>>, CurrentPosition,
                  _XMLEscape, CDataEndTokens) ->
    cdata_need_escape(Rest, CurrentPosition+1, true, CDataEndTokens);
cdata_need_escape(<<_:8,Rest/binary>>, CurrentPosition,
		  XMLEscape, CDataEndTokens) ->
    cdata_need_escape(Rest, CurrentPosition+1, XMLEscape,
                      CDataEndTokens).

%% escape cdata that contain CDATA end tokens
%% EndTokens is a list of position of end tokens (integer)
%% This is supposed to be a very rare case: You need to generate several
%% fields, splitting it in the middle of the end token.
%% See example: http://en.wikipedia.org/wiki/CDATA#Uses_of_CDATA_sections
escape_cdata(CData, EndTokens) ->
    escape_cdata(CData, 0, EndTokens, []).
escape_cdata(<<>>, _CurrentPosition, [], Acc) ->
    lists:reverse(Acc);
escape_cdata(Rest, CurrentPosition, [], Acc) ->
    CDATA1 = <<"<![CDATA[">>,
    CDATA2 = <<"]]>">>,
    escape_cdata(<<>>, CurrentPosition, [], [CDATA2, Rest, CDATA1|Acc]);
escape_cdata(CData, Index, [Pos|Positions], Acc) ->
    CDATA1 = <<"<![CDATA[">>,
    CDATA2 = <<"]]>">>,
    Split = Pos-Index,
    {Part, Rest} = split_binary(CData, Split+1),
    %% Note: We build the list in reverse to optimize construction
    escape_cdata(Rest, Pos+1, Positions, [CDATA2, Part, CDATA1|Acc]).

remove_cdata(L) -> exmpp_xml:remove_cdata_from_list(L).

get_cdata(L) ->
    exmpp_xml:get_cdata_from_list(L).

get_tag_cdata({xmlelement, _Name, _Attrs, Els}) ->
    get_cdata(Els).

get_attr(AttrName, Attrs) ->
    case exmpp_xml:get_attribute_node_from_list(Attrs, AttrName) of
	undefined ->
	    false;
	#xmlattr{value = Val} ->
	    {value, Val};
	{_Name, Val} ->
	    {value, Val}
    end.

get_attr_s(AttrName, Attrs) ->
    exmpp_xml:get_attribute_from_list(Attrs, AttrName).

get_tag_attr(AttrName, #xmlnselement{attrs = Attrs}) ->
    get_attr(AttrName, Attrs);
get_tag_attr(AttrName, #xmlelement{attrs = Attrs}) ->
    get_attr(AttrName, Attrs).

get_tag_attr_s(AttrName, El) ->
    exmpp_xml:get_attribute(El, AttrName).


get_subtag(El, Name) ->
    case exmpp_xml:get_element_by_name(El, Name) of
	undefined -> false;
	Sub_El    -> Sub_El
    end.

get_subtag_cdata(Tag, Name) ->
    exmpp_xml:get_cdata(Tag, Name).

get_path_s(El, []) ->
    El;
get_path_s(El, [{elem, Name} | Path]) ->
    case get_subtag(El, Name) of
	false ->
	    "";
	SubEl ->
	    get_path_s(SubEl, Path)
    end;
get_path_s(El, [{attr, Name}]) ->
    get_tag_attr_s(Name, El);
get_path_s(El, [cdata]) ->
    get_tag_cdata(El).


replace_tag_attr(Attr, Value, El) ->
    exmpp_xml:set_attribute(El, Attr, Value).


