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


% --------------------------------------------------------------------
% Records to represent XML nodes.
% --------------------------------------------------------------------

% Note: The records defined here are documented in exmpp_xml.

% Character data.
-record(xmlcdata, {
  cdata = <<>>     :: binary()
}).

% Attributes.
-record(xmlattr, {
  ns = undefined   :: exmpp_xml:xmlname() | undefined,
  name             :: exmpp_xml:xmlname(),
  value            :: binary()
}).

% Elements.
-record(xmlel, {
  ns = undefined   :: exmpp_xml:xmlname() | undefined,
  declared_ns = [] :: [{exmpp_xml:xmlname(), string() | none}],
  name             :: exmpp_xml:xmlname(),
  attrs = []       :: [exmpp_xml:xmlattr()],
  children = []    :: [#xmlel{} | exmpp_xml:xmlcdata()] | undefined
}).

% XML end tag.
% To use when 'children' is undefined in xmlel or xmlelement.
-record(xmlendtag, {
  ns = undefined   :: exmpp_xml:xmlname() | undefined,
  name             :: exmpp_xml:xmlname()
}).

% Old record for xmlel.
-record(xmlelement, {
  name             :: exmpp_xml:xmlname(),
  attrs = []       :: [exmpp_xml:xmlattr_old()],
  children = []    :: [#xmlelement{} | exmpp_xml:xmlcdata()] | undefined
}).

% Processing Instruction.
-record(xmlpi, {
  target           :: binary(),
  value            :: binary()
}).

% --------------------------------------------------------------------
% Macros to help creation of XML nodes.
% --------------------------------------------------------------------

-define(XMLEL1(Name),
  exmpp_xml:element(Name)).
-define(XMLEL2(NS, Name),
  exmpp_xml:element(NS, Name)).
-define(XMLEL4(NS, Name, Attrs, Children),
  exmpp_xml:element(NS, Name, Attrs, Children)).

-define(XMLATTR(Name, Value),
  exmpp_xml:attribute(Name, Value)).

-define(XMLCDATA(CData),
  exmpp_xml:cdata(CData)).
