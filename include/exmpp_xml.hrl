% $Id$

% --------------------------------------------------------------------
% Records to represent XML nodes.
% --------------------------------------------------------------------

% Elements.
-record(xmlel, {
  ns = undefined,         % Element namespace
  declared_ns = [],       % Declared namespaces in this element
  name,                   % Element name
  attrs = [],             % Attributes list
  children = []           % Children (elements or CDATA)
}).

% Attributes.
-record(xmlattr, {
  ns = undefined,
  name,
  value
}).

% Character data.
-record(xmlcdata, {
  cdata = []              % Character data
}).

% XML end tag.
% To use when 'children' is undefined in xmlel or xmlelement.
-record(xmlendtag, {
  ns = undefined,
  name
}).

% Processing Instruction.
-record(xmlpi, {
  target,
  value
}).

% Old record for xmlel.
-record(xmlelement, {
  name,                   % Element name
  attrs = [],             % Attributes list
  children = []           % Children (elements or CDATA)
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
