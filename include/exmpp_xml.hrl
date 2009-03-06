% $Id$

% --------------------------------------------------------------------
% Type definition.
% --------------------------------------------------------------------

% NS, element's name and attribute's name.
-type(xmlname() :: atom() | string()).

% Structures used by XML serialization functions.
-type(xmldefaultns()  :: xmlname() | [xmlname()]).
-type(xmldefaultnss()  :: [xmldefaultns()]).
-type(xmlprefixednss() :: [{xmlname(), string()}]).

% Path description (to be used in exmpp_xml:get_path/2).
-type(xmlpathcomponent() ::
  {element, xmlname()} |
  {element, xmlname(), xmlname()} |
  {attribute, xmlname()} |
  {attribute, xmlname(), xmlname()} |
  cdata |
  cdata_as_list).
-type(xmlpath() :: [xmlpathcomponent()]).

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
  ns = undefined   :: xmlname() | undefined,
  name             :: xmlname(),
  value            :: binary()
}).

% Old attribute isn't represented by a record.
-type(xmlattr_old() :: {xmlname(), string()}).

% Elements.
-record(xmlel, {
  ns = undefined   :: xmlname() | undefined,
  declared_ns = [] :: [{xmlname(), string() | none}],
  name             :: xmlname(),
  attrs = []       :: [#xmlattr{} | xmlattr_old()],
  children = []    :: [#xmlel{} | #xmlcdata{}] | undefined
}).

% XML end tag.
% To use when 'children' is undefined in xmlel or xmlelement.
-record(xmlendtag, {
  ns = undefined   :: xmlname() | undefined,
  name             :: xmlname()
}).

% Old record for xmlel.
-record(xmlelement, {
  name             :: xmlname(),
  attrs = []       :: [xmlattr_old()],
  children = []    :: [#xmlelement{} | #xmlcdata{}] | undefined
}).

% Processing Instruction.
-record(xmlpi, {
  target           :: binary(),
  value            :: binary()
}).

-type(xmlel()    :: #xmlel{} | #xmlelement{}).
-type(xmlchild() :: #xmlel{} | #xmlelement{} | #xmlcdata{}).

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
