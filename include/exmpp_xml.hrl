% $Id$

% --------------------------------------------------------------------
% Records to represent XML nodes.
% --------------------------------------------------------------------

% Elements without namespace support.
-record(xmlelement, {
  name,                   % Element name
  attrs = [],             % Attributes list
  children = []           % Children (elements or CDATA)
}).

% Elements WITH namespace support.
-record(xmlel, {
  ns = undefined,         % Element namespace
  declared_ns = [],       % Declared namespaces in this element
  name,                   % Element name
  attrs = [],             % Attributes list
  children = []           % Children (elements or CDATA)
}).

% Attributes WITH namespace support.
-record(xmlattr, {
  ns = undefined,
  prefix = undefined,
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
  prefix = undefined,
  name
}).

% Processing Instruction.
-record(xmlpi, {
  target,
  value
}).
