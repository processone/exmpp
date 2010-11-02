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


% Records to represent Caps.
%% --------------------------------------------------------------------
%% Documentation / type definitions.
%% --------------------------------------------------------------------

-record(field,
	{
	  var    = "" :: exmpp_caps:xvar(),
	  values = [] :: [exmpp_caps:value()]
			 }).

-record(form,
	{
	  type   = "" :: exmpp_caps:formtype(),
	  fields = [] :: exmpp_caps:field() | [exmpp_caps:field()]
			 }).

%
-record(identity,
	{
	  category = "" :: exmpp_caps:identitycategory(),
	  type     = "" :: exmpp_caps:identitytype(),
	  lang     = "" :: exmpp_caps:identitylang(),
	  name     = "" :: exmpp_caps:identityname()
   }).


%
-record(ecaps,
	{
	  identities = [] :: exmpp_caps:identity() | [exmpp_caps:identity()],
	  features   = [] :: [exmpp_caps:ns()],
	  forms      = [] :: exmpp_caps:form()     | [exmpp_caps:form()]
			     }).
