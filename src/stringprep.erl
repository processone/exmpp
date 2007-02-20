%%%----------------------------------------------------------------------
%%% File    : stringprep.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Interface to exmpp_stringprep_drv
%%% Created : 16 Feb 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(stringprep).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([
  start/0,
  start_link/0,
  tolower/1,
  nameprep/1,
  nodeprep/1,
  resourceprep/1
]).

start() ->
    exmpp_stringprep:start().

start_link() ->
    exmpp_stringprep:start_link().

tolower(String) ->
    exmpp_stringprep:tolower(String).

nameprep(String) ->
    exmpp_stringprep:nameprep(String).

nodeprep(String) ->
    exmpp_stringprep:nodeprep(String).

resourceprep(String) ->
    exmpp_stringprep:resourceprep(String).
