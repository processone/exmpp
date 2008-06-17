%% Copyright 2007, Process-one

%% This record is used to pass received packets back to client.
%% The record is defined to make it easy to use pattern matching on
%% the most used data received.
-record(received_packet,
        {
          packet_type, % message, iq, presence
          type_attr,   % depend on packet. Example: set, get, subscribe, etc
          from,        % JID
          id,          % Packet ID
          queryns,     % IQ only: Namespace of the query
          raw_packet   % raw exmpp record
        }).
