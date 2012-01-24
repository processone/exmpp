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

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides functions to handle
%% stream compression.

-module(exmpp_compress).



%% Compression activation.
-export([
	 enable_inflate/2,
	 enable_deflate/0,
	 disable_compression/1
	]).

%% Common socket API.
-export([
	 send/2,
	 recv/1,
	 recv/2,
	 getopts/2,
	 setopts/2,
	 peername/1,
	 sockname/1,
	 controlling_process/2,
	 close/1,
	 recv_data/2,
	 send_data/2
	]).



-record(compress_socket, {socket,
			  inflate,
			  deflate
			 }).


%% --------------------------------------------------------------------
%% Compression activation.
%% --------------------------------------------------------------------

%% pablo Hack:
%%      zlib in otp can only be called from the process that opening and inited it. 
%%      since exmpp_session had two process (one receiving from the socket, other sending to it)
%%      with this ugly thing we start the deflate zlib in the sending process, and the inflate one
%%      in the receiving process. 
%%      Otherwise, we need to made bigger changes to exmpp_compress, exmpp_socket, exmpp_session, etc,  to 
%%      explicitly had two zlibs opened.
enable_deflate() ->
    Zdeflate = zlib:open(),
    zlib:deflateInit(Zdeflate),
    Zdeflate.
 
%% @spec (Socket_Desc, Deflate) -> Compress_Socket
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Compress_Socket = compress_socket()
%% @doc Enable compression over the given socket.
enable_inflate(Socket_Desc, Zdeflate) ->
    Zinflate = zlib:open(),
    zlib:inflateInit(Zinflate),
    #compress_socket{socket = Socket_Desc, 
	    inflate = Zinflate, 
	    deflate = Zdeflate}.

%% @spec (Compress_Socket) -> Socket_Desc
%%     Compress_Socket = compress_socket()
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%% @doc Disable compression and return the underlying socket.

disable_compression(#compress_socket{socket = Socket_Desc, deflate = ZD, inflate = ZI}) ->
    zlib:close(ZD),
    zlib:close(ZI),
    Socket_Desc.


%% --------------------------------------------------------------------
%% Common socket API.
%% --------------------------------------------------------------------

%% @spec (Compress_Socket, Orig_Packet) -> ok | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Orig_Packet = binary() 
%%     Reason = term()
%% @doc Send `Orig_Packet' over a compressed connection.

send(#compress_socket{socket = Socket_Desc, deflate = ZD}, Packet) ->
    try
	    
        Compressed = zlib:deflate(ZD, Packet, sync),
        exmpp_internals:gen_send(Socket_Desc, Compressed)
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (Compress_Socket, Orig_Data) -> {ok, CompressedData} | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Orig_Data = binary() | list()
%%     Reason = term()
%% @doc Compress `Orig_Data' before sending over compressed connection.

send_data(#compress_socket{deflate = ZD}, Data) ->
    try
        Compressed = zlib:deflate(ZD, Data, sync),
        {ok, Compressed}
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (Compress_Socket) -> {ok, Orig_Packet} | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Receive data over a compressed connection.

recv(Compress_Socket) ->
    recv(Compress_Socket, infinity).

%% @spec (Compress_Socket, Timeout) -> {ok, Orig_Packet} | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Timeout = integer()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Receive data over a compressed connection.

recv(#compress_socket{socket = Socket_Desc} = Compress_Socket, Timeout) ->
    try
        case exmpp_internals:gen_recv(Socket_Desc, Timeout) of
            {ok, Packet} ->
                recv_data(Compress_Socket, Packet);
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (Compress_Socket, Packet) -> {ok, Orig_Packet} | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Packet = binary() | list()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Uncompress already received data.

recv_data(#compress_socket{inflate = ZI}, Packet) ->
    try
        Uncompressed = zlib:inflate(ZI, Packet),
	{ok, iolist_to_binary(Uncompressed)}
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (Compress_Socket, Options) -> {ok, Option_Values} | {error, posix()}
%%     Compress_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Options = list()
%%     Option_Values = list()
%% @doc Sets one or more options for a socket.

getopts(#compress_socket{socket = Socket_Desc}, Options) ->
    exmpp_internals:gen_getopts(Socket_Desc, Options).

%% @spec (Compress_Socket, Options) -> ok | {error, posix()}
%%     Compress_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Options = list()
%% @doc Sets one or more options for a socket.

setopts(#compress_socket{socket = Socket_Desc}, Options) ->
    exmpp_internals:gen_setopts(Socket_Desc, Options).

%% @spec (Compress_Socket) -> {ok, {Address, Port}} | {error, posix()}
%%     Compress_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Address = ip_address()
%%     Port = integer()
%% @doc Returns the address and port for the other end of a connection.

peername(#compress_socket{socket = Socket_Desc}) ->
    exmpp_internals:gen_peername(Socket_Desc).

%% @spec (Compress_Socket) -> {ok, {Address, Port}} | {error, posix()}
%%     Compress_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Address = ip_address()
%%     Port = integer()
%% @doc Returns the local address and port number for a socket.

sockname(#compress_socket{socket = Socket_Desc}) ->
    exmpp_internals:gen_sockname(Socket_Desc).

%% @spec (Compress_Socket, Pid) -> ok | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Pid = pid()
%%     Reason = term()
%% @doc Change the controlling socket of the underlying socket.

controlling_process(#compress_socket{socket = Socket_Desc}, Pid) ->
    exmpp_internals:gen_controlling_process(Socket_Desc, Pid).

%% @spec (Compress_Socket) -> ok | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Reason = term()
%% @doc Turn off compression and close the underlying socket.

close(#compress_socket{socket = Socket_Desc} = Compress_Socket) ->
    %% First, turn off compression.
    disable_compression(Compress_Socket),
    %% Close the underlying socket.
    exmpp_internals:gen_close(Socket_Desc).

