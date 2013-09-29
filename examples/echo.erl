%%% Copyright (c) 2013, Michael Santos <michael.santos@gmail.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(echo).
-include_lib("procket/include/procket.hrl").

-export([
        listen/0, listen/1
    ]).

listen() ->
    listen(8081).

listen(Port) ->
    {ok, Ref} = inert:start(),

    {ok, Socket} = procket:socket(inet, stream, 0),
    Sockaddr = <<
        (procket:sockaddr_common(?PF_INET, 16))/binary,
        Port:16,        % Port
        0,0,0,0,        % IPv4 ANY address
        0:64
        >>,
    BACKLOG = 50,
    ok = procket:bind(Socket, Sockaddr),
    ok = procket:listen(Socket, BACKLOG),

    accept(Ref, Socket).

accept(Ref, Listen) ->
    ok = inert:poll(Ref, Listen),
    {ok, Socket} = procket:accept(Listen),
    error_logger:info_report([{accept, Socket}]),
    spawn(fun() -> echo(Ref, Socket) end),
    accept(Ref, Listen).

echo(Ref, Socket) ->
    ok = inert:poll(Ref, Socket),
    case procket:read(Socket, 16#ffff) of
        {ok, <<>>} ->
            error_logger:info_report([{close, Socket}]),
            ok = procket:close(Socket),
            ok;
        {ok, Buf} ->
            ok = inert:poll(Ref, Socket, [{mode, write}]),
            ok = procket:write(Socket, Buf),
            echo(Ref, Socket)
    end.
