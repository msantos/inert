%%% Copyright (c) 2013-2021, Michael Santos <michael.santos@gmail.com>
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
-module(conn).
-include_lib("procket/include/procket.hrl").

-export([ssh/0]).

-ifndef(SO_ERROR).
-define(SO_ERROR, 4).
-endif.

ssh() ->
    ok = inert:start(),
    {ok, Socket} = procket:socket(inet, stream, 0),
    try
        Sockaddr = <<
            (procket:sockaddr_common(?PF_INET, 16))/binary,
            % Port
            22:16,
            % IPv4 loopback
            127,
            0,
            0,
            1,
            0:64
        >>,
        ok =
            case procket:connect(Socket, Sockaddr) of
                ok ->
                    ok;
                {error, einprogress} ->
                    poll(Socket)
            end,
        {ok, read} = inert:poll(Socket, read),
        procket:read(Socket, 16#ffff)
    after
        procket:close(Socket)
    end.

poll(Socket) ->
    {ok, write} = inert:poll(Socket, write),
    case procket:getsockopt(Socket, ?SOL_SOCKET, ?SO_ERROR, <<>>) of
        {ok, _Buf} ->
            ok;
        {error, _} = Error ->
            Error
    end.
