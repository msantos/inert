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
-module(echo).
-include_lib("procket/include/procket.hrl").

-export([
    listen/0, listen/1
]).

listen() ->
    listen(0).

listen(Port) ->
    ok = inert:start(),

    {ok, Socket} = procket:socket(inet, stream, 0),
    try
        Sockaddr = <<
            (procket:sockaddr_common(procket:family(inet), 16))/binary,
            % Port
            Port:16,
            % IPv4 ANY address
            0,
            0,
            0,
            0,
            0:64
        >>,
        BACKLOG = 1024,
        ok = procket:bind(Socket, Sockaddr),
        ok = procket:listen(Socket, BACKLOG),

        {ok, <<_:16, ListenPort:16, _/binary>>} = procket:getsockname(Socket, Sockaddr),
        error_logger:info_report([{listening, ListenPort}])
    catch
        Error:Message ->
            procket:close(Socket),
            erlang:error({Error, Message})
    end,

    accept(Socket).

accept(Listen) ->
    inert:poll(Listen),
    try procket:accept(Listen) of
        {ok, Socket} ->
            error_logger:info_report([{accept, Socket}]),
            spawn(fun() -> echo(Socket) end);
        {error, eagain} ->
            ok;
        Error ->
            procket:close(Listen),
            erlang:error(Error)
    catch
        Error:Message ->
            procket:close(Listen),
            erlang:error({Error, Message})
    end,
    accept(Listen).

echo(Socket) ->
    inert:poll(Socket),
    case procket:read(Socket, 16#ffff) of
        {ok, <<>>} ->
            error_logger:info_report([{close, Socket}]),
            procket:close(Socket);
        {ok, Buf} ->
            reply(Socket, Buf);
        {error, eagain} ->
            echo(Socket);
        {error, _} = Error ->
            error_logger:info_report([{read, Error}]),
            procket:close(Socket)
    end.

reply(Socket, Buf) ->
    inert:poll(Socket, write),
    case procket:write(Socket, Buf) of
        ok ->
            echo(Socket);
        {error, eagain} ->
            reply(Socket, Buf);
        {error, _} = Error ->
            error_logger:info_report([{write, Error}]),
            procket:close(Socket)
    end.
