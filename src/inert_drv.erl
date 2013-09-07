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
-module(inert_drv).
-export([
        start/0,
        stop/0,

        encode/1,
        decode/1,

        send/3
    ]).

-define(INERT_FDSET, 1).
-define(INERT_FDCLR, 2).

start() ->
    case erl_ddll:load_driver(priv_dir(), ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> exit({error, erl_ddll:format_error(Error)})
    end.

stop() ->
    ok.

send(Port, Op, Data) when is_atom(Op) ->
    send(Port, command(Op), Data);
send(Port, Op, Data) ->
    case erlang:port_control(Port, Op, Data) of
        [] -> ok;
        Error -> {error, list_to_atom(Error)}
    end.

command(fdset) -> ?INERT_FDSET;
command(fdclr) -> ?INERT_FDCLR.

encode(FD) -> [<<FD:4/big-signed-integer-unit:8>>].

decode([A,B,C,D]) -> (A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D;
decode(<<N:4/big-signed-integer-unit:8>>) -> N.

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]);
        Dir ->
            Dir
    end.
