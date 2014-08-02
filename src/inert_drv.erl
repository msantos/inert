%%% Copyright (c) 2013-2014, Michael Santos <michael.santos@gmail.com>
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

        ctl/3
    ]).

-export_type([command/0]).

-type command() :: 'fdset' | 'fdclr'.

-define(INERT_FDSET, 1).
-define(INERT_FDCLR, 2).

-define(ERL_DRV_READ, (1 bsl 0)).
-define(ERL_DRV_WRITE, (1 bsl 1)).
-define(ERL_DRV_USE, (1 bsl 2)).

-define(INT32(N), N:4/big-signed-integer-unit:8).

start() ->
    case erl_ddll:load(priv_dir(), ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> exit({error, erl_ddll:format_error(Error)})
    end.

stop() ->
    erl_ddll:unload(?MODULE).

-spec ctl(port(), command() | non_neg_integer(), iodata()) -> 'ok' | {'error', file:posix() | 'closed'}.
ctl(Port, Op, Data) when is_atom(Op) ->
    ctl(Port, command(Op), Data);
ctl(Port, Op, Data) ->
    try erlang:port_control(Port, Op, Data) of
        [] -> ok;
        Error -> {error, list_to_atom(Error)}
    catch
        error:badarg -> {error, closed}
    end.

command(fdset) -> ?INERT_FDSET;
command(fdclr) -> ?INERT_FDCLR.

encode(FD) when is_integer(FD) -> encode({FD, read});
encode({FD, read}) ->
    [<<?INT32(FD), ?INT32(?ERL_DRV_READ)>>];
encode({FD, write}) ->
    [<<?INT32(FD), ?INT32(?ERL_DRV_WRITE)>>];
encode({FD, read_write}) ->
    [<<?INT32(FD), ?INT32((?ERL_DRV_READ bor ?ERL_DRV_WRITE))>>].

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]);
        Dir ->
            Dir
    end.
