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
-module(inert).

%% API
-export([start/0, stop/0]).
-export([
        fdset/1, fdset/2,
        fdclr/1, fdclr/2,
        poll/1, poll/2,
        pollid/0,

        controlling_process/1
    ]).

start() ->
    Port = prim_inert:start(),
    try register(inert, Port)
    catch
        error:badarg -> ok
    end,
    ok.

stop() ->
    prim_inert:stop(inert).

pollid() ->
    whereis(inert).

-spec fdset(integer()) -> 'ok' | {'error',file:posix() | 'closed'}.
fdset(FD) ->
    fdset(FD, []).

-spec fdset(integer(), proplists:proplist()) -> 'ok' | {'error',file:posix() | 'closed'}.
fdset(FD, Options) ->
    prim_inert:fdset(inert, FD, Options).

-spec fdclr(integer()) -> 'ok' | {'error',file:posix() | 'closed'}.
fdclr(FD) ->
    fdclr(FD, []).

-spec fdclr(integer(), proplists:proplist()) -> 'ok' | {'error',file:posix() | 'closed'}.
fdclr(FD, Options) ->
    prim_inert:fdclr(inert, FD, Options).

-spec poll(integer()) -> 'ok' | {'error',file:posix() | 'closed' | 'timeout'}.
poll(FD) ->
    poll(FD, []).

-spec poll(integer(), proplists:proplist()) -> 'ok' | {'error',file:posix() | 'closed' | 'timeout'}.
poll(FD, Options) ->
    prim_inert:poll(inert, FD, Options).

-spec controlling_process(pid()) -> 'ok' | {'error', 'not_owner' | 'einval'}.
controlling_process(Pid) when is_pid(Pid) ->
    prim_inert:controlling_process(inert, Pid).
