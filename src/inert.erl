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
-module(inert).
-export([
        start/0,
        stop/0,

        set/1,
        clr/1,

        poll/1, poll/2
    ]).

start() ->
    inert_drv:start().

stop() ->
    inert_drv:stop().

set(FD) ->
    inert_drv:set(FD).

clr(FD) ->
    inert_drv:clr(FD).

poll(FD) ->
    poll(FD, []).

poll(FD, Options) ->
    Timeout = proplists:get_value(timeout, Options, infinity),
    case set(FD) of
        ok ->
            poll_1(FD, Timeout);
        Error ->
            Error
    end.

poll_1(FD, Timeout) ->
    receive
        {inert, _, FD} ->
            ok;
        {inert_error, _, Error} ->
            {error, Error}
    after
        Timeout ->
            {error, eintr}
    end.
