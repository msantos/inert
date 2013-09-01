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
-module(inert_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

inert_test_() ->
    {setup, fun inert:start/0,
        {timeout, 480, [
            {?LINE, fun inert_select/0},
            {?LINE, fun inert_badfd/0},
            {?LINE, fun inert_stream/0}
        ]}}.


inert_select() ->
    {ok, Sock1} = gen_tcp:listen(0, [binary,{active,false}]),
    {ok, Sock2} = gen_tcp:listen(0, [binary,{active,false}]),

    {ok, Port1} = inet:port(Sock1),
    {ok, Port2} = inet:port(Sock2),

    {ok, FD1} = inet:getfd(Sock1),
    {ok, FD2} = inet:getfd(Sock2),

    ok = inert:set(FD1),
    ok = inert:set(FD2),

    {ok, C1} = gen_tcp:connect("localhost", Port1, []),
    {ok, C2} = gen_tcp:connect("localhost", Port2, []),

    gen_tcp:close(C1),
    gen_tcp:close(C2),

    receive
        {inert, _, FD1} ->
            error_logger:info_report([{fd, FD1}]),
            inert:clr(FD1),
            receive
                {inert, _, FD2} ->
                    error_logger:info_report([{fd, FD2}]),
                    inert:clr(FD2),
                    ok
            end
    end.

inert_badfd() ->
    {error, ebadfd} = inert:set(-1).

inert_stream() ->
    N = 200,

    {ok, Socket} = gen_tcp:listen(0, [binary,{active,false}]),
    {ok, Port} = inet:port(Socket),
    spawn(fun() -> connect(Port, N) end),
    accept(Socket, N).

accept(S,N) ->
    accept(S,N,N).

accept(S, X, 0) ->
    wait(S, X);
accept(S, X, N) ->
    {ok, S1} = gen_tcp:accept(S),
    {ok, FD} = inet:getfd(S1),
    Self = self(),
    spawn(fun() -> read(Self, FD) end),
    accept(S, X, N-1).

wait(S, 0) ->
    gen_tcp:close(S);
wait(S, N) ->
    receive
        {fd_close, _FD} ->
            wait(S, N-1)
    end.

read(Pid, FD) ->
    error_logger:info_report([{set, FD}]),
    inert:set(FD),
    receive
        {inert, _Port, FD} ->
            case procket:read(FD, 1024) of
                {ok, Buf} when byte_size(Buf) =:= 0 ->
                    procket:close(FD),
                    Pid ! {fd_close, FD};
                {ok, Buf} ->
                    error_logger:info_report([{fd, FD}, {size, byte_size(Buf)}, {buf, Buf}]),
                    read(Pid, FD);
                {error, eagain} ->
                    error_logger:info_report([{fd, FD}, {error, eagain}]),
                    read(Pid, FD);
                {error, Error} ->
                    error_logger:error_report([{fd, FD}, {error, Error}])
            end
    end.

connect(Port, N) ->
    {ok, C} = gen_tcp:connect("localhost", Port, []),
    %ok = gen_tcp:send(C, <<"abc1234567890">>),
    {ok, Bin} = file:read_file("/etc/passwd"),
    ok = gen_tcp:send(C, <<Bin/binary, Bin/binary, Bin/binary>>),
    ok = gen_tcp:close(C),
    connect(Port, N-1).
