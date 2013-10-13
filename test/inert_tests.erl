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

-define(INERT_STREAM_NUM_BYTES, 1024).

inert_test_() ->
    {ok, Ref} = inert:start(),

    {timeout, 480, [
        {?LINE, fun() -> inert_select(Ref) end},
        {?LINE, fun() -> inert_badfd(Ref) end},
        {?LINE, fun() -> inert_stream(Ref) end},
        {?LINE, fun() -> inert_poll_timeout(Ref) end},
        {?LINE, fun() -> inert_stateless_fdset(Ref) end}
    ]}.

inert_select(Ref) ->
    {ok, Sock1} = gen_tcp:listen(0, [binary,{active,false}]),
    {ok, Sock2} = gen_tcp:listen(0, [binary,{active,false}]),

    {ok, Port1} = inet:port(Sock1),
    {ok, Port2} = inet:port(Sock2),

    {ok, FD1} = inet:getfd(Sock1),
    {ok, FD2} = inet:getfd(Sock2),

    ok = inert:fdset(Ref, FD1),
    ok = inert:fdset(Ref, FD2),

    {ok, C1} = gen_tcp:connect("localhost", Port1, []),
    {ok, C2} = gen_tcp:connect("localhost", Port2, []),

    gen_tcp:close(C1),
    gen_tcp:close(C2),

    receive
        {inert_read, _, FD1} ->
            error_logger:info_report([{fd, FD1}]),
            inert:fdclr(Ref, FD1),
            receive
                {inert_read, _, FD2} ->
                    error_logger:info_report([{fd, FD2}]),
                    inert:fdclr(Ref, FD2),
                    ok
            end
    end.

inert_badfd(Ref) ->
    {error, ebadfd} = inert:fdset(Ref, -1),
    {error, ebadfd} = inert:poll(Ref, -1),
    {error, ebadfd} = inert:fdset(Ref, 127),
    {error, ebadfd} = inert:fdset(Ref, 128),
    {error, ebadfd} = inert:fdset(Ref, 10000).

inert_stream(Ref) ->
    N = case os:getenv("INERT_TEST_STREAM_RUNS") of
        false -> 10;
        Var -> list_to_integer(Var)
    end,

    {ok, Socket} = gen_tcp:listen(0, [binary,{active,false}]),
    {ok, Port} = inet:port(Socket),
    spawn(fun() -> connect(Port, N) end),
    accept(Ref, Socket, N).

accept(Ref,S,N) ->
    accept(Ref,S,N,N).

accept(_Ref, S, X, 0) ->
    wait(S, X);
accept(Ref, S, X, N) ->
    {ok, S1} = gen_tcp:accept(S),
    {ok, FD} = inet:getfd(S1),
    Self = self(),
    spawn(fun() -> read(Ref, Self, FD) end),
    accept(Ref, S, X, N-1).

wait(S, 0) ->
    gen_tcp:close(S);
wait(S, N) ->
    receive
        {fd_close, _FD} ->
            wait(S, N-1)
    end.

read(Ref, Pid, FD) ->
    read(Ref, Pid, FD, 0).
read(Ref, Pid, FD, N) ->
    ok = inert:poll(Ref, FD),
    case procket:read(FD, 1) of
        {ok, <<>>} ->
            procket:close(FD),
            error_logger:info_report([
                    {fd, FD},
                    {read_bytes, N}
                ]),
            N = ?INERT_STREAM_NUM_BYTES,
            Pid ! {fd_close, FD};
        {ok, Buf} ->
            read(Ref, Pid, FD, N + byte_size(Buf));
        {error, eagain} ->
            error_logger:info_report([{fd, FD}, {error, eagain}]),
            read(Ref, Pid, FD, N);
        {error, Error} ->
            error_logger:error_report([{fd, FD}, {error, Error}])
    end.

connect(Port, N) ->
    {ok, C} = gen_tcp:connect("localhost", Port, []),
    Bin = crypto:rand_bytes(?INERT_STREAM_NUM_BYTES),
    ok = gen_tcp:send(C, Bin),
    ok = gen_tcp:close(C),
    connect(Port, N-1).

inert_poll_timeout(Ref) ->
    {ok, Socket} = gen_tcp:listen(0, [binary, {active,false}]),
    {ok, FD} = inet:getfd(Socket),
    timeout = inert:poll(Ref, FD, [{timeout, 10}]).

% Test successive calls to fdset overwrite the previous mode
inert_stateless_fdset(Ref) ->
    {ok, Socket} = gen_tcp:listen(0, [binary, {active,false}]),
    {ok, Port} = inet:port(Socket),
    {ok, FD} = inet:getfd(Socket),

    ok = inert:fdset(Ref, FD, [{mode, read_write}]),
    ok = inert:fdset(Ref, FD, [{mode, write}]),

    {ok, Conn} = gen_tcp:connect("localhost", Port, [binary]),
    ok = gen_tcp:close(Conn),

    ok = receive
        {inert_read, _, FD} = N ->
            N
    after
        0 ->
            ok
    end,

    ok = inert:fdset(Ref, FD, [{mode, read}]),

    receive
        {inert_read, _, FD} ->
            ok
    end.
