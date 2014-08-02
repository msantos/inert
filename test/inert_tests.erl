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
-module(inert_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

inert_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun run/1
    }.

run(PollId) ->
    [
        inert_badfd(PollId),
        inert_select(PollId),
        inert_stream(PollId),
        inert_poll_timeout(PollId),
        inert_stateless_fdset(PollId),
        inert_controlling_process(PollId),
        inert_error_closed()
    ].

start() ->
    inert:start().

stop(PollId) ->
    inert:stop(PollId).

inert_badfd(PollId) ->
    [
        ?_assertEqual({error, ebadf}, inert:fdset(PollId, -1)),
        ?_assertEqual({error, ebadf}, inert:poll(PollId, -1)),
        ?_assertEqual({error, ebadf}, inert:fdset(PollId, 127)),
        ?_assertEqual({error, ebadf}, inert:fdset(PollId, 128)),
        ?_assertEqual({error, ebadf}, inert:fdset(PollId, 10000))
    ].

inert_select(PollId) ->
    {ok, Sock1} = gen_tcp:listen(0, [binary,{active,false}]),
    {ok, Sock2} = gen_tcp:listen(0, [binary,{active,false}]),

    {ok, Port1} = inet:port(Sock1),
    {ok, Port2} = inet:port(Sock2),

    {ok, FD1} = inet:getfd(Sock1),
    {ok, FD2} = inet:getfd(Sock2),

    ok = inert:fdset(PollId, FD1),
    ok = inert:fdset(PollId, FD2),

    {ok, C1} = gen_tcp:connect("localhost", Port1, []),
    {ok, C2} = gen_tcp:connect("localhost", Port2, []),

    gen_tcp:close(C1),
    gen_tcp:close(C2),

    Result = receive
        {inert_read, _, FD1} ->
%            error_logger:info_report([{fd, FD1}]),
            inert:fdclr(PollId, FD1),
            receive
                {inert_read, _, FD2} ->
%                    error_logger:info_report([{fd, FD2}]),
                    inert:fdclr(PollId, FD2)
            end
    end,
    ?_assertEqual(ok, Result).

inert_stream(PollId) ->
    N = getenv("INERT_TEST_STREAM_RUNS", 10),

    {ok, Socket} = gen_tcp:listen(0, [binary,{active,false}]),
    {ok, Port} = inet:port(Socket),
    spawn(fun() -> connect(Port, N) end),
    accept(PollId, Socket, N).

accept(PollId,S,N) ->
    accept(PollId,S,N,N).

accept(_PollId, S, X, 0) ->
    wait(S, X);
accept(PollId, S, X, N) ->
    {ok, S1} = gen_tcp:accept(S),
    {ok, FD} = inet:getfd(S1),
    Self = self(),
    spawn(fun() -> read(PollId, Self, FD) end),
    accept(PollId, S, X, N-1).

wait(S, 0) ->
    ?_assertEqual(ok, gen_tcp:close(S));
wait(S, N) ->
    receive
        {fd_close, _FD} ->
            wait(S, N-1)
    end.

read(PollId, Pid, FD) ->
    read(PollId, Pid, FD, 0).
read(PollId, Pid, FD, N) ->
    ok = inert:poll(PollId, FD),
    case procket:read(FD, 1) of
        {ok, <<>>} ->
            procket:close(FD),
%            error_logger:info_report([
%                    {fd, FD},
%                    {read_bytes, N}
%                ]),
            N = getenv("INERT_TEST_STREAM_NUM_BYTES", 1024),
            Pid ! {fd_close, FD};
        {ok, Buf} ->
            read(PollId, Pid, FD, N + byte_size(Buf));
        {error, eagain} ->
            error_logger:info_report([{fd, FD}, {error, eagain}]),
            read(PollId, Pid, FD, N);
        {error, Error} ->
            error_logger:error_report([{fd, FD}, {error, Error}])
    end.

connect(Port, N) ->
    {ok, C} = gen_tcp:connect("localhost", Port, []),
    Num = getenv("INERT_TEST_STREAM_NUM_BYTES", 1024),
    Bin = crypto:rand_bytes(Num),
    ok = gen_tcp:send(C, Bin),
    ok = gen_tcp:close(C),
    connect(Port, N-1).

inert_poll_timeout(PollId) ->
    {ok, Socket} = gen_tcp:listen(0, [binary, {active,false}]),
    {ok, FD} = inet:getfd(Socket),
    ?_assertEqual({error, timeout}, inert:poll(PollId, FD, [{timeout, 10}])).

% Test successive calls to fdset overwrite the previous mode
inert_stateless_fdset(PollId) ->
    {ok, Socket} = gen_tcp:listen(0, [binary, {active,false}]),
    {ok, Port} = inet:port(Socket),
    {ok, FD} = inet:getfd(Socket),

    ok = inert:fdset(PollId, FD, [{mode, read_write}]),
    ok = inert:fdset(PollId, FD, [{mode, write}]),

    {ok, Conn} = gen_tcp:connect("localhost", Port, [binary]),
    ok = gen_tcp:close(Conn),

    ok = receive
        {inert_read, _, FD} = Fail ->
            Fail
    after
        0 ->
            ok
    end,

    ok = inert:fdset(PollId, FD, [{mode, read}]),

    Result = receive
        {inert_read, _, FD} = N ->
            N
    end,
    ?_assertMatch({inert_read, _, FD}, Result).

% Pass port ownership through a ring of processes
inert_controlling_process(PollId) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active,false}]),
    {ok, FD} = inet:getfd(Socket),
    Pid = self(),
    inert_controlling_process(PollId, Pid, FD, 0),
    Result = receive
        inert_controlling_process ->
            N = inert:poll(PollId, FD, [{mode, write}]),
            gen_udp:close(Socket),
            N
    end,
    ?_assertEqual(ok, Result).

inert_controlling_process(PollId, Parent, _FD, 3) ->
    ok = inert:controlling_process(PollId, Parent),
    Parent ! inert_controlling_process;
inert_controlling_process(PollId, Parent, FD, N) ->
    ok = inert:poll(PollId, FD, [{mode, write}]),
    Pid1 = spawn(fun() -> inert_controlling_process(PollId, Parent, FD, N+1) end),
    ok = inert:controlling_process(PollId, Pid1).

% Catch the badarg if the port has been closed
inert_error_closed() ->
    PollId = inert:start(),
    ok = inert:stop(PollId),
    ?_assertEqual({error, closed}, inert:poll(PollId, 1)).

getenv(Var, Default) when is_list(Var), is_integer(Default) ->
    case os:getenv(Var) of
        false -> Default;
        N -> list_to_integer(N)
    end.
