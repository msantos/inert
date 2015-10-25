%%% Copyright (c) 2013-2015, Michael Santos <michael.santos@gmail.com>
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

run(_) ->
    [
        inert_badfd(),
        inert_select(),
        inert_stream(),
        inert_poll_read_write(),
        inert_poll_timeout(),
        inert_poll_race_timeout(),
        inert_stateless_fdset(),
        inert_controlling_process(),
        inert_error_closed(),
        inert_fdownership()
    ].

start() ->
    inert:start().

stop(_) ->
    inert:stop().

inert_badfd() ->
    [
        ?_assertEqual({error, ebadf}, inert:fdset(-1)),
        ?_assertEqual({error, ebadf}, inert:poll(-1)),
        ?_assertEqual({error, ebadf}, inert:fdset(127)),
        ?_assertEqual({error, ebadf}, inert:fdset(128)),
        ?_assertEqual({error, ebadf}, inert:fdset(10000))
    ].

inert_select() ->
    {ok, Sock1} = gen_tcp:listen(0, [binary,{active,false}]),
    {ok, Sock2} = gen_tcp:listen(0, [binary,{active,false}]),

    {ok, Port1} = inet:port(Sock1),
    {ok, Port2} = inet:port(Sock2),

    {ok, FD1} = inet:getfd(Sock1),
    {ok, FD2} = inet:getfd(Sock2),

    ok = inert:fdset(FD1),
    ok = inert:fdset(FD2),

    {ok, C1} = gen_tcp:connect("localhost", Port1, []),
    {ok, C2} = gen_tcp:connect("localhost", Port2, []),

    gen_tcp:close(C1),
    gen_tcp:close(C2),

    Result = receive
        {inert_read, _, FD1} ->
%            error_logger:info_report([{fd, FD1}]),
            inert:fdclr(FD1),
            receive
                {inert_read, _, FD2} ->
%                    error_logger:info_report([{fd, FD2}]),
                    inert:fdclr(FD2)
            end
    end,
    ?_assertEqual(ok, Result).

inert_stream() ->
    N = getenv("INERT_TEST_STREAM_RUNS", 10),

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
    ?_assertEqual(ok, gen_tcp:close(S));
wait(S, N) ->
    receive
        {fd_close, _FD} ->
            wait(S, N-1)
    end.

read(Pid, FD) ->
    read(Pid, FD, 0).
read(Pid, FD, N) ->
    {ok,read} = inert:poll(FD),
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
            read(Pid, FD, N + byte_size(Buf));
        {error, eagain} ->
            error_logger:info_report([{fd, FD}, {error, eagain}]),
            read(Pid, FD, N);
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

inert_poll_read_write() ->
    {ok, Socket} = gen_udp:open(0, [{active,false}]),
    {ok, FD} = inet:getfd(Socket),
    {ok,_} = inert:poll(FD, read_write),
    PollId = inert:pollid(),
    Reply = receive
        {Tag, PollId, FD} when Tag == inert_read; Tag == inert_write ->
            {error, Tag}
    after
        10 ->
            ok
    end,
    gen_udp:close(Socket),
    ?_assertEqual(ok, Reply).

inert_poll_timeout() ->
    {ok, Socket} = gen_tcp:listen(0, [binary, {active,false}]),
    {ok, FD} = inet:getfd(Socket),
    ?_assertEqual({error, timeout}, inert:poll(FD, read, 10)).

inert_poll_race_timeout() ->
    {ok, Socket} = gen_udp:open(0, []),
    {ok, FD} = inet:getfd(Socket),

    Result = [ begin
                inert:poll(FD, read_write, 0),
                receive
                    X -> X
                after
                    0 -> ok
                end
        end || _ <- lists:seq(1,1000) ],
    ?_assertEqual([], [ N || N <- Result, N /= ok ]).

% Test successive calls to fdset overwrite the previous mode
inert_stateless_fdset() ->
    {ok, Socket} = gen_tcp:listen(0, [binary, {active,false}]),
    {ok, Port} = inet:port(Socket),
    {ok, FD} = inet:getfd(Socket),

    ok = inert:fdset(FD, read_write),
    ok = inert:fdset(FD, write),

    {ok, Conn} = gen_tcp:connect("localhost", Port, [binary]),
    ok = gen_tcp:close(Conn),

    ok = receive
        {inert_read, _, FD} = Fail ->
            Fail
    after
        0 ->
            ok
    end,

    ok = inert:fdset(FD, read),

    Result = receive
        {inert_read, _, FD} = N ->
            N
    end,
    ?_assertMatch({inert_read, _, FD}, Result).

% Pass port ownership through a ring of processes
inert_controlling_process() ->
    PollId = prim_inert:start(),
    {ok, Socket} = gen_udp:open(0, [binary, {active,false}]),
    {ok, FD} = inet:getfd(Socket),
    Pid = self(),
    inert_controlling_process(PollId, Pid, FD, 0),
    Result = receive
        inert_controlling_process ->
            N = prim_inert:poll(PollId, FD, write),
            gen_udp:close(Socket),
            N
    end,
    ?_assertEqual({ok,write}, Result).

inert_controlling_process(PollId, Parent, _FD, 3) ->
    ok = prim_inert:controlling_process(PollId, Parent),
    Parent ! inert_controlling_process;
inert_controlling_process(PollId, Parent, FD, N) ->
    {ok,write} = prim_inert:poll(PollId, FD, write),
    Pid1 = spawn(fun() -> inert_controlling_process(PollId, Parent, FD, N+1) end),
    ok = prim_inert:controlling_process(PollId, Pid1).

% Catch the badarg if the port has been closed
inert_error_closed() ->
    PollId = prim_inert:start(),
    ok = prim_inert:stop(PollId),
    ?_assertEqual({error, closed}, prim_inert:poll(PollId, 1)).

inert_fdownership() ->
    {ok, Socket} = gen_udp:open(0, [binary, {active,false}]),
    {ok, FD} = inet:getfd(Socket),
    ok = inert:fdset(FD),
    Self = self(),

    % Test the fd is locked
    spawn(fun() -> Self ! {inert_test, inert:fdset(FD)} end),
    Reply1 = receive
        {inert_test, Error1} -> Error1
    after
        1000 -> {error,timeout}
    end,

    % Spawned process clears our lock
    spawn(fun() -> inert:fdclr(FD), Self ! {inert_test, inert:fdset(FD)} end),
    Reply2 = receive
        {inert_test, Error2} -> Error2
    after
        1000 -> {error,timeout}
    end,

    % Spawned process has exited, fd is unlocked
    Reply3 = inert:fdset(FD),

    % Spawn and process and remove the lock
    spawn(fun() -> Self ! {inert_test, poll}, inert:poll(FD) end),
    receive
        {inert_test,poll} -> ok
    end,
    ok = inert:fdclr(FD),
    spawn(fun() -> Self ! {inert_test, inert:fdset(FD)} end),
    Reply4 = receive
        {inert_test, Error4} -> Error4
    after
        1000 -> {error,timeout}
    end,

    [
        ?_assertEqual({error,ebusy}, Reply1),
        ?_assertEqual(ok, Reply2),
        ?_assertEqual(ok, Reply3),
        ?_assertEqual(ok, Reply4)
    ].

getenv(Var, Default) when is_list(Var), is_integer(Default) ->
    case os:getenv(Var) of
        false -> Default;
        N -> list_to_integer(N)
    end.
