%%% Copyright (c) 2013-2016, Michael Santos <michael.santos@gmail.com>
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
-module(inert_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
        all/0,
        init_per_suite/1,
        end_per_suite/1
    ]).

-export([
        inert_badfd/1,
        inert_select/1,
        inert_stream/1,
        inert_poll_read_write/1,
        inert_poll_timeout/1,
        inert_poll_race_timeout/1,
        inert_stateless_fdset/1,
        inert_controlling_process/1,
        inert_error_closed/1,
        inert_fdownership/1
    ]).

all() ->
    [
        inert_badfd,
        inert_select,
        inert_stream,
        inert_poll_read_write,
        inert_poll_timeout,
        inert_poll_race_timeout,
        inert_stateless_fdset,
        inert_controlling_process,
        inert_error_closed,
        inert_fdownership
    ].

init_per_suite(Config) ->
    inert:start(),
    [{inert_test_stream_runs,
            list_to_integer(os:getenv("INERT_TEST_STREAM_RUNS", "10"))},
        {inert_test_stream_num_bytes,
            list_to_integer(os:getenv("INERT_TEST_STREAM_NUM_BYTES", "1024"))}
        | Config].

end_per_suite(Config) ->
    inert:stop(),
    Config.

%%
%% inert_badfd
%%
inert_badfd(_Config) ->
    {error, ebadf} = inert:fdset(-1),
    {error, ebadf} = inert:poll(-1),
    {error, ebadf} = inert:fdset(127),
    {error, ebadf} = inert:fdset(128),
    {error, ebadf} = inert:fdset(10000).

%%
%% inert_select
%%
inert_select(_Config) ->
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

    ok = receive
        {inert_read, _, FD1} ->
            inert:fdclr(FD1),
            receive
                {inert_read, _, FD2} ->
                    inert:fdclr(FD2)
            end
    end.

%%
%% inert_stream
%%
inert_stream(Config) ->
    Runs = ?config(inert_test_stream_runs, Config),
    Bytes = ?config(inert_test_stream_num_bytes, Config),

    {ok, Socket} = gen_tcp:listen(0, [binary,{active,false}]),
    {ok, Port} = inet:port(Socket),
    spawn_link(fun() -> connect(Port, Runs, Bytes) end),
    accept(Socket, Runs, Bytes).

accept(Socket, Runs, Bytes) ->
    accept(Socket, Runs, Runs, Bytes).

accept(Socket, Runs, 0, _) ->
    wait(Socket, Runs);
accept(Socket, Runs, Count, Bytes) ->
    {ok, S1} = gen_tcp:accept(Socket),
    {ok, FD} = inet:getfd(S1),
    Self = self(),
    spawn_link(fun() -> read(Self, FD, Bytes) end),
    accept(Socket, Runs, Count-1, Bytes).

wait(Socket, 0) ->
    ok = gen_tcp:close(Socket);
wait(Socket, Runs) ->
    receive
        {fd_close, _FD} ->
            wait(Socket, Runs-1)
    end.

read(Pid, FD, Bytes) ->
    read(Pid, FD, Bytes, 0).
read(Pid, FD, Bytes, N) ->
    {ok,read} = inert:poll(FD),
    case procket:read(FD, 1) of
        {ok, <<>>} when Bytes =:= N ->
            procket:close(FD),
            Pid ! {fd_close, FD};
        {ok, Buf} ->
            read(Pid, FD, Bytes, N + byte_size(Buf));
        {error, eagain} ->
            error_logger:info_report([{fd, FD}, {error, eagain}]),
            read(Pid, FD, Bytes, N);
        {error, Error} ->
            error_logger:error_report([{fd, FD}, {error, Error}])
    end.

connect(Port, Runs, Bytes) ->
    {ok, Socket} = gen_tcp:connect("localhost", Port, []),
    Bin = crypto:strong_rand_bytes(Bytes),
    ok = gen_tcp:send(Socket, Bin),
    ok = gen_tcp:close(Socket),
    connect(Port, Runs-1, Bytes).

%%
%% inert_poll_read_write
%%
inert_poll_read_write(_Config) ->
    {ok, Socket} = gen_udp:open(0, [{active,false}]),
    {ok, FD} = inet:getfd(Socket),
    {ok,_} = inert:poll(FD, read_write),
    PollId = inert:pollid(),
    ok = receive
        {Tag, PollId, FD} when Tag == inert_read; Tag == inert_write ->
            {error, Tag}
    after
        10 ->
            gen_udp:close(Socket)
    end.

%%
%% inert_poll_timeout
%%
inert_poll_timeout(_Config) ->
    {ok, Socket} = gen_tcp:listen(0, [binary, {active,false}]),
    {ok, FD} = inet:getfd(Socket),
    {error, timeout} = inert:poll(FD, read, 10).

%%
%% inert_poll_race_timeout
%%
inert_poll_race_timeout(_Config) ->
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
    [] = [ N || N <- Result, N /= ok ].

%%
%% inert_poll_race_timeout
%%
%% Test successive calls to fdset overwrite the previous mode
%%
inert_stateless_fdset(_Config) ->
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
    {inert_read, _, FD} = Result.

%%
%% inert_controlling_process
%%
%% Pass port ownership through a ring of processes
%%
inert_controlling_process(_Config) ->
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
    {ok,write} = Result.

inert_controlling_process(PollId, Parent, _FD, 3) ->
    ok = prim_inert:controlling_process(PollId, Parent),
    Parent ! inert_controlling_process;
inert_controlling_process(PollId, Parent, FD, N) ->
    {ok,write} = prim_inert:poll(PollId, FD, write),
    Pid1 = spawn(fun() -> inert_controlling_process(PollId, Parent, FD, N+1) end),
    ok = prim_inert:controlling_process(PollId, Pid1).

%%
%% inert_error_closed
%%
%% Catch the badarg if the port has been closed
inert_error_closed(_Config) ->
    PollId = prim_inert:start(),
    ok = prim_inert:stop(PollId),
    {error, closed} = prim_inert:poll(PollId, 1).

%%
%% inert_fdownership
%%
inert_fdownership(_Config) ->
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

    {error,ebusy} = Reply1,
    ok = Reply2,
    ok = Reply3,
    ok = Reply4.
