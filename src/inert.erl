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

%% API
-export([start/0, start/1, stop/1]).
-export([
        fdset/2, fdset/3,
        fdclr/2, fdclr/3,
        poll/2, poll/3,

        controlling_process/2
    ]).

start() ->
    start([]).
start(Options) when is_list(Options) ->
    case inert_drv:start() of
        ok ->
            Port = open_port({spawn_driver, "inert_drv"}, [stream]),
            {ok, Port};
        Error ->
            {error, Error}
    end.

stop(Port) ->
    catch erlang:port_close(Port),
    inert_drv:stop().

fdset(Port, FD) ->
    fdset(Port, FD, []).
fdset(Port, FD, Options) ->
    Mode = proplists:get_value(mode, Options, read),
    Event = inert_drv:encode({FD, Mode}),
    inert_drv:ctl(Port, fdset, Event).

fdclr(Port, FD) ->
    fdclr(Port, FD, []).
fdclr(Port, FD, Options) ->
    Mode = proplists:get_value(mode, Options, read_write),
    Event = inert_drv:encode({FD, Mode}),
    inert_drv:ctl(Port, fdclr, Event).

poll(Port, FD) ->
    poll(Port, FD, []).

poll(Port, FD, Options) ->
    case fdset(Port, FD, Options) of
        ok ->
            poll_1(Port, FD, Options);
        Error ->
            Error
    end.

poll_1(Port, FD, Options) when is_port(Port) ->
    Mode = proplists:get_value(mode, Options, read),
    Timeout = proplists:get_value(timeout, Options, infinity),
    receive
        {inert_read, Port, FD} ->
            ok;
        {inert_write, Port, FD} ->
            ok
    after
        Timeout ->
            inert:fdclr(Port, FD, [{mode, Mode}]),
            {error, timeout}
    end.

controlling_process(Port, Pid) when is_port(Port), is_pid(Pid) ->
    Owner = self(),
    case erlang:port_info(Port, connected) of
        {connected, Pid} ->
            ok;
        {connected, Owner} ->
            try erlang:port_connect(Port, Pid) of
                true ->
                    unlink(Port),
                    receive
                        {'EXIT', Port, _} ->
                            ok
                    after
                        0 ->
                            ok
                    end
            catch
                error:Error ->
                    {error, Error}
            end;
        {connected, _} ->
            {error, not_owner};
        _ ->
            {error, einval}
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
