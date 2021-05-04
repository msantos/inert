%%% @copyright 2015-2021, Michael Santos <michael.santos@gmail.com>

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
-module(prim_inert).

%% API
-export([start/0, stop/1]).
-export([
    fdset/2, fdset/3,
    fdclr/2, fdclr/3,
    poll/2, poll/3, poll/4,

    controlling_process/2
]).

start() ->
    ok = inert_drv:start(),
    open_port({spawn_driver, "inert_drv"}, [stream]).

-spec stop(inert_drv:ref()) -> 'ok'.
stop(Port) ->
    catch erlang:port_close(Port),
    inert_drv:stop().

-spec fdset(inert_drv:ref(), integer()) -> 'ok' | inert_drv:errno().
fdset(Port, FD) ->
    fdset(Port, FD, []).

-spec fdset(inert_drv:ref(), integer(), inert_drv:mode() | proplists:proplist()) ->
    'ok' | inert_drv:errno().
fdset(Port, FD, Options) when is_list(Options) ->
    Mode = proplists:get_value(mode, Options, read),
    fdset(Port, FD, Mode);
fdset(Port, FD, Mode) when is_atom(Mode) ->
    Event = inert_drv:encode({FD, Mode}),
    inert_drv:ctl(Port, fdset, Event).

-spec fdclr(inert_drv:ref(), integer()) -> 'ok' | inert_drv:errno().
fdclr(Port, FD) ->
    fdclr(Port, FD, []).

-spec fdclr(inert_drv:ref(), integer(), inert_drv:mode() | proplists:proplist()) ->
    'ok' | inert_drv:errno().
fdclr(Port, FD, Options) when is_list(Options) ->
    Mode = proplists:get_value(mode, Options, read_write),
    fdclr(Port, FD, Mode);
fdclr(Port, FD, Mode) when is_atom(Mode) ->
    Event = inert_drv:encode({FD, Mode}),
    inert_drv:ctl(Port, fdclr, Event).

-spec poll(inert_drv:ref(), integer()) ->
    {'ok', 'read'} | {'error', 'timeout'} | inert_drv:errno().
poll(Port, FD) ->
    poll(Port, FD, []).

-spec poll(inert_drv:ref(), integer(), inert_drv:mode() | proplists:proplist()) ->
    {'ok', 'read' | 'write'} | {'error', 'timeout'} | inert_drv:errno().
poll(Port, FD, Options) when is_list(Options) ->
    Mode = proplists:get_value(mode, Options, read),
    Timeout = proplists:get_value(timeout, Options, infinity),
    poll(Port, FD, Mode, Timeout);
poll(Port, FD, Mode) when is_atom(Mode) ->
    poll(Port, FD, Mode, infinity).

-spec poll(inert_drv:ref(), integer(), inert_drv:mode(), timeout()) ->
    {'ok', 'read' | 'write'} | {'error', 'timeout'} | inert_drv:errno().
poll(Port, FD, Mode, Timeout) ->
    case wait(Port, FD, Mode, 0) of
        {error, timeout} ->
            polldrv(Port, FD, Mode, Timeout);
        Reply ->
            Reply
    end.

polldrv(Port, FD, Mode, Timeout) ->
    case fdset(Port, FD, Mode) of
        ok ->
            wait(Port, FD, Mode, Timeout);
        Error ->
            Error
    end.

%% @doc Transfer ownership of the inert port driver from the current
%% process to another process.
%%
%% Since any process can use the port, controlling_process/2 just
%% sets the port owner and links the process to the port. The
%% original owner will continue to receive messages for any file
%% descriptors it has added to the pollset.
-spec controlling_process(inert_drv:ref(), pid()) ->
    'ok' | {'error', 'not_owner' | 'einval'}.
controlling_process(Port, Pid) when is_atom(Port), is_pid(Pid) ->
    controlling_process(whereis(Port), Pid);
controlling_process(Port, Pid) when is_port(Port), is_pid(Pid) ->
    Owner = self(),
    case erlang:port_info(Port, connected) of
        {connected, Pid} ->
            ok;
        {connected, Owner} ->
            erlang:port_connect(Port, Pid),
            unlink(Port),
            receive
                {'EXIT', Port, _} ->
                    ok
            after 0 ->
                ok
            end;
        {connected, _} ->
            {error, not_owner};
        _ ->
            {error, einval}
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec wait(inert_drv:ref(), integer(), read | write | read_write, timeout()) ->
    {'ok', 'read' | 'write'} | {'error', 'timeout'}.
wait(Port, FD, Mode, Timeout) when is_atom(Port) ->
    wait(whereis(Port), FD, Mode, Timeout);
wait(Port, FD, read_write, Timeout) ->
    receive
        {inert_read, Port, FD} ->
            {ok, read};
        {inert_write, Port, FD} ->
            {ok, write}
    after Timeout ->
        {error, timeout}
    end;
wait(Port, FD, Mode, Timeout) ->
    Tag =
        case Mode of
            read -> inert_read;
            write -> inert_write
        end,
    receive
        {Tag, Port, FD} ->
            {ok, Mode}
    after Timeout ->
        {error, timeout}
    end.
