%%% @copyright 2013-2021, Michael Santos <michael.santos@gmail.com>

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
    poll/1, poll/2, poll/3,
    pollid/0
]).

%% @doc Start the inert service.
start() ->
    Port = prim_inert:start(),
    try
        register(inert, Port)
    catch
        error:badarg -> ok
    end,
    unlink(Port),
    ok.

%% @doc Stop the inert service.
stop() ->
    prim_inert:stop(inert).

%% @doc Retrieves the port identifier for the inert driver.
-spec pollid() -> 'undefined' | port().
pollid() ->
    whereis(inert).

%% @doc Monitor a file descriptor for read events.
-spec fdset(integer()) -> 'ok' | inert_drv:errno().
fdset(FD) ->
    prim_inert:fdset(inert, FD, read).

%% @doc Monitor a file descriptor for events.
%%
%% fdset/1,2 will send one message when a file descriptor is ready
%% for reading or writing:
%%
%%     {inert_read, PollId, FD}   % fd is ready for reading
%%     {inert_write, PollId, FD}  % fd is ready for writing
%%
%% Use pollid/0 to retrieve the descriptor for the inert driver.
%%
%% When requesting a monitoring mode of read_write, the calling
%% process may receive two messages (one for read, one for write).
%%
%% Further events are not monitored after the message is sent. To
%% re-enable monitoring, fdset/2,3 must be called again.
%%
%% Successive calls to fdset/2,3 reset the mode:
-spec fdset(integer(), inert_drv:mode() | proplists:proplist()) ->
    'ok' | inert_drv:errno().
fdset(FD, Options) ->
    prim_inert:fdset(inert, FD, Options).

%% @doc Clear read_write events for a file descriptor.
-spec fdclr(integer()) -> 'ok' | inert_drv:errno().
fdclr(FD) ->
    prim_inert:fdclr(inert, FD, read_write).

%% @doc Clear an event set for a file descriptor.
-spec fdclr(integer(), inert_drv:mode() | proplists:proplist()) ->
    'ok' | inert_drv:errno().
fdclr(FD, Options) ->
    prim_inert:fdclr(inert, FD, Options).

%% @doc Block until a file descriptor is ready for reading.
-spec poll(integer()) ->
    {'ok', 'read'} | {'error', 'timeout'} | inert_drv:errno().
poll(FD) ->
    prim_inert:poll(inert, FD, read, infinity).

%% @doc Block until a file descriptor is ready for reading
%% or writing.
-spec poll(integer(), inert_drv:mode() | proplists:proplist()) ->
    {'ok', 'read' | 'write'} | {'error', 'timeout'} | inert_drv:errno().
poll(FD, Mode) when is_atom(Mode) ->
    prim_inert:poll(inert, FD, Mode, infinity);
poll(FD, Options) when is_list(Options) ->
    prim_inert:poll(inert, FD, Options).

%% @doc Block until a file descriptor is ready for reading
%% or writing.
%%
%% The timeout option will interrupt poll/3 after the specified timeout
%% (in milliseconds), returning the tuple {error, timeout}.
-spec poll(integer(), inert_drv:mode() | proplists:proplist(), timeout()) ->
    {'ok', 'read' | 'write'} | {'error', 'timeout'} | inert_drv:errno().
poll(FD, Mode, Timeout) ->
    prim_inert:poll(inert, FD, Mode, Timeout).
