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
-behaviour(gen_server).

%% API
-export([start/0, start/1, stop/1]).
-export([start_link/1]).
-export([
        fdset/2, fdset/3,
        fdclr/2, fdclr/3,
        poll/2, poll/3
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        port,
        fds = dict:new()
    }).

start() ->
    start_link([]).
start(Options) when is_list(Options) ->
    start_link(Options).

start_link(Options) ->
    gen_server:start_link(?MODULE, [Options], []).

stop(Ref) ->
    gen_server:call(Ref, stop).

fdset(Ref, FD) ->
    fdset(Ref, FD, []).
fdset(Ref, FD, Options) ->
    gen_server:call(Ref, {fdset, FD, Options}).

fdclr(Ref, FD) ->
    fdclr(Ref, FD, []).
fdclr(Ref, FD, Options) ->
    gen_server:call(Ref, {fdclr, FD, Options}).

poll(Ref, FD) ->
    poll(Ref, FD, []).

poll(Ref, FD, Options) ->
    Timeout = proplists:get_value(timeout, Options, infinity),
    case fdset(Ref, FD) of
        ok ->
            poll_1(Ref, FD, Timeout);
        Error ->
            Error
    end.

poll_1(Ref, FD, Timeout) when is_atom(Ref) ->
    poll_1(whereis(Ref), FD, Timeout);
poll_1(Ref, FD, Timeout) when is_pid(Ref) ->
    receive
        {inert, Ref, FD} ->
            ok;
        {inert_error, Ref, Error} ->
            {error, Error}
    after
        Timeout ->
            inert:fdclr(Ref, FD),
            {error, eintr}
    end.


%%--------------------------------------------------------------------
%%% gen_server callbacks
%%--------------------------------------------------------------------
init([_Options]) ->
    process_flag(trap_exit, true),
    ok = inert_drv:start(),
    Port = open_port({spawn_driver, inert_drv}, [stream]),
    {ok, #state{
            port = Port
        }}.

handle_call({fdset, FD, _Options}, {Pid,_}, #state{port = Port, fds = FDs} = State) ->
    Event = inert_drv:encode(FD),
    Reply = inert_drv:send(Port, fdset, Event),
    FDs1 = case Reply of
        ok ->
            dict:store(FD, Pid, FDs);
        _Error ->
            FDs
    end,
    {reply, Reply, State#state{fds = FDs1}};

handle_call({fdclr, FD, _Options}, _From, #state{port = Port, fds = FDs} = State) ->
    Event = inert_drv:encode(FD),
    Reply = inert_drv:send(Port, fdclr, Event),
    FDs1 = dict:erase(FD, FDs),
    {reply, Reply, State#state{fds = FDs1}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port}) ->
    catch erlang:port_close(Port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Port communication
%%--------------------------------------------------------------------
handle_info({Port, {data, Data}}, #state{port = Port, fds = FDs} = State) ->
    Event = inert_drv:decode(Data),
    case dict:find(Event, FDs) of
        {ok, Caller} ->
            Caller ! {inert, self(), Event};
        error ->
            error_logger:error_report([
                    {error, "event not found"},
                    {event, Event},
                    {fds, dict:to_list(FDs)}
                ])
    end,
    {noreply, State};

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {shutdown, Reason}, State};

% WTF
handle_info(Info, State) ->
    error_logger:error_report([{wtf, Info}]),
    {noreply, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
