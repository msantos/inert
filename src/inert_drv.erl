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
-module(inert_drv).
-export([
        start/0,
        stop/0
    ]).
-export([
        set/1, clr/1
    ]).

-define(INERT_FDSET, 1).
-define(INERT_FDCLR, 2).

start() ->
    case erl_ddll:load_driver(priv_dir(), ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> exit({error, erl_ddll:format_error(Error)})
    end,

    case whereis(inert) of
        undefined ->
            spawn(fun() -> init() end);
        Pid ->
            Pid
    end.

init() ->
    register(inert, self()),
    Port = open_port({spawn_driver, ?MODULE}, [stream]),
    loop(Port).

stop() ->
    inert ! stop.

set(X) ->
    call_port({set, X}).
clr(Y) ->
    call_port({clr, Y}).

call_port(Msg) ->
    inert ! {call, self(), Msg},
    receive
        ok ->
            ok;
        {inert_error, Error} ->
            {error, list_to_atom(Error)}
    end.

loop(Port) ->
    loop_1(Port, dict:new()).

loop_1(Port, Map) ->
    receive
        {call, Caller, {_, FD} = Msg} ->
            {Op, Data} = encode(Msg),
            Response = erlang:port_control(Port, Op, Data), 
            Map1 = case Response of
                [] ->
                    Caller ! ok,
                    dict:store(FD, Caller, Map);
                Error ->
                    Caller ! {inert_error, Error},
                    Map
            end,
            loop_1(Port, Map1);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            io:format("~p ~n", [Reason]),
            exit(port_terminated);
        {inert, Port, Event} ->
            case dict:find(Event, Map) of
                {ok, Caller} ->
                    Caller ! {inert, Port, Event};
                error ->
                    ok
            end,
            loop_1(Port, Map);
        {Port, {data, Event0}} ->
            Event = decode(Event0),

            case dict:find(Event, Map) of
                {ok, Caller} ->
                    Caller ! {inert, Port, Event};
                error ->
                    error_logger:info_report([
                            {event, Event},
                            {error, event_not_found},
                            {dict, dict:to_list(Map)}
                        ]),
                    ok
            end,
            loop_1(Port, Map);
        Any ->
            error_logger:info_report([{any, Any}]),
            loop_1(Port, Map)
    end.

encode({set, X}) -> {?INERT_FDSET, [<<X:4/big-signed-integer-unit:8>>]};
encode({clr, X}) -> {?INERT_FDCLR, [<<X:4/big-signed-integer-unit:8>>]}.

decode([A,B,C,D]) -> (A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D;
decode(<<N:4/big-signed-integer-unit:8>>) -> N.

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]);
        Dir ->
            Dir
    end.
