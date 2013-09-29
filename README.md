inert is a library for asynchronous socket notifications. To be scheduler
friendly, inert uses the native Erlang socket polling mechanism.

_WARNING:_ this library is under development

# QUICK USAGE

    STDIN = 1,
    {ok, Ref} = inert:start(),
    ok = inert:poll(Ref, STDIN).

# OVERVIEW

inert sends a message whenever an event occurs on a non-blocking file
descriptor.  You'll need another library to open the fd's (see _ADDITIONAL
LIBRARIES_). For example, using inet:

    {ok, Socket} = gen_tcp:listen(1234, [binary, {active,false}]),
    {ok, FD} = inet:getfd(Socket).

Be careful when using file descriptors opened by inet. Both inet and
inert use the same mechanism for polling, so stealing fd's may result
in unexpected behaviour like generating storms of error messages or
crashing the emulator.

# ADDITIONAL LIBRARIES

* network sockets

    https://github.com/msantos/procket

* serial devices

    https://github.com/msantos/srly

# EXPORTS

## inert

    start() -> {ok, Ref}

        Types   Ref = pid()

        Start the inert service.

    poll(Ref, FD) -> ok | {error, posix()}
    poll(Ref, FD, Options) -> ok | timeout | {error, posix()}

        Types   Ref = pid()
                FD = int32()
                Options = [ {timeout, Timeout} | {mode, Mode} ]
                Timeout = infinity | uint()
                Mode = read | write | read_write

        poll/2,3 blocks until a file descriptor is ready for reading
        or writing (default: read).

        poll will block forever unless the timeout option is used.
        With the timeout option, poll will be interrupted after the
        specified timeout (in milliseconds) and return the atom 'timeout'.

    fdset(Ref, FD) -> ok | {error, posix()}
    fdset(Ref, FD, Options) -> ok | {error, posix()}

        Types   Ref = pid()
                FD = int32()
                Options = [ {mode, Mode} ]
                Mode = read | write | read_write

        Monitor a file descriptor for events. The default monitoring
        mode is {mode,read}.

        fdset/2,3 will send one message for each read or write event:

            {inert_read, Ref, FD}   % fd is ready for reading
            {inert_write, Ref, FD}  % fd is ready for writing

        When requesting a monitoring mode of read_write, the calling
        process may receive two messages (one for read, one for write).

        Successive calls to fdset/2,3 reset the mode:

            fdset(Ref, FD, [{mode, read_write}]),
            fdset(Ref, FD, [{mode, write}]).
            % monitoring the fd for write events only
        
    fdclr(Ref, FD) -> ok | {error, posix()}
    fdclr(Ref, FD, Options) -> ok | {error, posix()}

        Types   Ref = pid()
                FD = int32()
                Options = [ {mode, Mode} ]
                Mode = read | write | read_write

        Clear an event set for a file descriptor.

## inert\_drv

inert\_drv is a wrapper around driver\_select() found in erl\_driver. See:

    http://www.erlang.org/doc/man/erl_driver.html#driver_select

# EXAMPLES

Run:

    make eg

## echo server

See `examples/echo.erl`. To run it:

    erl -pa ebin
    1> echo:listen(1234).

## Connecting to a port

This (slightly terrifying) example uses procket and the BSD socket
interface to connect to SSH on localhost and read the version header. It
is the equivalent of:

    {ok, Socket} = gen_tcp:connect("localhost", 22, [binary, {active,false}]),
    gen_tcp:recv(Socket, 0).

``` erlang
-module(conn).
-include_lib("procket/include/procket.hrl").

-export([ssh/0]).

-define(SO_ERROR, 4).

ssh() ->
    {ok, Ref} = inert:start(),
    {ok, Socket} = procket:socket(inet, stream, 0),
    Sockaddr = <<(procket:sockaddr_common(?PF_INET, 16))/binary,
            22:16,          % Port
            127,0,0,1,      % IPv4 loopback
            0:64
        >>,
    ok = case procket:connect(Socket, Sockaddr) of
        ok ->
            ok;
        {error, einprogress} ->
            poll(Ref, Socket)
    end,
    ok = inert:poll(Ref, Socket, [{mode,read}]),
    procket:read(Socket, 16#ffff).

poll(Ref, Socket) ->
    ok = inert:poll(Ref, Socket, [{mode,write}]),
    case procket:getsockopt(Socket, ?SOL_SOCKET, ?SO_ERROR, <<>>) of
        {ok, _Buf} ->
            ok;
        {error, _} = Error ->
            Error
    end.
```

# ALTERNATIVES

So why would you use `inert` instead of `inet`?

1. You want to monitor socket events without using inet. For example,
   to use socket interfaces like sendmsg(2) and recvmsg(2).

2. You want to experiment with alternatives to inet.

Otherwise, there are a few builtin methods for polling file descriptors
in Erlang. All of these methods will read/write from the socket on
your behalf. It is your responsibility to close the socket.

## gen\_udp:open/2

Works with inet and inet6 sockets and supports the flow control mechanisms
in inet ({active, true}, {active, once}, {active, false}).

    FD = 7,
    {ok, Socket} = gen_udp:open(0, [binary, {fd, FD}, inet]).

`inet` is a big, complicated driver. It expects to be receiving TCP or
UDP data. If you pass in other types of packets, you may run into some
weird behaviour. For example, see:

https://github.com/erlang/otp/commit/169080db01101a4db6b1c265d04d972f3c39488a#diff-a2cead50e09b9f8f4a7f0d8d5ce986f7

## erlang:open\_port/2

Works with any type of non-blocking file descriptor:

    FD = 7,
    {ok, Ref} = erlang:open_port({fd, FD, FD}, [stream,binary]).

## Busy waiting

And of course, the simple, dumb way is to spin on the file descriptor:

    spin(FD) ->
        case procket:read(FD, 16#ffff) of
            {ok, <<>>} ->
                ok = procket:close(FD),
                ok;
            {ok, Buf} ->
                {ok, Buf};
            {error, eagain} ->
                timer:sleep(10),
                spin(FD);
            {error, Error} ->
                {error, Error}
        end.

# TODO

* this will cause a segfault when inet goes to close the fd

        1> {ok, Ref} = inert:start().
        {ok,<0.35.0>}
        2> {ok, Socket} = gen_tcp:listen(7171, [binary, {active,false}]).
        {ok,#Port<0.955>}
        3> inet:getfd(Socket).
        {ok,7}
        4> inert:fdset(Ref, 7).
        ok
        5> halt().
        Segmentation fault (core dumped)

* pass in sets of file descriptors

        inert:fdset([7, {8, write}, {11, read_write}])
