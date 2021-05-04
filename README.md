inert is a library for asynchronous notification of events on file
descriptors. To be scheduler friendly, inert uses the native Erlang
socket polling mechanism.

# QUICK USAGE

    STDOUT = 1,
    ok = inert:start(),
    {ok,read} = inert:poll(STDOUT).

# BUILD

    $ rebar3 compile

# OVERVIEW

inert sends your process a message whenever an event occurs on a
non-blocking file descriptor.  You'll need another library to open the
fd's (see _ADDITIONAL LIBRARIES_). For example, using inet:

    {ok, Socket} = gen_tcp:listen(1234, [binary, {active,false}]),
    {ok, FD} = inet:getfd(Socket).

Both inet and inert use erts for polling, so stealing fd's may generate
error reports.

# ADDITIONAL LIBRARIES

* network sockets

    https://github.com/msantos/procket

* serial devices

    https://github.com/msantos/srly

* TUN/TAP devices

    https://github.com/msantos/tunctl

# EXPORTS

## inert

    start() -> ok

        Start the inert service.

    stop() -> ok

        Stop the inert service.

    poll(FD) -> {ok,read} | {error, Error}
    poll(FD, Mode) -> {ok, Mode} | {error, Error}
    poll(FD, Mode, Timeout) -> {ok, Mode} | {error, Error}

        Types   FD = integer()
                Mode = read | write | read_write
                Timeout = timeout()
                Error = closed | timeout | ebadf | einval

        poll/2,3 blocks until a file descriptor is ready for reading
        or writing (default mode: read).

        poll will block forever unless the timeout option is used.
        With the timeout option, poll will be interrupted after the
        specified timeout (in milliseconds) and return the tuple {error,
        timeout}.

    pollid() -> PollId

        Types   PollId = port()

        Retrieves the port identifier for the inert driver.

    fdset(FD) -> ok | {error, Error}
    fdset(FD, Mode) -> ok | {error, Error}

        Types   FD = integer()
                Mode = read | write | read_write
                Error = closed | ebadf | einval

        Monitor a file descriptor for events (default mode: read).

        fdset/1,2 will send one message when a file descriptor is ready
        for reading or writing:

            {inert_read, PollId, FD}   % fd is ready for reading
            {inert_write, PollId, FD}  % fd is ready for writing

        Use pollid/0 to retrieve the descriptor for the inert driver.

        When requesting a monitoring mode of read_write, the calling
        process may receive two messages (one for read, one for write).

        Further events are not monitored after the message is sent. To
        re-enable monitoring, fdset/2,3 must be called again.

        Successive calls to fdset/2,3 reset the mode:

            fdset(FD, [{mode, read_write}]),
            fdset(FD, [{mode, write}]).
            % monitoring the fd for write events only

    fdclr(FD) -> ok | {error, Error}
    fdclr(FD, Mode) -> ok | {error, Error}

        Types   FD = integer()
                Mode = read | write | read_write
                Error = closed | ebadf | einval

        Clear an event set for a file descriptor (default: read_write).

## prim\_inert

    controlling_process(Port, PID) -> ok | {error, Error}

        Types   Port = port()
                PID = pid()
                Error = not_owner | einval

        Transfer ownership of the inert port driver from the current
        process to another process.

        Since any process can use the port, controlling_process/2 just
        sets the port owner and links the process to the port. The
        original owner will continue to receive messages for any file
        descriptors it has added to the pollset.

## inert\_drv

inert\_drv is a wrapper around the driver\_select() function in the
erl\_driver API. See:

http://www.erlang.org/doc/man/erl_driver.html#driver_select

# EXAMPLES

Run:

    erlc -I deps -o ebin examples/*.erl

## echo server

See `examples/echo.erl`. To run it:

    erl -pa ebin
    1> echo:listen(1234).

## Connecting to a port

This (slightly terrifying) example uses the BSD socket interface in
procket to connect to SSH on localhost and read the version header. It
is the equivalent of:

    {ok, Socket} = gen_tcp:connect("localhost", 22, [binary, {active,false}]),
    gen_tcp:recv(Socket, 0).

``` erlang
-module(conn).
-include_lib("procket/include/procket.hrl").

-export([ssh/0]).

-ifndef(SO_ERROR).
-define(SO_ERROR, 4).
-endif.

ssh() ->
    ok = inert:start(),
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
            poll(Socket)
    end,
    {ok,read} = inert:poll(Socket, read),
    procket:read(Socket, 16#ffff).

poll(Socket) ->
    {ok,write} = inert:poll(Socket, write),
    case procket:getsockopt(Socket, ?SOL_SOCKET, ?SO_ERROR, <<>>) of
        {ok, _Buf} ->
            ok;
        {error, _} = Error ->
            Error
    end.
```

# ALTERNATIVES

So why would you use `inert` instead of `inet`?

* You have to monitor socket events without using inet. For example,
  to use socket interfaces like sendmsg(2) and recvmsg(2).

* You want to experiment with alternatives to inet. It'd be interesting
  to experiment with moving some of the network code from C to Erlang.

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
    Port = erlang:open_port({fd, FD, FD}, [stream,binary]).

Ports do not have a built in way to do flow control (inet is a port but
the flow control is done within the driver). Flow control can be done
by closing the port and re-opening it after the data in the mailbox has
been processed:

    % synchronous close of port, flushes buffers
    erlang:port_close(Port),
    % process data
    Port1 = erlang:open_port({fd, FD, FD}, [stream,binary]).

    % async close
    Port1 ! {self(), close}
    % process data
    Port2 = erlang:open_port({fd, FD, FD}, [stream,binary]).

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

# Behaviour of driver\_select()

These are some notes describing the empirical behaviour of
driver\_select().

The function signature for driver\_select() is:

    int driver_select(ErlDrvPort port, ErlDrvEvent event, int mode, int on)

* the `return value` is either 0 or -1. However, it will only return
  error in the case the driver has not defined the ready\_input callback
  (for the ERL\_DRV\_READ mode) or the ready\_output callback (for the
  ERL\_DRV\_WRITE mode)

* on Unix platforms, `event` is simply a file descriptor which is
  represented as an int but the size of the `ErlDrvEvent` type varies:

    * int (32/64-bit): 4 bytes
    * ErlDrvEvent (32-bit VM): 4 bytes
    * ErlDrvEvent (64-bit VM): 8 bytes

  The ErlDrvEvent type needs to be cast or included in a union:

        typedef union {
            ErlDrvEvent ev;
            int32_t fd;
        } inert_fd_t;

* `mode` sets which events the file descriptor will be monitored:

    * ERL\_DRV\_READ: call the ready\_input callback when the file
      descriptor is ready for reading

    * ERL\_DRV\_WRITE: call the ready\_output callback when the file
      descriptor is ready for writing

    * ERL\_DRV\_USE: call the stop\_select callback when it is safe for
      the file descriptor to be closed

  The modes in successive calls to driver\_select() are OR'ed with the
  previous value.

        driver_select(port, event, ERL_DRV_READ, 1)
        // mode = read
        driver_select(port, event, ERL_DRV_WRITE, 1)
        // mode = read, write

* the `on` argument is used to either set (1) or clear (0) modes from
  an event

The only modes defined for old drivers was monitoring for read and write
events. An additional mode (ERL\_DRV\_USE) was introduced to allow the
driver to indicate to the VM when it is safe to close events, necessary on
SMP VMs where one thread may close an event that another thread is using.

To support old drivers, the ERL\_DRV\_USE mode is not required. Any use
of the ERL\_DRV\_USE mode (either setting or clearing) results in the
VM issuing a warning if the driver does not support the stop\_select
callback.

The interaction between `mode` and `on`:

* mode:ERL\_DRV\_READ, ERL\_DRV\_WRITE ERL\_DRV\_USE, on:1

  The mode is OR'ed with the existing mode for the event. If ERL\_DRV\_USE
  was not previously set, the VM will now check for the existence of a
  stop\_select callback and issue a warning if it does not exist.

* mode:ERL\_DRV\_USE, on:1

  Setting the mode for the event to only `use` indicates to the VM that
  the event will be re-used. Presumably the VM will not de-allocate
  resources for the event.

* mode:ERL\_DRV\_READ, ERL\_DRV\_WRITE, on:0

  The mode is NOT'ed from the existing mode for the event.

* mode:ERL\_DRV\_USE, on:0

  Indicate to the VM that resources associated with the event can be
  scheduled for de-allocation. When all uses of the event have completed,
  the VM will call the driver's stop\_schedule callback.

# TODO

* pass in sets of file descriptors
