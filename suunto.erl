-module(suunto).
-export([start/1, stop/0, init/1]).
-export([write/1]).

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).

stop() ->
    complex ! stop.

write(X) ->
    call_port({write, X}).


call_port({write, Msg}) ->
    complex ! {call, self(), Msg},
    io:format("call port~n", []),
    receive
	{response, Result} ->
	    Result
    end.

init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, Msg}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {response, Data}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', _Port, _Reason} ->
	    exit(port_terminated)
    end.
