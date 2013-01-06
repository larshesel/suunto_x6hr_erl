-module(suunto).
-export([start/1, stop/0, init/1]).
-export([start/0, write/1, write/3, get/1]).

start() ->
    start("./suunto_port").

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).

stop() ->
    complex ! stop.

get(hist) ->
    <<5, 0,21,72,13,18,
      Year, Month, Day,
      HighPoint:16/little,
      Ascent:32/little,
      Descent:32/little,
      _Rest/binary>> = write(16#48, 16#0d, 16#12),
    [{date, Year, Month, Day},{highpoint, HighPoint},{ascent, Ascent}, {descent, Descent}];
get(hiking_logs) ->
    write(16#b4, 16#0f, 16#14).

%% [5, 0, 3, AddrLoByte, AddrHiByte, Len, CheckSum (AddrLoByte^AddrHiByte^Len) ]
write(AddrLo, AddrHi, Len) ->
    write(<<5, 0, 3, AddrLo, AddrHi, Len, (AddrLo bxor AddrHi bxor Len)>>).

write(X) ->
    io:format("writing command: ~p~n", [X]),
    call_port({write, X}).


call_port({write, Msg}) ->
    complex ! {call, self(), Msg},
    receive
	{response, Result} ->
	    io:format("got response: ~p~n", [Result]),
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
	    io:format("port terminated\n", []),
	    exit(port_terminated)
    end.
