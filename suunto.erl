-module(suunto).
-export([start/1, stop/0, init/2]).
-export([start/0, read_addr/1, get/1, loop/1, parse/1]).
-export([verify_check_sum/1]).

start() ->
    start("/dev/ttyUSB1").

start(Device) ->
    start("./suunto_port", Device).

start(ExtPrg, Device) ->
    spawn(?MODULE, init, [ExtPrg, Device]).

stop() ->
    suunto_port ! stop.

get(hist) ->
    {ok, <<72,13,18,
	   Year, Month, Day,
	   HighPoint:16/little,
	   Ascent:32/little,
	   Descent:32/little,
	   _Rest/binary>>} = read_addr(16#0d48, 16#12),
    [{date, Year, Month, Day},{highpoint, HighPoint},{ascent, Ascent}, {descent, Descent}];
get(hiking_logs) ->
    read_addr(16#0fb4, 16#14);
get(chrono_logs) ->
    read_addr(16#19c9, 16#19);
get(hiking_log1) ->
    {ok, Data} = read_addr(16#0fc8, 16#30),
    parse(Data).

parse(<<200, 15, 48, 
	_WhatIsthis:8,
	StartRaw:5/binary, %% 12,10,1,17,33
	Interval:8, %% 10
	HrData:8, %% 0
	TotalAscent:16/big, %% 0,110
	TotalDescent:16/big, %% 0,109
	_Gap:8, %% 0
	Laps:8, %% 1
	DurationHours:8,
	DurationMins:8,
	DurationSecs:8,
	DurationMS:8,
	_WhatIsThis:4/binary,
	HighestPointAlt:16/big,
	HighestTimeRaw:4/binary,
	LowestPointAlt:16/big,
	LowestTimeRaw:4/binary,
	_Rest/binary>>) ->
     [{start_time, parse_timestamp(StartRaw)},
      {interval, Interval},
      {hr_data, HrData},
      {total_ascent, TotalAscent},
      {total_descent, TotalDescent},
      {laps, Laps},
      {duration, {DurationHours, DurationMins, DurationSecs, DurationMS}},
      {highest_time, parse_timestamp(HighestTimeRaw)},
      {highest_point_altitude, HighestPointAlt},
      {lowest_time, parse_timestamp(LowestTimeRaw)},
      {lowest_point_altitude, LowestPointAlt}].

parse_timestamp(<<Year:8, RestDate:4/binary>>) ->
    [{year, Year} |  parse_timestamp(RestDate)];
parse_timestamp(<<Month:8, Day:8, Hour:8, Min:8>>) ->
    [{month, Month}, {day, Day}, {hour, Hour}, {min, Min}].

%% [5, 0, 3, AddrLoByte, AddrHiByte, Len, CheckSum (AddrLoByte^AddrHiByte^Len) ]
read_addr(Addr, Len) ->
    <<Hi:8, Lo:8>> = <<Addr:16>>,
    read_addr(<<5, 0, 3, Lo, Hi, Len, (Lo bxor Hi bxor Len)>>).

read_addr(X) ->
    io:format("writing command: ~p~n", [X]),
    verify_check_sum(call_port({write, X})).

verify_check_sum(<<_Pre:3/binary, Rest/binary>>) ->
    DataSize =  byte_size(Rest) - 1,
    <<Data:DataSize/binary, CheckSum:8>> = Rest,
    case calc_sum(Data, 0, CheckSum) of
	ok ->
	    {ok, Data};
	_ -> {error, checksum_not_correct}
    end.

calc_sum(<<H:8, T/binary>>, Acc, CheckSum) ->
    calc_sum(T, Acc bxor H, CheckSum);
calc_sum(<<>>, CheckSum, CheckSum) ->
    ok.

call_port({write, Msg}) ->
    suunto_port ! {call, self(), Msg},
    receive
	{response, Result} ->
	    io:format("got response: ~p~n", [Result]),
	    Result
    after 3000 ->
	    exit(port_hung)
    end.

init(ExtPrg, Device) ->
    register(suunto_port, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn_executable, ExtPrg}, [{packet, 2}, {args, [Device]}, binary]),
    suunto:loop(Port).


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
	{'EXIT', _Port, Reason} ->
	    io:format("port terminated, reason: ~p\n", [Reason]),
	    exit(port_terminated)
    end.

%% bin_to_hexstr(Bin) ->
%%    lists:flatten([io_lib:format("~2.16.0B ", [X]) ||
%%      X <- binary_to_list(Bin)]).
