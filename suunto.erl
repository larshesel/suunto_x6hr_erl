-module(suunto).
-export([start/1, stop/0, init/2]).
-export([start/0, loop/1]).
-export([get/1, get/2, code_switch/0]).

-define(HIST_ADDR,              16#0d48).
-define(HIKING_LOGS_ADDR,       16#0fb4).
-define(CHRONO_LOGS_ADDR,       16#19c9).
-define(CHRONO_LOG_BASE_ADDR,   16#19fa).
-define(CHRONO_LOG_ENTRY_OFFSET,  16#32).
-define(HIKING_LOG_BASE_ADDR,   16#0fc8).
-define(HIKING_LOG_ENTRY_OFFSET,  16#80).
-define(HIKING_LOG_ADDR,        16#0fb4).

get(hiking_log, No) when (0 =< No) and (No < 20) ->
    {ok, LogIdxs} = suunto:get(hiking_logs),
    case binary:at(LogIdxs, No) of 
	0 -> 
	    no_log;
	LogIdx -> 
	    {ok, Data} = read_addr(?HIKING_LOG_BASE_ADDR + LogIdx * ?HIKING_LOG_ENTRY_OFFSET, 16#30),
	    {ok, parse(hiking_log, Data)}
    end;
get(chrono_log, No) when (0 =< No) and (No < 20) ->
    {ok, LogIdxs} = suunto:get(chrono_logs),
    case binary:at(LogIdxs, No) of 
	0 -> 
	    no_log;
	LogIdx -> 
	    io:format("Asking for LogIdx: ~p~n", [LogIdx]),
	    {ok, Data} = read_addr(?CHRONO_LOG_BASE_ADDR + (LogIdx-1) * ?CHRONO_LOG_ENTRY_OFFSET, 16#32),
	    {ok, parse(chrono_log, Data)}
    end.

get(What) ->
    {ok, RawData} = read(What),
    {ok, parse(What, RawData)}.

read(hist) ->
    read_addr(?HIST_ADDR, 16#12);
read(hiking_logs) ->
    read_addr(?HIKING_LOGS_ADDR, 16#14);
read(chrono_logs) ->
    read_addr(?CHRONO_LOGS_ADDR, 16#19).

parse(hiking_logs, <<?HIKING_LOGS_ADDR:16/little, 
	PayLoadLen, 
	IdxData:PayLoadLen/binary>>) ->
    IdxData;
parse(chrono_log, <<_TwoFirstBytes:2/binary, PayLoadLen, Rest:PayLoadLen/binary>>) ->
    parse_chrono_log(Rest);
parse(hist, <<?HIST_ADDR:16/little,18,
	Year, Month, Day,
	HighPoint:16/little,
	Ascent:32/little,
	Descent:32/little,
	_Rest/binary>>) ->
    [{date, Year, Month, Day},{highpoint, HighPoint},{ascent, Ascent}, {descent, Descent}];
parse(chrono_logs, <<?CHRONO_LOGS_ADDR:16/little, 
	PayLoadLen, 
	IdxData:PayLoadLen/binary>>) ->
    IdxData;
parse(hiking_log, <<_:16, 48, 
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

parse_chrono_log(<<FirstChunk:8,
		   Date:5/binary,
		   Interval:8,
		   HrExist:8,
		   TotalAscent:16/big,
		   TotalDescent:16/big,
		   _Skip1:8,
		   Laps:8,
		   Duration:4/binary,
		   InterUp:16/big,
		   InterDown:16/big,
		   HighestAlt:16/big,
		   HighestTime:4/binary,
		   LowestAlt:16/big,
		   LowestTime:4/binary,
		   _skip1:8,
		   HrMin:8,
		   HrMax:8,
		   HrAvg:8,
		   HrLimitHigh:8,
		   HrLimitLow:8,
		   HrOverLimit:16/big,
		   HrInLimit:16/big,
		   HrUnderLimit:16/big, _Rest/binary>>) ->
    [{first_chunk, FirstChunk}, 
     {date, parse_timestamp(Date)},
     {interval, Interval}, 
     {hr_exist, HrExist}, 
     {total_ascent, TotalAscent}, 
     {total_descent, TotalDescent}, 
     {laps, Laps}, 
     {duration, Duration},
     {inter_up, InterUp}, 
     {inter_down, InterDown},
     {highest_alt, HighestAlt},
     {highest_time, parse_timestamp(HighestTime)},
     {lowest_alt, LowestAlt},
     {lowest_time, parse_timestamp(LowestTime)},
     {hr_min, HrMin},
     {hr_max, HrMax},
     {hr_avg, HrAvg},
     {hr_limit_high, HrLimitHigh},
     {hr_limit_low, HrLimitLow},
     {hr_over_limit, HrOverLimit},
     {hr_in_limit, HrInLimit},
     {hr_under_limit, HrUnderLimit},
     {graph_data, parse_chrono_graph_data(get_chrono_graph_data(FirstChunk), [])}].

get_chrono_graph_data(FirstChunkAddr) ->
    get_chrono_graph_data(<<>>, FirstChunkAddr).

get_chrono_graph_data(Res, 0) ->
    io:format("graph_data: ~p~n", [Res]),
    Res;
get_chrono_graph_data(Res, Addr) ->
    BaseAddr = 16#2000 + (Addr -1)*128,
    %% hmm.. maybe we can't read more than 50 bytes?
    {ok, <<_:3/binary, _:1/binary, Data1:49/binary>>} = read_addr(BaseAddr, 50),
    {ok, <<_:3/binary, Data2:50/binary>>} = read_addr(BaseAddr + 50, 50),
    {ok, <<_:3/binary, Data3:27/binary, Next:8>>} = read_addr(BaseAddr + 100, 28),
    get_chrono_graph_data(<<Res/binary, Data1/binary, Data2/binary, Data3/binary>>, Next).

parse_chrono_graph_data(<<130, _Skip:10/binary, Rest/binary>>, ResList) ->
    parse_chrono_graph_data(Rest, ResList);
parse_chrono_graph_data(<<128, _Rest/binary>>, ResList) ->
    lists:reverse(ResList);
parse_chrono_graph_data(<<B1:16/big, B2:8, Rest/binary>>, ResList) ->
    parse_chrono_graph_data(Rest, [{B1, B2} |ResList]).


    
    

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


%%%%%%%%%%%%%%%%%%%%%% checksum %%%%%%%%%%%%%%%%%%%%%%%%%%
%% this verifies AND returns only the data part. BAD!
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


%%%%%%%%%%%%%%%%%%%%%%%%%% port stuff %%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    start("/dev/ttyUSB0").

start(Device) ->
    start("./suunto_port", Device).

start(ExtPrg, Device) ->
    spawn(?MODULE, init, [ExtPrg, Device]).

stop() ->
    suunto_port ! stop.


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

code_switch() ->
    suunto_port ! code_switch.

loop(Port) ->
    receive
	code_switch ->
	    suunto:loop(Port);
	{call, Caller, Msg} ->
	    Port ! {self(), {command, Msg}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {response, Data}
	    end,
	    suunto:loop(Port);
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
