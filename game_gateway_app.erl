%%%-----------------------------------
%%% @Module  : game_gateway_app
%%% @Author  : smyx
%%% @Created : 2014.06.30
%%% @Description: 启动网关应用
%%%-----------------------------------
-module(game_gateway_app).
-behaviour(application).
-export([start/2, stop/1]).   
-include("common.hrl"). 

start(_Type, _Args) ->
	ets:new(?ETS_SYSTEM_INFO, [set, public, named_table]),
	ets:new(?ETS_MONITOR_PID, [set, public, named_table]),
	ets:new(?ETS_STAT_SOCKET, [set, public, named_table]),
	ets:new(?ETS_STAT_DB, [set, public, named_table]),
	
	[Port, _Acceptor_num, _Max_connections] = config:get_tcp_listener(gateway),
	[Ip] = config:get_tcp_listener_ip(gateway),
	LogPath = config:get_log_path(gateway),
	LogLevel = config:get_log_level(gateway), 
	loglevel:set(tool:to_integer(LogLevel)),
	LogFile = lists:concat([LogPath, "/log"]),
	io:format("LogFile:~p, loglevel: ~p Ip:~p ~n", [LogFile, LogLevel, Ip]),
	main:init_db(gateway),
%%  	game_timer:start(game_gateway_sup),
	case filelib:is_dir(LogPath) of
		true -> skip;
		false ->	file:make_dir(LogPath)
	end,
	game_gateway_sup:start_link([Ip, tool:to_integer(Port), LogFile, tool:to_integer(LogLevel)]).
%% 	game_timer:start(game_gateway_sup).

stop(_State) ->   
	void.
