%%%-----------------------------------
%%% @Module  : game_server_app
%%% @Author  : smyx
%%% @Created : 2013.06.30
%%% @Description: 游戏服务器应用启动
%%%-----------------------------------
-module(game_server_app).
-behaviour(application).
-export([start/2, stop/1]).
-include("common.hrl").
-include("record.hrl").

start(normal, []) ->    
    ping_gateway(),
    
    ets:new(?ETS_SYSTEM_INFO, [set, public, named_table]),
    ets:new(?ETS_MONITOR_PID, [set, public, named_table]),
    ets:new(?ETS_STAT_SOCKET, [set, public, named_table]),
    ets:new(?ETS_STAT_DB, [set, public, named_table]),
    
    [Port, _Acceptor_num, _Max_connections] = config:get_tcp_listener(server),
    [Ip] = config:get_tcp_listener_ip(server),
	LogPath = config:get_log_path(server),
    LogLevel = config:get_log_level(server),
	Gateways = config:get_gateway_node(server),
	ServerNum = config:get_server_num(),
    ServerType = config:get_server_type(),
	loglevel:set(tool:to_integer(LogLevel)),    
	io:format("LogPath:~p, loglevel: ~p~n", [LogPath, LogLevel]),
    {ok, SupPid} = game_server_sup:start_link(),
    game_timer:start(game_server_sup),
    game_server:start(
                  [Ip, tool:to_integer(Port), tool:to_integer(ServerNum), tool:to_integer(ServerType), Gateways, LogPath, tool:to_integer(LogLevel)]
                ),
    {ok, SupPid}.

stop(_State) ->   
    void. 

ping_gateway()->
    case config:get_gateway_node(server) of
        undefined -> no_action;
        DataList ->    
			Fun = fun(GatewayNode) ->
						  catch net_adm:ping(GatewayNode)
				  end ,
			lists:foreach(Fun, DataList)
    end.



