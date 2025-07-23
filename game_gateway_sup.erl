%%%-----------------------------------
%%% @Module  : game_gateway_sup
%%% @Author  : smyx
%%% @Created : 2013.06.30
%%% @Description: 网关监控树
%%%-----------------------------------
-module(game_gateway_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

-include("common.hrl").

start_link([Ip, Port, LogFile, LogLevel]) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, [Ip, Port, LogFile, LogLevel]).

init([Ip, Port, LogFile, LogLevel]) ->
    {ok,
        {
            {one_for_one, 3, 10},
            [
                {
                    game_gateway,
                    {game_gateway, start_link, [Port]},
                    permanent,
                    10000,
                    supervisor,
                    [game_gateway]
                },
                {
                    mod_disperse,
                    {mod_disperse, start_link,[Ip, Port, 0, 0,[]]},
                    permanent,
                    10000,
                    supervisor,
                    [mod_disperse]
                },
				{
				 	?LOGMODULE, 
					{logger_h, start_link, [LogFile, LogLevel]}, 
					permanent, 
					5000, 
					worker, 
					dynamic
				}
            ]
        }
    }.
