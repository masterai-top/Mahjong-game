%%%-----------------------------------
%%% @Module  : game_tcp_client_sup
%%% @Author  : csj
%%% @Created : 2010.10.05
%%% @Description: 客户端服务监控树
%%%-----------------------------------
-module(game_tcp_client_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{game_server_reader, {game_server_reader,start_link,[]},
            temporary, brutal_kill, worker, [game_server_reader]}]}}.
