%%%-----------------------------------
%%% @Module  : game_tcp_listener_sup
%%% @Author  : csj
%%% @Created : 2010.10.05
%%% @Description: tcp listerner 监控树
%%%-----------------------------------

-module(game_tcp_listener_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Port) ->
    supervisor:start_link(?MODULE, {10, Port}).

init({AcceptorCount, Port}) ->
    {ok,
        {{one_for_all, 10, 10},
            [
                {
                    game_tcp_acceptor_sup,
                    {game_tcp_acceptor_sup, start_link, []},
                    transient,
                    infinity,
                    supervisor,
                    [game_tcp_acceptor_sup]
                },
                {
                    lists:concat([game_tcp_listener_,Port]),
                    {game_tcp_listener, start_link, [AcceptorCount, Port]},
                    transient,
                    100,
                    worker,
                    [game_tcp_listener]
%%                 },
%% 				{
%%                     lists:concat([cgame_tcp_listener_,Port-100]),
%%                     {game_tcp_listener, start_link, [AcceptorCount, Port-100]},
%%                     transient,
%%                     100,
%%                     worker,
%%                     [game_tcp_listener]
%%                 },
%% 				{
%%                     lists:concat([game_tcp_listener_,Port-200]),
%%                     {game_tcp_listener, start_link, [AcceptorCount, Port-200]},
%%                     transient,
%%                     100,
%%                     worker,
%%                     [game_tcp_listener]
                }
            ]
        }
    }.
