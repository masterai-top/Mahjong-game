%%%---------------------------------------
%%% @Module  : game_gateway_server
%%% @Author  : smyx
%%% @Created : 2013.06.30
%%% @Description:  网关tcp服务器
%%%---------------------------------------
-module(game_gateway_server).
-export([start_raw_server/3, stop/1, children/1]).
-include("common.hrl").
-define(ACCEPT_NUM,50) .
	
%%开启TCP_SERVER
%%Port：端口
%%Fun：回调监听函数
%%Max：最大连接数
start_raw_server(Port, Fun, Max) ->
	Name = port_name(Port),
	case whereis(Name) of
            undefined ->
                Self = self(),
                Pid = spawn(fun() -> code_start(Self, Port, Fun, Max) end),
                receive
                    {Pid, ok} ->
                        register(Name, Pid);
                    {Pid, Error} ->
                        Error
                end;
            _Pid ->
                {error, already_started}
	end.

%%停止TCP_SERVER服务器
%%Port：端口
stop(Port) when is_integer(Port) ->
    Name = port_name(Port),
    case whereis(Name) of
        undefined ->
            not_started;
        Pid ->
            exit(Pid, kill),
            (catch unregister(Name)),
            stopped
    end.

%%获取所有子进程
%%Port：端口
children(Port) when is_integer(Port) ->
    port_name(Port) ! {children, self()},
    receive
        {session_server, Reply} -> Reply
    end.

%%组合主进程
%%Port：端口
port_name(Port) 
  when is_integer(Port) ->
	list_to_atom("game_gateway_" ++ integer_to_list(Port)).

%%打开监听
%%Master：主进程
%%Port:端口
%%Fun:回调
%%Max:最大连接数
code_start(Master, Port, Fun, Max) ->
	process_flag(trap_exit, true),
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, Listen} ->
			Master ! {self(), ok},
			%% New = start_accept(Listen, Fun),
			%% socket_loop(Listen, New, [], Fun, Max);
			lists:foreach(fun(_X) ->
								  New = start_accept(Listen, Fun),
								  socket_loop(Listen, New, [], Fun, Max)
						  end ,lists:seq(1, ?ACCEPT_NUM))	;
		Error ->
			Master ! {self(), Error}
	end.

%%循环接收外部连接
%%Listen：
%%New:当前外部连接子进程
%%Active:所有的子进程
%%Fun:回调
%%Max:最大连接数
socket_loop(Listen, New, Active, Fun, Max) ->
    receive
        {istarted, New} ->
            Active1 = [New|Active],
            possibly_start_another(false, Listen, Active1, Fun, Max);
        {'EXIT', New, _Why} ->
            possibly_start_another(false, Listen, Active, Fun, Max);
        {'EXIT', Pid, _Why} ->
            Active1 = lists:delete(Pid, Active),
            possibly_start_another(New, Listen, Active1, Fun, Max);
        {children, From} ->
            From ! {session_server, Active},
            socket_loop(Listen, New, Active, Fun, Max);
        Other ->
            ?DEBUG("Here in loop:~p~n",[Other])
    end.

%%开始一个新的子进程
%%参数如上
possibly_start_another(New, Listen, Active, Fun, Max) 
  when is_pid(New) ->
    socket_loop(Listen, New, Active, Fun, Max);

possibly_start_another(false, Listen, Active, Fun, Max) ->
    case length(Active) of
        N when N < Max ->
        New = start_accept(Listen, Fun),
        socket_loop(Listen, New, Active, Fun, Max);
        _ ->
        error_logger:warning_report(
                [{module, ?MODULE},
                {line, ?LINE},
                {message, "Connections maxed out"},
                {maximum, Max},
                {connected, length(Active)},
                {now, now()}]),
        socket_loop(Listen, false, Active, Fun, Max)
    end.

%%接受来自客户端的连接
start_accept(Listen, Fun) ->
    S = self(),
    spawn_link(fun() -> start_child(S, Listen, Fun) end).

start_child(Parent, Listen, Fun) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            Parent ! {istarted, self()},
            Fun(Socket);
        _Other ->
            exit(oops)
    end.
