%%--------------------------------------
%% Module : pp_chat
%% Author : water
%% Created: Tue Feb 05 16:02:06 2013
%% Description: 聊天模块
%%--------------------------------------
-module(pp_chat).

%%--------------------------------------
%% Include files
%%--------------------------------------
-include("common.hrl").
-include("record.hrl").


%%--------------------------------------
%% Exported Functions
%%--------------------------------------
-compile(export_all).

%- 返回码:  0: 未定义错误  1：成功  2: 禁言 3:黑名单 4:不在线 5:发言太快了
-define(MAX_LENGTH, 150).        %%消息最大长度
-define(MIN_CHAT_INTERVAL, 5).   %%消息发送间隔秒
-define(MIN_LENGTH, 10).         %%意见的最小长度
-define(MIN_SUGGESTION_INTERVAL, 15 * 60 * 1000). %% 提意见的时间间隔

handle(Cmd, Seq, Status, Data) ->
	handle_cmd(Cmd, Seq, Status, Data).

%% handle_cmd(12001, Seq, Status, Data) ->
%% 	io:format("==10001===~p~n", [Data]),
%% 	[Tick, Sign, Version, Agent, AccounId, Accname, Password, Nick,Channel, Device, Did] = Data,
%% 	{ok, PlayerId} = lib_account:has_player_id([Tick, Sign, Version, Agent, AccounId, Accname, Password, Nick,Channel, Device, Did, Status#player.last_login_ip]),
%% 	Time = util:unixtime(),
%% 	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 10001, {s2c_account_login,Time,PlayerId}, Seq);
%%--------------------------------------
%%Protocol: 12001 发送世界信息
%%--------------------------------------
%% handle_cmd(12001, Seq, Status, Data) ->
%% 	[Str] = Data,
%% 	Length = guild_util:get_utf8_list(Str),
%% 	CanChat = check_chat_interval(),
%% 	if 
%% 		Length > ?MAX_LENGTH ->
%% 			pack_and_send(Status, 11001, [0]);
%% 		CanChat =:= false ->
%% 			pack_and_send(Status, 11001, [5]);
%% 		true ->
%% 			case donttalk() of
%% 				true ->	
%% 					if 
%% 						Status#player.level > 10 ->
%% 							Data = lib_words_ver:words_filter([Content]),
%% 							lib_chat:chat_world(Status, [Data]);
%% 						true ->
%% 							ok
%% 					end,
%% 					pack_and_send(Status, 11001, [1]);
%% 				false ->
%% 					pack_and_send(Status, 11001, [2])
%% 			end
%% 	end;

%% %%--------------------------------------
%% %%Protocol: 11004 发送私聊信息
%% %%--------------------------------------
%% handle(11004, Status, [Uid, Content]) ->
%% 	[_,_|Str] = Content,
%% 	Length = guild_util:get_utf8_list(Str),
%%     CanChat = check_chat_interval(),
%% %% 	io:format("Uid === ~p~n Content === ~p~n", [Uid, Content]),
%%     if 
%% 		Uid =:= Status#player.id ->
%% 			pack_and_send(Status, 11004, [0]);
%%         Length > ?MAX_LENGTH ->
%%            pack_and_send(Status, 11004, [0]);
%%         CanChat =:= false ->
%%             pack_and_send(Status, 11004, [5]);
%%        true ->
%%            case donttalk() of
%%                true ->
%% 				   Data = lib_words_ver:words_filter([Content]),
%% 				   gen_server:cast(mod_relation:get_available_pid(),
%% 								   {'send_msg', [Status#player.type, Status#player.id, Status#player.nick, Uid, Data, Status#player.other#player_other.pid_send]});
%% 			   false ->
%% %% 					io:format("donnttalk"),
%%                    pack_and_send(Status, 11004, [2])
%%          end
%%    end;

%%--------------------------------------
%%Protocol: 12004 GM命令
%%--------------------------------------
handle_cmd(12004, Seq, Status, Data) ->	
	[Content] =  Data,
	case config:get_can_gmcmd() of
		1 ->
			GmData = string:tokens(tool:to_list(Content), " "),
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 12004, {s2c_chat_gm,0}, Seq),
			lib_gm:handle_cmd(Status, GmData);
        _ -> 
            lib_send:pack_and_send(Status#player.other#player_other.pid_send, 12004, {s2c_chat_gm,1}, Seq)
	end;

%% %%--------------------------------------
%% %%Protocol: 11007 提意见
%% %%--------------------------------------
%% handle(11007, Status, [Content]) ->   
%%     Length = guild_util:get_utf8_list(Content),
%%     if
%%         Length > ?MAX_LENGTH ->
%%             pack_and_send(Status, 11007, [3]);
%%         Length < ?MIN_LENGTH ->
%%             pack_and_send(Status, 11007, [4]);
%%         true ->    
%%             case tool:is_operate_ok(pp_11007, ?MIN_SUGGESTION_INTERVAL) of
%%                 true -> 
%%                     spawn(fun() -> db_agent_log:insert_suggestion_log(Status#player.id, Status#player.nick, 
%%                                                                       Status#player.account_id,Status#player.account_name, 
%%                                                                       Content, util:unixtime()) end),
%%                     pack_and_send(Status, 11007, [1]);
%%                 _ ->
%%                     pack_and_send(Status, 11007, [2])
%%             end
%%     end;
       

handle_cmd(Cmd, Seq, Status, Data) ->
    ?ERROR_MSG("Undefine handler: Cmd ~p, Status:~p, Data:~p~n", [Cmd, Status, Data]),
    skip.
    
pack_and_send(Status, Cmd, Data) ->
    {ok, BinData} = pt_11:write(Cmd, Data),
    lib_send:send_to_sid(Status#player.other#player_other.pid_send, BinData).

%%检查禁言情况, 玩家进程调用
%%返回true, 可以发言, false不可以发言
donttalk() ->
    Now = util:unixtime(),
    case get(ban_chat) of
        [BeginTime, EndTime] ->
			if
				BeginTime =:= 0 -> true;
				EndTime =:= 0 -> false;
				Now < EndTime -> false;
				true -> true
			end;
        _Other ->
             true
    end.

%% 检查聊天间隔
check_chat_interval() ->
    case get(prev_chat_time) of
		undefined -> 
            PrevChatTime = 0;
		Val ->
		    PrevChatTime = Val 
	end,
    Now = util:unixtime(),
    put(prev_chat_time, Now), %%先更新发言时间再说,不准无技术含量的踩点临界值行为.
    Now >= PrevChatTime + ?MIN_CHAT_INTERVAL.

