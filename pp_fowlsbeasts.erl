%% @author mxb
%% @doc @todo 飞禽走兽

-module(pp_fowlsbeasts).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/4]).

-include("common.hrl").
-include("record.hrl").
-include("debug.hrl").

handle(Cmd, Seq, Status, Data) ->
	%%io:format("~n===handle_cmd==Cmd:~p===table_attr:~p====~n", [Cmd, Status#player.table_attr]),
	handle_cmd(Cmd, Seq, Status, Data).

%% ====================================================================
%% Internal functions
%% ====================================================================

%%--------------------------------------
%%Protocol: 21000 请求进入房间
%%--------------------------------------
handle_cmd(21000, Seq, Status, _Data) ->
	%%io:format("~n===handle_cmd=======21000===================~n"),
	%% 	if
	%% 		Status#player.system_id =:= 0 ->
	case Status#player.table_attr of
		[fowlsbeasts, _RoomId, _RoomPid, ServerId, DomainId, GameNode, _] ->
			[Numerator, Denominator] = lottery_draw_util:get_profit_numerator_denominator(Status),
			ServerData = [ServerId, DomainId, GameNode],
			PlayerData = [Seq, Status#player.id, Status#player.nick, Status#player.facelook,  Status#player.vip, node(), Status#player.coin, 
						  Numerator, Denominator, round(Status#player.total_recharge_rmb / 100)],
			case mod_fowlsbeasts_room:get_mod_fowlsbeasts_room_pid(DomainId, ServerId) of
				Pid when is_pid(Pid) ->
					gen_server:cast(Pid, {enter_fowlsbeasts_room, PlayerData, ServerData});
				_ ->
					ReturnInfo = {s2c_enter_fowlsbeasts, ?FLAG_FAIL, 0, 0, {fb_player_info, 0, "", 0, 0, 0, 0, 0}, {bf_state_info, 0, 0},
								  {fb_player_info, 0, "", 0, 0, 0, 0, 0}, 0, [], 0, []},
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21000, ReturnInfo, Seq)
			end;
		[] -> %% 第一次进入 
			case lib_system_config:is_can_enter_game(Status#player.coin, "fowlsbeasts") of
				true ->
					[GameNode, ServerId, DomainId] = lib_game_online:select_game_server(fowlsbeasts, 400),
					[Numerator, Denominator] = lottery_draw_util:get_profit_numerator_denominator(Status),
					ServerData = [ServerId, DomainId, GameNode],
					PlayerData = [Seq, Status#player.id, Status#player.nick, Status#player.facelook,  Status#player.vip, node(), Status#player.coin, 
								  Numerator, Denominator, round(Status#player.total_recharge_rmb / 100)],
					%%io:format("~n===handle_cmd=======21000===========[Node, ServerId, Domain]:~p========~n", [[GameNode, ServerId, DomainId]]),
					case mod_fowlsbeasts_room:get_mod_fowlsbeasts_room_pid(DomainId, ServerId) of 
						Pid when is_pid(Pid) ->
							gen_server:cast(Pid, {enter_fowlsbeasts_room, PlayerData, ServerData});
						_Error ->
							%%io:format("~n===handle_cmd=======21000========get_mod_lottery_pid:~p========~n", [_Error]),
							ReturnInfo = {s2c_enter_fowlsbeasts, ?FLAG_FAIL, 0, 0, {fb_player_info, 0, "", 0, 0, 0, 0, 0}, {bf_state_info, 0, 0},
										  {fb_player_info, 0, "", 0, 0, 0, 0, 0}, 0, [], 0, []},
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21000, ReturnInfo, Seq)
					end;
				_ ->  %% 金币不足
					ReturnInfo = {s2c_enter_fowlsbeasts, 21000003, 0, 0, {fb_player_info, 0, "", 0, 0, 0, 0, 0}, {bf_state_info, 0, 0},
								  {fb_player_info, 0, "", 0, 0, 0, 0, 0}, 0, [], 0, []},
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21000, ReturnInfo, Seq)
			end;
		_ ->
			ReturnInfo = {s2c_enter_fowlsbeasts, ?FLAG_FAIL, 0, 0, {fb_player_info, 0, "", 0, 0, 0, 0, 0}, {bf_state_info, 0, 0},
						  {fb_player_info, 0, "", 0, 0, 0, 0, 0}, 0, [], 0, []},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21000, ReturnInfo, Seq)
	end,
	%% 		true ->
	%% 			ReturnInfo = {s2c_enter_fowlsbeasts, ?FLAG_FAIL, 0, 0, {fb_player_info, 0, "", 0, 0, 0, 0, 0}, {bf_state_info, 0, 0},
	%% 						  {fb_player_info, 0, "", 0, 0, 0, 0, 0}, 0, [], 0, []},
	%% 			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21000, ReturnInfo, Seq)
	%% 	end,
	{ok, Status};


%%--------------------------------------
%%Protocol: %% 21001 离开房间
%%--------------------------------------
handle_cmd(21001, Seq, Status, _Data) ->
	%%io:format("~n===handle_cmd=======21001===================~n"),
	case Status#player.table_attr of
		[fowlsbeasts, RoomId, _RoomPid, _ServerId, _DomainId, _GameNode, _] ->
			case mod_fowlsbeasts:get_alive_fb_pid(RoomId) of 
				Pid when is_pid(Pid) ->
					gen_server:cast(Pid, {leave_fowlsbeasts, [Seq, Status#player.id]});
				_Error1 ->  %% 活动进程已经挂掉了
					%%io:format("~n===handle_cmd===21001===_Error1:~p==~n", [_Error1]),
					ReturnInfo = {s2c_leave_fowlsbeasts, ?FLAG_FAIL},
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21001, ReturnInfo, Seq),
					{ok, Status#player{table_attr = []}}
			end;		
		_Error2 ->
			%%io:format("~n===handle_cmd===21001===_Error1:~p==~n", [_Error2]),
			ReturnInfo = {s2c_leave_fowlsbeasts, ?FLAG_FAIL},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21001, ReturnInfo, Seq),
			{ok, Status}
	end;

%%--------------------------------------
%%Protocol: %% 21002 押注
%%--------------------------------------
handle_cmd(21002, Seq, Status, [BetData]) ->
	%%io:format("~n===handle_cmd=====21002===BetData:~p==~n", [BetData]), 
	BetCoin = lib_fowlsbeasts:get_bet_coin(BetData),
	if
		Status#player.coin >= BetCoin ->
			case Status#player.table_attr of
				[fowlsbeasts, RoomId, _RoomPid, _ServerId, _DomainId, _GameNode, _] -> 
					BetCoin = lists:sum([Coin || {_, _, Coin} <- BetData]),
					if
						BetCoin =< Status#player.coin ->
							case mod_fowlsbeasts:get_alive_fb_pid(RoomId) of 
								Pid when is_pid(Pid) ->
									gen_server:cast(Pid, {fb_make_bet, [Seq, Status#player.id, Status#player.coin, BetData]});					
								_ ->  %% 活动进程已经挂掉了
									Info = {s2c_fb_make_bet, ?FLAG_FAIL, [], 0},
									lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21002, Info, Seq)
							end;
						true ->  %% 金币不足
							Info = {s2c_fb_make_bet, 21002002, [], 0},
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21002, Info, Seq)
					end;
				_ ->
					Info = {s2c_fb_make_bet, ?FLAG_FAIL, [], 0},
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21002, Info, Seq)
			end;
		true ->
			Info = {s2c_fb_make_bet, 21002002, [], 0},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21002, Info, Seq)
	end,
	{ok, Status};


%%--------------------------------------
%%Protocol: %% 21004 申请上庄
%%--------------------------------------
handle_cmd(21004, Seq, Status, Data) ->
	%%io:format("~n===handle_cmd=====21004===BetData:~p==~n", [Data]),
	case Status#player.table_attr of
		[fowlsbeasts, RoomId, _RoomPid, _ServerId, _DomainId, _GameNode, _] -> 
			case mod_fowlsbeasts:get_alive_fb_pid(RoomId) of 
				Pid when is_pid(Pid) ->
					gen_server:cast(Pid, {apply_for_dealer, [Seq, Status#player.id, Status#player.coin]});					
				_ ->  %% 活动进程已经挂掉了
					Info = {s2c_apply_for_dealer, ?FLAG_FAIL},
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21004, Info, Seq)
			end;
		_ ->
			Info = {s2c_apply_for_dealer, ?FLAG_FAIL},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21004, Info, Seq)
	end,
	{ok, Status};


%%--------------------------------------
%%Protocol: %% 21005 下庄
%%--------------------------------------
handle_cmd(21005, Seq, Status, Data) ->
	%%io:format("~n===handle_cmd=====21005===BetData:~p==~n", [Data]),
	case Status#player.table_attr of
		[fowlsbeasts, RoomId, _RoomPid, _ServerId, _DomainId, _GameNode, _] -> 
			case mod_fowlsbeasts:get_alive_fb_pid(RoomId) of 
				Pid when is_pid(Pid) ->
					gen_server:cast(Pid, {down_dealer, [Seq, Status#player.id]});					
				_ ->  %% 活动进程已经挂掉了
					Info = {s2c_not_be_dealer, ?FLAG_FAIL},
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21005, Info, Seq)
			end;
		_ ->
			Info = {s2c_not_be_dealer, ?FLAG_FAIL},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21005, Info, Seq)
	end,
	{ok, Status};



%%--------------------------------------
%%Protocol: %% 21012 获取押注列表
%%--------------------------------------
handle_cmd(21012, Seq, Status, _Data) ->
	%%io:format("~n===handle_cmd=====21012===BetData:~p==~n", [_Data]),
	case Status#player.table_attr of
		[fowlsbeasts, RoomId, _RoomPid, _ServerId, _DomainId, _GameNode, _] -> 
			case mod_fowlsbeasts:get_alive_fb_pid(RoomId) of 
				Pid when is_pid(Pid) ->
					gen_server:cast(Pid, {get_fb_bet_list, [Seq, Status#player.id]});					
				_ ->  %% 活动进程已经挂掉了
					Info = {s2c_fb_bet_list, [], []},
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21012, Info, Seq)
			end;
		_ ->
			Info = {s2c_fb_bet_list, [], []},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21012, Info, Seq)
	end,
	{ok, Status};


%%--------------------------------------
%%Protocol: %% 21013 获取申请上庄列表
%%--------------------------------------
handle_cmd(21013, Seq, Status, _Data) ->
	%%io:format("~n===handle_cmd=====21012===BetData:~p==~n", [_Data]),
	case Status#player.table_attr of
		[fowlsbeasts, RoomId, _RoomPid, _ServerId, _DomainId, _GameNode, _] -> 
			case mod_fowlsbeasts:get_alive_fb_pid(RoomId) of 
				Pid when is_pid(Pid) ->
					gen_server:cast(Pid, {get_fb_dealer_apply_list, [Seq, Status#player.id]});					
				_ ->  %% 活动进程已经挂掉了
					Info = {s2c_fb_dealer_apply_list, []},
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21013, Info, Seq)
			end;
		_ ->
			Info = {s2c_fb_dealer_apply_list, []},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 21013, Info, Seq)
	end,
	{ok, Status};











handle_cmd(_Cmd, _, _, _) ->
	{error, "pp_lottery no match"}.