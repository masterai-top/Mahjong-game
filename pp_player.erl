%%%--------------------------------------
%%% @Module  : pp_player
%%% @Author  : 
%%% @Created : 
%%% @Description: 角色功能管理  
%%%--------------------------------------
-module(pp_player).
-compile(export_all).
-include("common.hrl").
-include("record.hrl").
-include("debug.hrl").
-include("proto_player.hrl").

%% API Functions

handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%%--------------------------------------
%%Protocol: 11000 玩家自身信息(FULL)   
%%--------------------------------------
handle_cmd(11000, Seq, Status, Data) ->
	UidCode = lib_player:player_id_to_special_id(Status#player.id),
	NowTime = util:unixtime(),
	VIP = lib_vip:is_svip(Status#player.vip, Status#player.vip_expire_time, NowTime),
	%%VIP = ?IF((Status#player.vip_expire_time - NowTime)  > 0 andalso Status#player.vip > 0, 1, 0),
	Info = {player_info,Status#player.id,UidCode,Status#player.server_id,Status#player.level,
			Status#player.exp,Status#player.gender,Status#player.facelook,
			VIP,Status#player.vip_expire_time,Status#player.coin,Status#player.gold,
			Status#player.red_card,Status#player.win_num,Status#player.lose_num,
			Status#player.winning,Status#player.nick, Status#player.skin, Status#player.newbie_guide_status,
			Status#player.activity_red_point, Status#player.exchange_red_point, Status#player.clicked_acty_list,
			Status#player.open_system_list, Status#player.clicked_btn_list, Status#player.day_first_pay, Status#player.vip_coin_double_num},
	NewStatus = lib_player:reset_player_login_num(Status),
	%%io:format("~n====11000=====is_first_login:~p==activity_red_point:~p==exchange_red_point:~p==~n", 
	%%		[Status#player.is_first_login, Status#player.activity_red_point, Status#player.exchange_red_point]),
	lib_send:pack_and_send(NewStatus#player.other#player_other.pid_send, 11000, {s2c_player_info,Info}, Seq),
	{ok, NewStatus};

%%--------------------------------------
%%Protocol: 11002 修改性别
%%--------------------------------------
handle_cmd(11002, Seq, Status, Data) ->
	[Sex] = Data,
    case lib_player:change_sex(Status, Sex) of
		{ok, NewStatus}->
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11002, {s2c_change_sex,0,Sex}, Seq),
			{ok, NewStatus};
		_ ->
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11002, {s2c_change_sex,1,Sex}, Seq)
	end;

%%--------------------------------------
%%Protocol: 11003 修改游戏名
%%--------------------------------------
handle_cmd(11003, Seq, Status, Data) ->
    [Nick] = Data,
	case lib_player:change_nick(Status, Nick) of
		{ok, NewStatus} ->
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11003, {s2c_change_nick,0,Nick}, Seq),
			{ok, NewStatus};
		_ ->
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11003, {s2c_change_nick,11003001,Nick}, Seq)
	end;

%%--------------------------------------
%%Protocol: 11007 更新玩家头像
%%--------------------------------------
handle_cmd(11007, Seq, Status, [IconId]) ->
	case lib_player:change_facelook(Status, IconId) of
		{ok, _IconId} ->
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11007, {s2c_change_facelook, ?FLAG_SUCC, IconId}, Seq),
			{ok, Status#player{facelook = IconId}};
		_Error ->
			?ERROR_MSG("~s handle_cmd(11007 error:  player_id[~p], _Error:~p, IconId:~p, code:~p~n",
									   [misc:time_format(game_timer:now()), Status#player.id, _Error, IconId, ?FLAG_FAIL]),	
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11007, {s2c_change_facelook, ?FLAG_FAIL, Status#player.facelook}, Seq)
	end;

%%--------------------------------------
%%Protocol: 11008 获取头像列表
%%--------------------------------------
handle_cmd(11008, Seq, Status, _) ->
	case lib_player:get_facelook_list(Status#player.facelook, Status#player.skin) of
		FaceLookList when is_list(FaceLookList) ->
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11008, {s2c_get_facelook_list, FaceLookList}, Seq);
		_ ->
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11008, {s2c_get_facelook_list, []}, Seq)
	end,
	{ok, Status};

%%--------------------------------------
%%Protocol: 11010 获取会员信息
%%--------------------------------------
handle_cmd(11010, Seq, Status, _) ->
	ReturnInfo = lib_vip:get_vip_info(Status),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11010, ReturnInfo, Seq),
	{ok, Status};

%%--------------------------------------
%%Protocol: 11011 获取斗地主超级加倍剩余次数
%%--------------------------------------
handle_cmd(11011, Seq, Status, _) ->	
	{NewStatus, Time} = lib_player:get_super_time(Status),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11011, {s2c_get_super_time, Time}, Seq),
	{ok, NewStatus};	

handle_cmd(11012, Seq, Status, _) ->
	NewStatus = lib_player:finish_newbie_guide(Status),
	{ok, NewStatus};	

%% 热点
handle_cmd(11013, Seq, Status, [HotPoint]) ->
	%%io:format("~n====11013=====HotPoint:~p======~n", [HotPoint]),
	HopPointList = [9, 16, 17, 27],
	case lists:member(HotPoint, HopPointList) of
		true ->
			lib_player:update_player_game(HotPoint, Status);
		_ ->
			skip
	end,
	#player{id=UID, channel_id=ChannelId} = Status,
	?ASYNC_EXEC(db_agent_log:inset_hotpoint_log(UID, ChannelId, HotPoint));

%% 领取vip奖励
handle_cmd(11015, Seq, Status, _) ->
	%%io:format("~n====11015===========~n"),
	{Code, NewStatus, RewardList} = lib_vip:get_vip_reward(Status),
	RewardRecord = [{vip_reward_info, GoodsTID, Num} || {GoodsTID, Num} <- RewardList],
	MsgRecord = {s2c_get_vip_reward, Code, RewardRecord},
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11015, MsgRecord, Seq),
	{ok, NewStatus};

%% 点击活动
handle_cmd(11017, Seq, #player{clicked_acty_list=CAList} = Status, Data) ->
	%io:format("~n====11017===========~n"),
	[ActyId] = Data,
	NewCAList = [ActyId|lists:delete(ActyId, CAList)],
	NewStatus = Status#player{clicked_acty_list = NewCAList},
	MsgRecord = {s2c_record_acty_click, ?FLAG_SUCC},
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11017, MsgRecord, Seq),
	{ok, NewStatus};

%% ios内购充值
handle_cmd(11019, Seq, Status, [UID, RechargedID, ProductID, PayAmount, OrderNote]) when Status#player.id =:= UID ->
 	io:format("~n====11019======[UID, RechargedID, ProductID, PayAmount, OrderNote]:~p=============~n", 
 			  [[UID, RechargedID, ProductID, PayAmount, OrderNote]]),
	Code = lib_vip:ios_inn_pay(Status, RechargedID, ProductID, PayAmount, OrderNote),
	if
		Code =/= 0 ->
			NewCode = lib_vip:ios_inn_pay_ex(Status, RechargedID, ProductID, PayAmount, OrderNote);
		true ->
			NewCode = Code
	end,

    
	io:format("~n====11019====Code:~p=========~n", [NewCode]),
	MsgRecord = {s2c_ios_inn_pay, NewCode},
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11019, MsgRecord, Seq),
	{ok, Status};

%%  按钮点击记录
handle_cmd(11021, Seq, #player{clicked_btn_list=CBList} = Status, Data) ->
	%io:format("~n====11021===========~n"),
	[BtnId] = Data,
	case data_button:get(BtnId) of
		undefined ->
			NewStatus = Status;
		_ ->
			NewCBList = [BtnId|lists:delete(BtnId, CBList)],
			NewStatus = Status#player{clicked_btn_list=NewCBList}
	end,
	MsgRecord = {s2c_record_btn_click, ?FLAG_SUCC},
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11021, MsgRecord, Seq),
	{ok, NewStatus};

%% 11022 获取玩家面板信息
handle_cmd(11022, Seq, Status, Data) ->
	?PRINT("c2s_get_player_board_info PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[TargetId] = Data,
	case lib_player:get_playre_info(TargetId) of
		undefined ->
			?ERROR_MSG("c2s_get_player_board_info PlayerId: ~p undefined", [TargetId]),
			skip;
		Target ->
			RetRecord = 
				#s2c_get_player_board_info{
					player_id = Target#player.id,
					special_id = lib_player:player_id_to_special_id(Target#player.id),
					nick = Target#player.nick,
					vip = Target#player.vip,
					facelook = Target#player.facelook,
					coin = Target#player.coin,
					gold = Target#player.gold,
					win_num = Target#player.win_num,
					lose_num = Target#player.lose_num
				},
			?PRINT("s2c_get_player_board_info PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 11022, RetRecord, Seq)
	end;

handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	io:format("~n====_Cmd:~p======_Data:~p=====~n", [_Cmd, _Data]),
	{error, "handle_account no match"}.
