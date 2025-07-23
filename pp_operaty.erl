%%%--------------------------------------
%%% @Module  : pp_operaty
%%% @Author  : xws
%%% @Created : 2017-1-4
%%% @Description:  商城操作
%%%--------------------------------------
-module(pp_operaty).
-include("common.hrl").
-include("record.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("log.hrl").
-include("debug.hrl").
-include("proto/proto_28_operaty.hrl").


-compile(export_all).

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%%--------------------------------------
%%Protocol: 28000 获取运营活动信息
%%-------------------------------------- 
handle_cmd(28000, Seq, Status, [UID]) when UID =:= Status#player.id ->
	OperatyInfo = lib_operaty:get_operaty_info(UID, Status#player.nick),
	RetRecord = {s2c_operaty_info, OperatyInfo},
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 28000, RetRecord, Seq),
	{ok, Status};

%%--------------------------------------
%%Protocol: 28001 获取活动状态信息
%%-------------------------------------- 
handle_cmd(28001, Seq, Status, _) -> 
	OperatyList = lib_operaty:get_operaty_state(),
	OperatyList1 = [#operaty_state{operaty_id=Id, state=State} || {Id, State} <- OperatyList],
	RetRecord = {s2c_get_operaty_state, OperatyList1},
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 28001, RetRecord, Seq),
	{ok, Status};

%%--------------------------------------
%%Protocol: 28100 获取运营活动奖励
%%-------------------------------------- 
handle_cmd(28100, Seq, Status, [SubOperatyID]) ->
	{NewPS, Code} = lib_operaty:give_operaty_reward(Status, SubOperatyID),
	RetRecord = {s2c_give_operaty_reward, Code},
	lib_send:pack_and_send(NewPS#player.other#player_other.pid_send, 28100, RetRecord, Seq),
	{ok, NewPS};

%%--------------------------------------
%%Protocol: 28101 分享比赛结果
%%-------------------------------------- 
handle_cmd(28101, Seq, Status, [SubOperatyID, _Data]) ->
	lib_operaty_6:finish_game_share_operaty(Status#player.id, SubOperatyID),
	%% lib_task_evnet:share_game(玩家Id)进行游戏分享
	lib_task_event:share_game(Status#player.id),
	RetRecord = {s2c_share_game, ?FLAG_SUCC},
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 28101, RetRecord, Seq),
	{ok, Status};


%%--------------------------------------
%%Protocol: 28001 获取运营活动1数据
%%--------------------------------------
handle_cmd(28001, Seq, Status, Data) ->
	?PRINT("c2s_operacty_1_get_data PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{IsFreeDrawOn} = lib_operaty_1:get_data(Status#player.id),
	RetRecord = #s2c_operacty_1_get_data{
					is_free_draw_on = IsFreeDrawOn
				},

	?PRINT("s2c_operacty_1_get_data PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 28001, RetRecord, Seq);

%%--------------------------------------
%%Protocol: 28002 免费抽奖
%%--------------------------------------
handle_cmd(28002, Seq, Status, Data) ->
	?PRINT("c2s_operaty_1_free_draw PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{Ret, Pos, ResList, NStatus} = lib_operaty_1:free_draw(Status),
	ResList1 = [#proto_oy_resurce{resurce_id=Id, num=Num} || {Id, Num} <- ResList],
	RetRecord = #s2c_operaty_1_free_draw{
					ret_code = Ret,
					pos = Pos,
					resurce_list = ResList1
				},

	?PRINT("s2c_operaty_1_free_draw PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 28002, RetRecord, Seq),
	{ok, NStatus};

%%--------------------------------------
%%Protocol: 28021 集字兑换
%%--------------------------------------
handle_cmd(28021, Seq, Status, Data) ->
	?PRINT("c2s_operaty_2_collect_word_exchange PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[ExchangeId] = Data,
	{Ret, NStatus} = lib_operaty_2:collect_word_exchange(Status, ExchangeId),
	RetRecord = #s2c_operaty_2_collect_word_exchange{
					ret_code = Ret,
					exchange_id = ExchangeId
				},

	?PRINT("s2c_operaty_2_collect_word_exchange PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 28021, RetRecord, Seq),
	{ok, NStatus};


%%--------------------------------------
%%Protocol: 28041 获取在线奖励信息
%%--------------------------------------
handle_cmd(28041, Seq, Status, [UID]) when Status#player.id =:= UID ->
	%%io:format("~n====28041====================~n"),
	[OnlineID, RewardTime, _LeftTime] = lib_operaty_3:get_online_award_info(UID),
	ReturnInfo = {s2c_operaty_3_online, OnlineID, RewardTime, util:unixtime()},
	%%io:format("~n=======ReturnInfo:~p===========~n", [ReturnInfo]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 28041, ReturnInfo, Seq),
	{ok, Status};

%%--------------------------------------
%%Protocol: 28042 领取在线奖励活动的奖励
%%--------------------------------------
handle_cmd(28042, Seq, Status, [UID]) when Status#player.id =:= UID ->
	%%io:format("~n====28042====================~n"),
	{Code,  NewStatus, [OnlineID, RewardTime, _]} = lib_operaty_3:give_online_award(Status),
	ReturnInfo = {s2c_operate_3_reward, Code, OnlineID, RewardTime},
	%%io:format("~n====28042=======ReturnInfo:~p=============~n", [ReturnInfo]),
	lib_send:pack_and_send(NewStatus#player.other#player_other.pid_send, 28042, ReturnInfo, Seq),
	{ok, NewStatus};


%% ============================================================================================
%%--------------------------------------
%%Protocol: 28061 获取幸运基金信息
%%--------------------------------------
handle_cmd(28061, Seq, Status, [UID]) when Status#player.id =:= UID ->
	%%io:format("~n====28061====================~n"),
	{Code, LuckyFund, IsCanDraw, IsCanReward} = lib_operaty_3:get_lucky_fund_info(Status),
	%%io:format("~n====get_lucky_fund_info=====LuckyFund:~p===Code:~p==IsCanDraw:~p==IsCanReward:~p==~n", [LuckyFund, Code, IsCanDraw, IsCanReward]),
	ReturnInfo = {s2c_operaty_4_luckyfund, Code, LuckyFund, IsCanDraw, IsCanReward},
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 28061, ReturnInfo, Seq),
	{ok, Status};

%%--------------------------------------
%%Protocol: 28062 幸运基金抽奖
%%--------------------------------------
handle_cmd(28062, Seq, Status, [UID]) when Status#player.id =:= UID ->
	%%io:format("~n====28062====================~n"),
	{Code, DrawNum, IsCanDraw, IsCanReward} = lib_operaty_3:draw_lucky_fund(Status),
	%%io:format("~n====28062==Code:~p===DrawNum:~p========IsCanDraw:~p==IsCanReward:~p=====~n", [Code, DrawNum, IsCanDraw, IsCanReward]),
	ReturnInfo = {s2c_operaty_4_draw, Code, DrawNum, IsCanDraw, IsCanReward},
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 28062, ReturnInfo, Seq),
	{ok, Status};

%%--------------------------------------
%%Protocol: 28063 领取幸运基金活动奖励
%%--------------------------------------
handle_cmd(28063, Seq, Status, [UID]) when Status#player.id =:= UID ->
	%%io:format("~n====28063====================~n"),
	{Code,  Coin} = lib_operaty_3:give_lucky_fund_reward(Status, Seq),
	case Code of
		?FLAG_SUCC ->
			NewStatus = lib_money:add_coin(Status, Coin, ?LOG_LUCKY_FUND_REWARD, 28),
			lib_player:send_player_coin_redcard(NewStatus);
		_ ->
			NewStatus = Status
	end,
	ReturnInfo = {s2c_operaty_4_reward, Code,  Coin},
	lib_send:pack_and_send(NewStatus#player.other#player_other.pid_send, 28063, ReturnInfo, Seq),
	{ok, NewStatus};

%%--------------------------------------
%%Protocol: 28082 领取新春雨红包
%%--------------------------------------
handle_cmd(28082, Seq, Status, Data) ->
	?PRINT("c2s_operaty_5_pick_redpack PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{Ret, ResList, NStatus} = lib_operaty_5:pick_redpack(Status),
	ResList1 = [#proto_oy_resurce{resurce_id=Id, num=Num} || {Id, Num} <- ResList],
	RetRecord = #s2c_operaty_5_pick_redpack{
					ret_code = Ret,
					res_list = ResList1
				},
	?PRINT("s2c_operaty_5_pick_redpack PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 28082, RetRecord, Seq),
	{ok, NStatus};

%%--------------------------------------
%%Protocol: 28121 获取幸运转盘数据
%%--------------------------------------
handle_cmd(28121, Seq, Status, Data) ->
	?PRINT("c2s_operaty_8_get_data PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{FreeTimes, DrawId} = lib_operaty_8:get_data(Status),
	RetRecord = #s2c_operaty_8_get_data{
					free_times = FreeTimes,
					draw_id = DrawId
				},
	?PRINT("s2c_operaty_8_get_data PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 28121, RetRecord, Seq);

%%--------------------------------------
%%Protocol: 28122 进行抽奖
%%--------------------------------------
handle_cmd(28122, Seq, Status, Data) ->
	?PRINT("c2s_operaty_8_draw PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[DrawType] = Data,
	{RetCode, NStatus, ResList, Pos, DrawId} = lib_operaty_8:draw(Status, DrawType),
	ResList1 = [#proto_oy_resurce{resurce_id=Id, num=Num} || {Id, Num} <- ResList],
	RetRecord = #s2c_operaty_8_draw{
					ret_code = RetCode,
					draw_type = DrawType,
					pos = Pos,
					res_list = ResList1,
					draw_id = DrawId
				},
	?PRINT("s2c_operaty_8_draw PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 28122, RetRecord, Seq),
	{ok, NStatus};

%%--------------------------------------
%%Protocol: 28123 刷新奖池
%%--------------------------------------
handle_cmd(28123, Seq, Status, Data) ->
	?PRINT("c2s_operaty_8_refresh_draw PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{RetCode, NStatus, DrawId} = lib_operaty_8:refresh_draw(Status),
	RetRecord = #s2c_operaty_8_refresh_draw{
					ret_code = RetCode,
					draw_id = DrawId
				},
	?PRINT("s2c_operaty_8_refresh_draw PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 28123, RetRecord, Seq),
	{ok, NStatus};

%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_operaty no match"}.
