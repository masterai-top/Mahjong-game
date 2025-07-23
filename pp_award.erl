%%%--------------------------------------
%%% @Module  : pp_award
%%% @Author  : xws
%%% @Created : 2016-9-14
%%% @Description:  商城操作
%%%--------------------------------------
-module(pp_award).
-include("common.hrl").
-include("record.hrl").
-include("log.hrl").
-include("debug.hrl").
-include("proto_20_award.hrl").
-include("award.hrl").


-compile(export_all).

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 获取玩家奖励数据
handle_cmd(20000, Seq, Status, Data) ->
	?PRINT("c2s_get_player_award_data PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
    #player_award{
    	acc_vip_times = AVTimes,
    	acc_daily_times = ADTimes,
    	acc_system_times = ASTimes,
    	acc_login_days = ALDays,
    	tomorrow_status = TStatus,
    	tomorrow_award_time = TATime
    } = lib_award:get_player_award_data(Status#player.id),
    LeftTime = TATime - util:unixtime(),
	RetRecord = #s2c_get_player_award_data{
					acc_vip_times = AVTimes,
					acc_daily_times = ADTimes,
					acc_system_times = ASTimes,
					acc_login_days = ALDays,
					is_tomorrow_on = TStatus,
					tomorrow_left_time = ?IF(LeftTime > 0, LeftTime, 0)
					},
	?PRINT("s2c_get_player_award_data PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 20000, RetRecord, Seq);

%% 20001 领取系统补偿
handle_cmd(20001, Seq, Status, Data) ->
	?PRINT("c2s_get_system_award PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{RetCode, ResList, NewStatus} = lib_award:get_system_award(Status),
	ResList1 = [#proto_resurce{resurce_id=Id, num=Num} || {Id, Num} <- ResList],
	RetRecord = #s2c_get_system_award{
					ret_code = RetCode,
					resurce_list = ResList1
				},
	?PRINT("s2c_get_system_award PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 20001, RetRecord, Seq),
	{ok, NewStatus};

% %% 20002 领取玩家每日奖励(暂时屏蔽)
% handle_cmd(20002, Seq, Status, Data) ->
% 	?PRINT("c2s_get_day_award PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
% 	{RetCode, Pos, ResList, NewStatus} = lib_award:get_day_award(Status),
% 	ResList1 = [#proto_resurce{resurce_id=Id, num=Num} || {Id, Num} <- ResList],
% 	RetRecord = #s2c_get_day_award{
% 					ret_code = RetCode,
% 					pos = Pos,
% 					resurce_list = ResList1
% 				},
% 	?PRINT("s2c_get_day_award PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
% 	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 20002, RetRecord, Seq),
% 	{ok, NewStatus};

%% 20003 领取玩家VIP奖励
handle_cmd(20003, Seq, Status, Data) ->
	?PRINT("c2s_get_vip_award PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{RetCode, Pos, ResList, NewStatus} = lib_award:get_vip_award(Status),
	ResList1 = [#proto_resurce{resurce_id=Id, num=Num} || {Id, Num} <- ResList],
	RetRecord = #s2c_get_vip_award{
					ret_code = RetCode,
					pos = Pos,
					resurce_list = ResList1
				},
	?PRINT("s2c_get_vip_award PlayerId: ~p, RetRecord:~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 20003, RetRecord, Seq),
	{ok, NewStatus};

%% 20004 领取明日礼包奖励
handle_cmd(20004, Seq, Status, Data) ->
	?PRINT("c2s_get_tomorrow_award PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{RetCode, NewStatus, ResList} = lib_award:get_tomorrow_award(Status),

	ResList1 = [#proto_resurce{resurce_id=Id, num=Num} || {Id, Num} <- ResList],
	RetRecord = #s2c_get_tomorrow_award{
					ret_code = RetCode,
					resurce_list = ResList1
				},
	?PRINT("s2c_get_tomorrow_award PlayerId: ~p, RetRecord:~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 20004, RetRecord, Seq),
	{ok, NewStatus};

%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_goods no match"}.





