%%%--------------------------------------
%%% @Module  : pp_recharge
%%% @Author  : xws
%%% @Created : 2016-8-18
%%% @Description:  充值操作
%%%--------------------------------------
-module(pp_recharge).
-include("common.hrl").
-include("record.hrl").
-include("log.hrl").
-include("debug.hrl").
-include("proto_15_recharge.hrl").
-include("record/data_recharge_record.hrl").

-compile(export_all).

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 15000 获取充值信息
handle_cmd(15000, Seq, Status, Data) ->
	?PRINT("c2s_get_player_recharge_data PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
    [IsFirstRecharge, THList, RList, NSRechargeId, IsGetAvaiable, LeftDay] = lib_recharge:get_player_recharge_data(Status),
    THList1 = [
    			begin
    				#recharge_config{max_buy_times=MBTimes} = data_recharge:get(RechargeId),
    			    #proto_tehui_recharge{recharge_id = RechargeId, left_times = (MBTimes - Times)}
    			end || {RechargeId, Times} <- THList
    		  ],
	RetRecord = #s2c_get_player_recharge_data{
					is_first_recharge = IsFirstRecharge, 
					tehui_list = THList1, 
					recharged_list = RList,
					now_super_recharge_id = NSRechargeId,
					is_get_available = IsGetAvaiable,
					super_left_days = LeftDay
				},
	?PRINT("s2c_get_player_recharge_data PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 15000, RetRecord, Seq),
	{ok, Status};

% %% 15002 测试充值
% handle_cmd(15002, Seq, Status, Data) ->
% 	?PRINT("c2s_test_recharge PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
% 	[RechargeId] = Data,
%     Status1 = lib_recharge:test_recharge(Status, RechargeId),
% 	{ok, Status1};

%% 15003 领取超值礼包每日奖励
handle_cmd(15003, Seq, Status, Data) ->
	?PRINT("c2s_get_daily_super_award PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
    {RetCode, NewStatus} = lib_recharge:get_daily_super_award(Status),

	RetRecord = #s2c_get_daily_super_award{
					ret_code = RetCode,
					player_id = Status#player.id
				},
	?PRINT("s2c_get_daily_super_award PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 15003, RetRecord, Seq),
	{ok, NewStatus};

%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_recharge no match"}.





