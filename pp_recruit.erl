%%%--------------------------------------
%%% @Module  : pp_recruit
%%% @Author  : xws
%%% @Created : 2016-11-2
%%% @Description:  招募操作
%%%--------------------------------------
-module(pp_recruit).
-include("common.hrl").
-include("record.hrl").
-include("log.hrl").
-include("debug.hrl").
-include("proto_26_recruit.hrl").
-include("recruit.hrl").


-compile(export_all).

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 26000 请求玩家招募数据
handle_cmd(26000, Seq, Status, Data) ->
	?PRINT("c2s_get_recruit_data PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
    PlayerRecruit = lib_recruit:get_recruit_data(Status#player.id),
	RetRecord = 
		#s2c_get_recruit_data{
			player_id = Status#player.id,
			recruit_num = PlayerRecruit#player_recruit.recruit_num,
			binding_player_id = PlayerRecruit#player_recruit.binding_player_id,
			award1_pick_times = PlayerRecruit#player_recruit.award1_pick_times,
			award1_picked_times = PlayerRecruit#player_recruit.award1_picked_times,
			award2_pick_times = PlayerRecruit#player_recruit.award2_pick_times,
			award2_picked_times = PlayerRecruit#player_recruit.award2_picked_times,
			award3_pick_coin = PlayerRecruit#player_recruit.award3_pick_coin
		},
	?PRINT("s2c_get_recruit_data PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 26000, RetRecord, Seq),
	{ok, Status#player{activity_red_point = 0}};

%% 26001 绑定玩家
handle_cmd(26001, Seq, Status, Data) ->
	?PRINT("c2s_recruit_binding_player PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[TargetSpecialId] = Data,
	{RetCode, NewStatus} = lib_recruit:binding_player(Status, TargetSpecialId),
	RetRecord = #s2c_recruit_binding_player{
					ret_code = RetCode,
					target_special_id = TargetSpecialId
				},
	?PRINT("s2c_recruit_binding_player PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 26001, RetRecord, Seq),
	{ok, NewStatus};

%% 26002 领取招募奖励
handle_cmd(26002, Seq, Status, Data) ->
	io:format("c2s_get_recruit_award PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[AwardType] = Data,
	{RetCode, NewStatus} = lib_recruit:get_award(Status, AwardType),
	RetRecord = #s2c_get_recruit_award{
					ret_code = RetCode,
					award_type = AwardType
				},
	?PRINT("s2c_get_recruit_award PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 26002, RetRecord, Seq),
	{ok, NewStatus};

handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_goods no match"}.





