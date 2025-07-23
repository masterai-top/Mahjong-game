%%%--------------------------------------
%%% @Module  : pp_sign
%%% @Author  : xws
%%% @Created : 2016-10-19
%%% @Description:  签到操作
%%%--------------------------------------
-module(pp_sign).
-include("common.hrl").
-include("record.hrl").
-include("log.hrl").
-include("debug.hrl").
-include("proto_24_sign.hrl").
-include("sign.hrl").


-compile(export_all).

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 24000 请求玩家签到数据
handle_cmd(24000, Seq, Status, Data) ->
	?PRINT("c2s_get_sign_data PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
    PlayerSign = lib_sign:get_player_sign_data(Status#player.id),
    IsSign = ?IF(util:now_days() >= PlayerSign#player_sign1.next_sign_day, ?YES, ?NO),
	RetRecord = #s2c_get_sign_data{
					acc_sign_days = PlayerSign#player_sign1.acc_sign_days,
					is_sign = IsSign
				},
	?PRINT("s2c_get_sign_data PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 24000, RetRecord, Seq);

%% 24001 玩家签到
handle_cmd(24001, Seq, Status, Data) ->
	?PRINT("c2s_sign PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{RetCode, NStatus, ExtraAward} = lib_sign:sign(Status),
	ExtraAward1 = [#proto_res{resurce_id=Id, num=Num} || {Id, Num} <- ExtraAward],
	RetRecord = #s2c_sign{
					ret_code = RetCode,
					extra_res_list = ExtraAward1
				},
	?PRINT("s2c_sign PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 24001, RetRecord, Seq),
	{ok, NStatus};

%% 1.41版本之前的签到系统代码, 暂时废除
% %% 24000 请求玩家签到数据
% handle_cmd(24000, Seq, Status, Data) ->
% 	?PRINT("c2s_get_sign_data PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
%     PlayerSign = lib_sign:get_player_sign_data(Status#player.id),
% 	StateList = [#proto_day_state{index_day=IDay, state=State} || {IDay, State} <- PlayerSign#player_sign.state_list],
% 	RetRecord = #s2c_get_sign_data{
% 					acc_sign_days = PlayerSign#player_sign.acc_sign_days,
% 					acc_resign_days = PlayerSign#player_sign.acc_resign_days,
% 					state_list = StateList
% 				},
% 	?PRINT("s2c_get_sign_data PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
% 	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 24000, RetRecord, Seq);

% %% 24001 玩家签到
% handle_cmd(24001, Seq, Status, Data) ->
% 	?PRINT("c2s_sign PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
% 	{RetCode, NStatus} = lib_sign:sign(Status),
% 	RetRecord = #s2c_sign{
% 					ret_code = RetCode
% 				},
% 	?PRINT("s2c_sign PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
% 	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 24001, RetRecord, Seq),
% 	{ok, NStatus};

% %% 24002 玩家补签
% handle_cmd(24002, Seq, Status, Data) ->
% 	?PRINT("c2s_resign PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
% 	[IndexDay] = Data,
% 	{RetCode, NStatus} = lib_sign:resign(Status, IndexDay),
% 	RetRecord = #s2c_resign{
% 					ret_code = RetCode,
% 					index_day = IndexDay
% 				},
% 	?PRINT("s2c_resign PlayerId: ~p, RetCode: ~p", [Status#player.id, RetCode]),
% 	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 24002, RetRecord, Seq),
% 	{ok, NStatus};

%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_goods no match"}.





