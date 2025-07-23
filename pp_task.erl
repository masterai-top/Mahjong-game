%%%--------------------------------------
%%% @Module  : pp_task
%%% @Author  : xws
%%% @Created : 2016-10-10
%%% @Description:  
%%%--------------------------------------
-module(pp_task).
-include("common.hrl").
-include("record.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("log.hrl").
-include("debug.hrl").
-include("proto_23_task.hrl").
-include("task.hrl").

-compile(export_all).

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 23000 请求玩家任务数据
handle_cmd(23000, Seq, Status, Data) ->
	?PRINT("c2s_get_task_data PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
    {DTList, CTList, NLiveness, PLIList, NDTask, IDOn, NTList, CycTaskL} = lib_task:get_player_task_data(Status#player.id),
    DTList1 = [pack_task(Task) || Task <- DTList],
    CTList1 = [pack_task(Task) || Task <- CTList],
    NTList1 = [pack_task(Task) || Task <- NTList],
    NDTask1 = pack_task(NDTask),
	CycTaskL1 = [pack_task(Task) || Task <- CycTaskL],
	RetRecord = #s2c_get_task_data{
					daily_task_list = DTList1, 
					challenge_task_list = CTList1,
					now_liveness = NLiveness,
					picked_liveness_id_list = PLIList,
					dayN_task = NDTask1,
					is_dayN_on = IDOn,
					newbie_task_list = NTList1,
					cyclic_task_list = CycTaskL1
				},
	?PRINT("s2c_get_task_data PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 23000, RetRecord, Seq);

%% 23002 领取任务奖励
handle_cmd(23002, Seq, Status, Data) ->
	?PRINT("c2s_pick_task_award PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[TaskId] = Data,
	{RetCode, NewStatus, RewardList} = lib_task:pick_task_award(Status, TaskId),
	RetRecord = #s2c_pick_task_award{
					ret_code = RetCode,
					task_id = TaskId,
					goods_list = [#reward_list{gtid = GTID, num = Num} || {GTID, Num} <- RewardList]
				},
	?PRINT("s2c_pick_task_award PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 23002, RetRecord, Seq),
	{ok, NewStatus};

%% 23003 刷新当前挑战任务
handle_cmd(23003, Seq, Status, Data) ->
	?PRINT("c2s_refresh_now_challenge_task PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{NewStatus, RetCode} = lib_task:refresh_now_challege_task(Status),
	RetRecord = #s2c_refresh_now_challenge_task{
					ret_code = RetCode
				},
	?PRINT("s2c_refresh_now_challenge_task PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 23003, RetRecord, Seq),
	{ok, NewStatus};

%% 23004 领取N日任务额外奖励
handle_cmd(23004, Seq, Status, Data) ->
	?PRINT("c2s_get_dayN_task_award PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{NewStatus, RetCode} = lib_task:get_dayN_task_award(Status),
	RetRecord = #s2c_get_dayN_task_award{
					ret_code = RetCode
				},
	?PRINT("s2c_get_dayN_task_award PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 23004, RetRecord, Seq),
	{ok, NewStatus};

%% 23005 领取活跃度奖励
handle_cmd(23005, Seq, Status, Data) ->
	?PRINT("c2s_get_liveness_award PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[LivenessId] = Data,
	{NewStatus, RetCode} = lib_task:get_liveness_award(Status, LivenessId),
	RetRecord = #s2c_get_liveness_award{
					ret_code = RetCode,
					liveness_id = LivenessId
				},
	?PRINT("s2c_get_liveness_award PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 23005, RetRecord, Seq),
	{ok, NewStatus};

%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_goods no match"}.

%%.
pack_task(Task) ->
	#proto_task_state{
		task_id = Task#task.task_id,
		times = Task#task.times,
		state = Task#task.state
	}.





