%%%--------------------------------------
%%% @Module  : pp_goldniu
%%% @Author  : xws
%%% @Created : 607-5-10
%%% @Description:  炸金牛操作
%%%--------------------------------------
-module(pp_goldniu).
-include("common.hrl").
-include("record.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("log.hrl").
-include("debug.hrl").
-include("proto_60_goldniu.hrl").
-include("douniu.hrl").

-compile(export_all).

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 60000 请求进入金牛
handle_cmd(60000, Seq, Status, Data) ->
	io:format("c2s_enter_goldniu PlayerId: ~p, Data,~p~n", [Status#player.id, Data]),
	[RoomType] = Data,
    {RetCode, RoomId, NewStatus} = lib_player_goldniu:enter_goldniu(Status, RoomType),
    RetRecord = #s2c_enter_goldniu{
					ret_code = RetCode,
					room_type = RoomType
				},
	?PRINT("s2c_enter_goldniu PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 60000, RetRecord, Seq),
	?PRINT("return attr: ~p", [NewStatus#player.table_attr]),
	{ok, NewStatus};

%% 60001 离开斗牛
handle_cmd(60001, Seq, Status, _Data) ->
	?PRINT("c2s_leave_goldniu PlayerId: ~p, Data: ~p~n", [Status#player.id, _Data]),
	{RetCode, NewStatus} = lib_player_goldniu:leave_goldniu(Status),
	RetRecord = #s2c_leave_goldniu{
					ret_code = RetCode
				},
	?PRINT("s2c_leave_goldniu PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 60001, RetRecord, Seq),
	{ok, NewStatus};

%% 60002 玩家准备
handle_cmd(60002, Seq, Status, Data) ->
	?PRINT("c2s_goldniu_prepare PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	RetCode = lib_player_goldniu:prepare(Status),
	RetRecord = #s2c_goldniu_prepare{
					ret_code = RetCode
				},
	?PRINT("s2c_goldniu_prepare PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 60002, RetRecord, Seq);

%% 60003 玩家跟注
handle_cmd(60003, Seq, Status, Data) ->
	?PRINT("c2s_goldniu_gocall PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[CallCoin] = Data,
	{RetCode, NewStatus} = lib_player_goldniu:gocall_coin(Status, CallCoin),
	RetRecord = #s2c_goldniu_gocall{
					ret_code = RetCode,
					coin = CallCoin
				},
	?PRINT("s2c_goldniu_gocall PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 60003, RetRecord, Seq),
	{ok, NewStatus};

%% 60004 玩家弃牌
handle_cmd(60004, Seq, Status, Data) ->
	?PRINT("c2s_goldniu_giveup PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	RetCode = lib_player_goldniu:giveup_card(Status),
	RetRecord = #s2c_goldniu_giveup{
					ret_code = RetCode
				},
	?PRINT("s2c_goldniu_giveup PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 60004, RetRecord, Seq);


%% 60005 玩家比牌
handle_cmd(60005, Seq, Status, Data) ->
	?PRINT("c2s_goldniu_compare PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[TargetId] = Data,
	{RetCode, NewStatus} = lib_player_goldniu:compare_card(Status, TargetId),

	RetRecord = #s2c_goldniu_compare{
					ret_code = RetCode,
					target_id = TargetId
				},
	io:format("s2c_goldniu_compare PlayerId: ~p, RetRecord: ~p~n", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 60005, RetRecord, Seq),
	{ok, NewStatus};


%% 60006 玩家看牌
handle_cmd(60006, Seq, Status, Data) ->
	?PRINT("c2s_goldniu_look PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{RetCode, HandCards} = lib_player_goldniu:look_card(Status),

	HandCards1 = 
		case HandCards of
			undefined ->
				#proto_hand_goldcards{
					cards = [],
					value_cards = [],
					niu = 0
				};
			{Cards, {Niu, _, ValueCards}} ->
				Cards1 = [#proto_goldcard{size=Size, color=Color} || {Size, Color, _, _} <- Cards],
				ValueCards1 = [#proto_goldcard{size=Size, color=Color} || {Size, Color, _, _} <- ValueCards],
				#proto_hand_goldcards{
					cards = Cards1,
					value_cards = ValueCards1,
					niu = Niu
				}
		end,

	RetRecord = #s2c_goldniu_look{
					ret_code = RetCode,
					hand_cards = HandCards1
				},
	io:format("s2c_goldniu_look PlayerId: ~p, RetRecord: ~p~n", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 60006, RetRecord, Seq);

%% 60007 玩家亮牌
handle_cmd(60007, Seq, Status, Data) ->
	?PRINT("c2s_goldniu_show PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[RankType] = Data,
	RetCode = lib_player_goldniu:show_card(Status),
	
	RetRecord = #s2c_goldniu_show{
					ret_code = RetCode
				},
	io:format("s2c_goldniu_show PlayerId: ~p, RetRecord: ~p~n", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 60007, RetRecord, Seq);

%% 60018 玩家allin
handle_cmd(60018, Seq, Status, Data) ->
	?PRINT("c2s_goldniu_allin PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[AllinCoin] = Data,
	{RetCode, NewStatus} = lib_player_goldniu:allin(Status, AllinCoin),
	
	RetRecord = #s2c_goldniu_allin{
					ret_code = RetCode,
					coin = AllinCoin
				},
	io:format("s2c_goldniu_allin PlayerId: ~p, RetRecord: ~p~n", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 60018, RetRecord, Seq),
	{ok, NewStatus};

%% 60019 玩家比牌
handle_cmd(60019, Seq, Status, Data) ->
	?PRINT("c2s_goldniu_begin_compare PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	RetCode = lib_player_goldniu:begin_compare(Status),
	
	RetRecord = #s2c_goldniu_begin_compare{
					ret_code = RetCode,
					player_id = Status#player.id
				},
	io:format("s2c_goldniu_begin_compare PlayerId: ~p, RetRecord: ~p~n", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 60019, RetRecord, Seq);


%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_goods no match"}.





