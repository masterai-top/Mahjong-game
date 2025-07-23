%%%--------------------------------------
%%% @Module  : pp_hundred_douniu
%%% @Author  : xws
%%% @Created : 2016-9-22
%%% @Description:  商行操作
%%%--------------------------------------
-module(pp_hundred_douniu).
-include("common.hrl").
-include("record.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("log.hrl").
-include("debug.hrl").
-include("proto_22_hundred_douniu.hrl").
-include("hundred_douniu.hrl").

-compile(export_all).

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 22000 请求进入百人牛牛
handle_cmd(22000, Seq, Status, _Data) ->
	?PRINT("c2s_enter_hundred_douniu PlayerId: ~p, ", [Status#player.id]),
	case lib_system_config:is_can_enter_game(Status#player.coin, "hundred_douniu") of
		true ->
			{RetCode, RoomId, NewStatus} = lib_player_hundred_douniu:enter_hundred_douniu(Status),
			RetRecord = #s2c_enter_hundred_douniu{
												  ret_code = RetCode,
												  room_id = RoomId
												 };
		_ ->
			NewStatus = Status,
			RetRecord = #s2c_enter_hundred_douniu{
												  ret_code = 22000003,  %% 金币不足，不能进入游戏
												  room_id = 0
												 }
	end,
	?PRINT("s2c_enter_hundred_douniu PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 22000, RetRecord, Seq),
	?PRINT("return attr: ~p", [NewStatus#player.table_attr]),
	{ok, NewStatus};

%% 22001 同步游戏押注信息
handle_cmd(22001, Seq, Status, _Data) ->
	%?PRINT("c2s_sync_yazhu_data PlayerId: ~p", [Status#player.id]),
	{AYList, SYList} = lib_player_hundred_douniu:get_yazhu_data(Status),
	PSYList = 
		[
			begin
				#proto_seat_yazhu{
					player_id = Id, 
					coin = Coin, 
					yazhu_list = [#proto_area_yazhu{area=Area, coin=Coin1} || {Area, Coin1} <- YazhuList]
				}
			end || {Id, Coin, YazhuList} <- SYList
		],
	RetRecord = #s2c_sync_yazhu_data{
					area_yazhu_list = [#proto_area_yazhu{area=Area, coin=Coin} || {Area, Coin} <- AYList],
					seat_yazhu_list = PSYList
				},

	%?PRINT("s2c_sync_yazhu_data PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 22001, RetRecord, Seq);

%% 22002 获取上庄列表
handle_cmd(22002, Seq, Status, _Data) ->
	?PRINT("c2s_get_banker_wait_list PlayerId: ~p", [Status#player.id]),
	BKWList = lib_player_hundred_douniu:get_banker_wait_list(Status),
	RetRecord = #s2c_get_banker_wait_list{
					banker_wait_list = BKWList
				},
	?PRINT("s2c_get_banker_wait_list PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 22002, RetRecord, Seq);

%% 22003 玩家坐下
handle_cmd(22003, Seq, Status, Data) ->
	?PRINT("c2s_sit_on PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[RoomId, Pos] = Data,
	RetCode = lib_player_hundred_douniu:sit_on(Status, Pos),
	RetRecord = #s2c_sit_on{
					ret_code = RetCode,
					room_id = RoomId,
					pos = Pos
				},
	?PRINT("s2c_sit_on PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 22003, RetRecord, Seq);

%% 22004 玩家起来
handle_cmd(22004, Seq, Status, Data) ->
	?PRINT("c2s_stand_up PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[RoomId] = Data,
	RetCode = lib_player_hundred_douniu:stand_up(Status),
	RetRecord = #s2c_stand_up{
					ret_code = RetCode,
					room_id = RoomId
				},
	?PRINT("s2c_stand_up PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 22004, RetRecord, Seq);

%% 22005 玩家押注
handle_cmd(22005, Seq, Status, Data) ->
%	?PRINT("c2s_yazhu_coin PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[YazhuList] = Data,
	{RetCode, NewStatus} = lib_player_hundred_douniu:yazhu_coin(Status, YazhuList),
	RetRecord = #s2c_yazhu_coin{
					ret_code = RetCode,
					yazhu_list = [#proto_area_yazhu{area=Area, coin=Coin} || {Area, Coin} <- YazhuList]
				},
%	?PRINT("s2c_yazhu_coin PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 22005, RetRecord, Seq),
	{ok, NewStatus};

%% 22006 上庄
handle_cmd(22006, Seq, Status, Data) ->
	?PRINT("c2s_go_banker PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[RoomId] = Data,
	RetCode = lib_player_hundred_douniu:go_banker(Status),
	RetRecord = #s2c_go_banker{
					ret_code = RetCode,
					room_id = RoomId
				},
	?PRINT("s2c_go_banker PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 22006, RetRecord, Seq);

%% 22007 下庄
handle_cmd(22007, Seq, Status, Data) ->
	?PRINT("c2s_out_banker PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[RoomId] = Data,
	{RetCode, NewStatus} = lib_player_hundred_douniu:out_banker(Status),
	RetRecord = #s2c_out_banker{
					ret_code = RetCode,
					room_id = RoomId
				},
	?PRINT("s2c_out_banker PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 22007, RetRecord, Seq),
	{ok, NewStatus};

%% 22008 强制退出游戏
handle_cmd(22008, _Seq, Status, Data) ->
	?PRINT("c2s_out_banker PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	NewStatus = lib_player_hundred_douniu:exit_room(Status),
	{ok, NewStatus};

%% 22019 获取玩家区域押注数据
handle_cmd(22019, Seq, Status, Data) ->
	?PRINT("c2s_get_areas_stake_coin PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	YazhuList = lib_player_hundred_douniu:get_yazhu_list(Status),
	EnYazhuList = [#proto_area_yazhu{area=Area, coin=Coin} || {Area, Coin} <- YazhuList],
	RetRecord = #s2c_get_areas_stake_coin{
					player_id = Status#player.id,
					yazhu_list = EnYazhuList
				},
	?PRINT("s2c_get_areas_stake_coin PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 22019, RetRecord, Seq);

%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_goods no match"}.





