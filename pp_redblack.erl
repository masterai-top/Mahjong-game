%%%--------------------------------------
%%% @Module  : pp_redblack
%%% @Author  : xws
%%% @Created : 2017-2-25
%%% @Description:  红黑操作
%%%--------------------------------------
-module(pp_redblack).
-include("common.hrl").
-include("record.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("log.hrl").
-include("debug.hrl").
-include("proto_30_redblack.hrl").
-include("redblack.hrl").

-compile(export_all).

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 30000 请求进入红黑大战
handle_cmd(30000, Seq, Status, _Data) ->
	?PRINT("c2s_enter_redblack PlayerId: ~p, ", [Status#player.id]),
	case lib_system_config:is_can_enter_game(Status#player.coin, "redblack") of
		true ->
			{RetCode, RoomId, NewStatus} = lib_player_redblack:enter_redblack(Status),
			RetRecord = #s2c_enter_redblack{
											ret_code = RetCode,
											room_id = RoomId
										   };
		_ ->
			NewStatus = Status,
			RetRecord = #s2c_enter_redblack{
											ret_code = 30000003,
											room_id = 0
										   }
	end,
	?PRINT("s2c_enter_redblack PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 30000, RetRecord, Seq),
	?PRINT("return attr: ~p", [NewStatus#player.table_attr]),
	{ok, NewStatus};

%% 30001 同步游戏押注信息
handle_cmd(30001, Seq, Status, _Data) ->
	%?PRINT("c2s_redblack_sync_yazhu_data PlayerId: ~p", [Status#player.id]),
	{AYList, SYList} = lib_player_redblack:get_yazhu_data(Status),
	PSYList = 
		[
			begin
				#proto_redblack_seat_yazhu{
					player_id = Id, 
					coin = Coin, 
					yazhu_list = [#proto_redblack_area_yazhu{area=Area, coin=Coin1} || {Area, Coin1} <- YazhuList]
				}
			end || {Id, Coin, YazhuList} <- SYList
		],
	RetRecord = #s2c_redblack_sync_yazhu_data{
					area_yazhu_list = [#proto_redblack_area_yazhu{area=Area, coin=Coin} || {Area, Coin} <- AYList],
					seat_yazhu_list = PSYList
				},
	%?PRINT("s2c_redblack_sync_yazhu_data RetRecord: ~p", [RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 30001, RetRecord, Seq);

%% 30002 玩家押注
handle_cmd(30002, Seq, Status, Data) ->
	%io:format("c2s_redblack_yazhu_coin PlayerId: ~p, Data: ~p~n", [Status#player.id, Data]),
	[YazhuList] = Data,
	{RetCode, NewStatus} = lib_player_redblack:yazhu_coin(Status, YazhuList),
	RetRecord = #s2c_redblack_yazhu_coin{
					ret_code = RetCode,
					yazhu_list = [#proto_redblack_area_yazhu{area=Area, coin=Coin} || {Area, Coin} <- YazhuList]
				},
	?PRINT("s2c_redblack_yazhu_coin PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 30002, RetRecord, Seq),
	{ok, NewStatus};

%% 30003 强制退出游戏
handle_cmd(30003, Seq, Status, Data) ->
	?PRINT("c2s_redblack_force_exit_redblack PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{NewStatus, RetCode} = lib_player_redblack:exit_room(Status),
	RetRecord = #s2c_redblack_force_exit_redblack{
					ret_code = RetCode
				},
	?PRINT("s2c_redblack_force_exit_redblack PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 30003, RetRecord, Seq),
	{ok, NewStatus};

%% 30011 发送红包
handle_cmd(30011, Seq, Status, Data) ->
	?PRINT("c2s_redblack_send_redpack PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[SendCoin] = Data,
	{RetCode, NewStatus} = lib_player_redblack:send_redpack(Status, SendCoin),
	RetRecord = #s2c_redblack_send_redpack{
					ret_code = RetCode,
					send_coin = SendCoin
				},
	?PRINT("s2c_redblack_force_exit_redblack PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 30011, RetRecord, Seq),
	{ok, NewStatus};

%% 30013 发送魔法表情
handle_cmd(30013, Seq, Status, Data) ->
	?PRINT("c2s_redblack_send_magic_look PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[MlId, TargetId] = Data,
	{RetCode, NewStatus} = lib_player_redblack:send_magic_look(Status, MlId, TargetId),
	RetRecord = #s2c_redblack_send_magic_look{
					ret_code = RetCode,
					ml_id = MlId,
					target_id = TargetId
				},
	?PRINT("s2c_redblack_send_magic_look PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 30013, RetRecord, Seq),
	{ok, NewStatus};


%% 30016 获取奖池数据
handle_cmd(30016, Seq, Status, Data) ->
	?PRINT("c2s_redblack_get_pot_data PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{PCValue, PRHList} = lib_player_redblack:get_pot_data(Status),
	PRHList1 =
		[
			begin
				#proto_redblack_pot_ret{
					total_coin = PotRet#redblack_pot_ret.total_coin,
					award_num = PotRet#redblack_pot_ret.award_num,
					first_player = redblack_util:pack_redblack_player(PotRet#redblack_pot_ret.first_player),
					reward_coin = PotRet#redblack_pot_ret.reward_coin,
					time = PotRet#redblack_pot_ret.time
				}
			end || PotRet <- PRHList
		],
	RetRecord = #s2c_redblack_get_pot_data{
					pot_coin_value = PCValue,
					pot_ret_hislist = PRHList1
				},
	io:format("s2c_redblack_get_pot_data PlayerId: ~p, RetRecord: ~p~n", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 30016, RetRecord, Seq);


%% 30017 获取排行榜
handle_cmd(30017, Seq, Status, Data) ->
	?PRINT("c2s_get_redblack_rank PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[RankType] = Data,
	{RankList, LastRankFirst, LastRank, PickAble, LastCoin, AwardCoin} = lib_player_redblack:get_rank(Status#player.id, RankType),
	Fun = 
		fun(RankData) ->
			case RankData of
				{Id, Coin, Nick, Facelook, Vip} ->
				    #proto_rank_player{
				    	player_id = Id,
				        nick = Nick,
				        facelook = Facelook,
				        vip = Vip,
				        coin = Coin,
				        time = 0    
				    };
				{Id, Coin, Nick, Facelook, Vip, Time} ->
				    #proto_rank_player{
				    	player_id = Id,
				        nick = Nick,
				        facelook = Facelook,
				        vip = Vip,
				        coin = Coin,
				        time = Time    
				    }
			end
		end,

	RankList1 = [Fun(RankData) || RankData <- RankList],
	LastRankFirst1 = Fun(LastRankFirst),
	RetRecord = #s2c_get_redblack_rank{
					rank_type = RankType,
					pick_able = PickAble,
					rank_list = RankList1,
					last_rank_first = LastRankFirst1,
					last_rank = LastRank,
					last_coin = LastCoin,
					award_coin = AwardCoin
				},
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 30017, RetRecord, Seq);

%% 30018 领取排行榜奖励
handle_cmd(30018, Seq, Status, Data) ->
	?PRINT("c2s_pick_redblack_rank_award PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[RankType] = Data,
	{NewStatus, RetCode} = lib_player_redblack:pick_rank(Status, RankType),
	
	RetRecord = #s2c_pick_redblack_rank_award{
					ret_code = RetCode,
					rank_type = RankType
				},
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 30018, RetRecord, Seq),
	{ok, NewStatus};


%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_goods no match"}.





