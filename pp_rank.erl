%%%--------------------------------------
%%% @Module  : pp_shop
%%% @Author  : xws
%%% @Created : 2017-2-21
%%% @Description:  排行榜
%%%--------------------------------------
-module(pp_rank).

%%--------------------------------------
%% Include files
%%--------------------------------------
-include("common.hrl").
-include("record.hrl").
-include("rank.hrl").
-include("shop.hrl").
-include("proto_14_shop.hrl").
-include("proto_29_rank.hrl").

%%--------------------------------------
%% Exported Functions
%%--------------------------------------
-compile(export_all).

%% API Functions
handle(Cmd, Seq, Status, Data) ->
    handle_cmd(Cmd, Seq, Status, Data).

%% 29000 c2s获取兑换排行榜
handle_cmd(29000, Seq, Status, _Data) ->
	?PRINT("c2s_get_exchange_rank PlayerId: ~p, Data: ~p", [Status#player.id, _Data]),
    RankList = lib_player_rank:get_rank_list(?RANK_EXCHANGE),
	Fun = 
		fun({Id, ExchangeValue, Nick, Vip, Facelook, Coin, Gold, WinNum, LoseNum, BuyHis}, {RetList, Index}) ->
			PackBuyHis = 
				[
					begin
						#exchange_order{order_id=OrderId, goods_id=GId, goods_num=GNum, status=OStatus, time=Time} = ExchangeOrder,
						#proto_shop_goods_his{order_id=OrderId, time=Time, goods_id=GId, goods_num=GNum, status=OStatus}
					end || ExchangeOrder <- BuyHis
				],
			{
				[#proto_exchange_rank{
					rank = Index,
					player_id = Id,
					special_id = lib_player:player_id_to_special_id(Id),
					nick = Nick,
					vip = Vip,
					facelook = Facelook, 
					coin = Coin,
					gold = Gold,
					win_num = WinNum,
					lose_num = LoseNum,
					exchange_value = ExchangeValue,
					exchange_his_list = PackBuyHis
				}|RetList], 
				Index + 1
			}
		end,
	{PackRankList, _} = lists:foldl(Fun, {[], 1}, RankList),
	PackRankList1 = lists:reverse(PackRankList),
	RetRecord = #s2c_get_exchange_rank{rank_list=PackRankList1},
	?PRINT("s2c_get_exchange_rank PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 29000, RetRecord, Seq);

%% 29001 c2s获取历史兑换记录
handle_cmd(29001, Seq, Status, _Data) ->
	?PRINT("c2s_get_exchange_his PlayerId: ~p, Data: ~p", [Status#player.id, _Data]),
    {_, HisList} = util:center_node_call(mod_shop, get_order_his_cache, []),
   	PackHisList = 
   		[
   			begin
   				#full_exchange_order{
							player_id = PlayerId,
							nick = Nick,
							vip = Vip,
							exchange_order = ExchangeOrder
				} = FullExchangeOrder,
				#exchange_order{order_id=OrderId, goods_id=GId, goods_num=GNum, status=OStatus, time=Time} = ExchangeOrder,
				#proto_full_shop_goods_his{
					player_id = PlayerId,
					nick = Nick,
					vip = Vip,
					order_id = OrderId, 
					time = Time, 
					goods_id=GId, 
					goods_num=GNum, 
					status=OStatus
				}
			end || FullExchangeOrder <- HisList
   		],
	RetRecord = #s2c_get_exchange_his{exchange_his_list=PackHisList},
	?PRINT("s2c_get_exchange_rank PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 29001, RetRecord, Seq);

%% 29002 c2s获取金币排行榜
handle_cmd(29002, Seq, Status, _Data) ->
	?PRINT("c2s_get_coin_rank PlayerId: ~p, Data: ~p", [Status#player.id, _Data]),
	RankList = lib_player_rank:get_rank_list(?RANK_COIN),
	Fun = 
		fun({Id, Coin, Nick, Vip, Facelook}, {RetList, Index}) ->
			{
				[#proto_coin_rank{
					rank = Index,
					player_id = Id,
					nick = Nick,
					vip = Vip,
					facelook = Facelook, 
					coin = Coin
				}|RetList], 
				Index + 1
			}
		end,
	{PackRankList, _} = lists:foldl(Fun, {[], 1}, RankList),
	PackRankList1 = lists:reverse(PackRankList),
	RetRecord = #s2c_get_coin_rank{rank_list=PackRankList1},
	?PRINT("s2c_get_coin_rank PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 29002, RetRecord, Seq);

%% 29003 c2s获取胜率排行榜
handle_cmd(29003, Seq, Status, _Data) ->
	?PRINT("c2s_get_win_rate_rank PlayerId: ~p, Data: ~p", [Status#player.id, _Data]),
	RankList = lib_player_rank:get_rank_list(?RANK_WIN_RATE),
	Fun = 
		fun({Id, WinRate, Nick, Vip, Facelook}, {RetList, Index}) ->
			{
				[#proto_win_rate_rank{
					rank = Index,
					player_id = Id,
					nick = Nick,
					vip = Vip,
					facelook = Facelook, 
					win_rate = io_lib:format("~.3f", [WinRate])
				}|RetList], 
				Index + 1
			}
		end,
	{PackRankList, _} = lists:foldl(Fun, {[], 1}, RankList),
	PackRankList1 = lists:reverse(PackRankList),
	RetRecord = #s2c_get_win_rate_rank{rank_list=PackRankList1},
	?PRINT("s2c_get_win_rate_rank PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 29003, RetRecord, Seq);

%% 29004 领取排行榜奖励
handle_cmd(29004, Seq, Status, Data) ->
	?PRINT("c2s_get_rank_award PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[RankType] = Data,
	{RetCode, NStatus} = lib_player_rank:pick_rank_award(Status, RankType),
	RetRecord = 
		#s2c_get_rank_award{
			ret_code = RetCode,
			rank_type = RankType
		},
	?PRINT("s2c_get_rank_award PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 29004, RetRecord, Seq);

%% 29005 获取玩家排行榜数据
handle_cmd(29005, Seq, Status, Data) ->
	?PRINT("c2s_get_player_rank_data PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	{PickedList} = lib_player_rank:get_player_rank_data(Status#player.id),
	RetRecord = 
		#s2c_get_player_rank_data{
			picked_list = PickedList
		},
	?PRINT("s2c_get_player_rank_data PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 29005, RetRecord, Seq);


%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_rank no match"}.
