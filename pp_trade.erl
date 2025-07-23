%%%--------------------------------------
%%% @Module  : pp_trade
%%% @Author  : xws
%%% @Created : 2016-9-7
%%% @Description:  商行操作
%%%--------------------------------------
-module(pp_trade).
-include("common.hrl").
-include("record.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("log.hrl").
-include("debug.hrl").
-include("proto_18_trade.hrl").
-include("trade.hrl").

-compile(export_all).

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 查找商行物品
handle_cmd(18000, Seq, Status, Data) ->
	?PRINT("c2s_find_trade_goods PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[ConditionList] = Data,
    TradeGoodsList = lib_player_trade:find_trade_goods(ConditionList),
    GoodsList1 = [pack_proto_trade_goods(TradeGoods) || TradeGoods <- TradeGoodsList],
	ConditionList1 = [#proto_find_condition{goods_id=GId, goods_quality=GQlty} || {GId, GQlty} <- ConditionList],
	%?PRINT("GoodsList1: ~p, ConditionList1: ~p", [TradeGoodsList, ConditionList1]),
	RetRecord = #s2c_find_trade_goods{
					condition_list = ConditionList1,
					trade_goods_list = GoodsList1
				},
	%?PRINT("s2c_find_trade_goods PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 18000, RetRecord, Seq);

%% 获取玩家商行数据
handle_cmd(18001, Seq, Status, Data) ->
	?PRINT("c2s_get_player_trade_data PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	TradeGoodsList = lib_player_trade:get_player_trade_data(Status#player.id),
	GoodsList1 = [pack_proto_trade_goods(TradeGoods) || TradeGoods <- TradeGoodsList],
	RetRecord = #s2c_get_player_trade_data{
					player_id = Status#player.id,
					trade_goods_list = GoodsList1
				},
	?PRINT("s2c_get_player_trade_data PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 18001, RetRecord, Seq);

%% 上架物品到商行
handle_cmd(18002, Seq, Status, Data) ->
	?PRINT("c2s_sell_trade_goods PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[GoodsId, GoodsNum, Price, Msg] = Data,
	{NewStatus, RetCode, TradeGoods} = lib_player_trade:sell_trade_goods(Status, GoodsId, GoodsNum, Price, Msg),
	TradeGoods1 = pack_proto_trade_goods(TradeGoods),
	RetRecord = #s2c_sell_trade_goods{
					ret_code = RetCode,
					trade_goods = TradeGoods1
				},
	?PRINT("s2c_sell_trade_goods PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 18002, RetRecord, Seq),
	{ok, NewStatus};

%% 下架物品到商行
handle_cmd(18003, Seq, Status, Data) ->
	?PRINT("c2s_unsell_trade_goods PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[TradeUId] = Data,
	RetCode = lib_player_trade:unsell_trade_goods(Status, TradeUId),
	RetRecord = #s2c_unsell_trade_goods{
					ret_code = RetCode,
					trade_uid = TradeUId
				},
	?PRINT("s2c_unsell_trade_goods PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 18003, RetRecord, Seq);

%% 购买物品
handle_cmd(18004, Seq, Status, Data) ->
	?PRINT("c2s_buy_trade_goods PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[TradeUId] = Data,
	{NewStatus, RetCode} = lib_player_trade:buy_trade_goods(Status, TradeUId),
	RetRecord = #s2c_buy_trade_goods{
					ret_code = RetCode,
					trade_uid = TradeUId
				},
	?PRINT("s2c_buy_trade_goods PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 18004, RetRecord, Seq),
	{ok, NewStatus};

%% 回收过期商行物品
handle_cmd(18006, Seq, Status, Data) ->
	?PRINT("c2s_recover_trade_goods PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[TradeUId] = Data,
	RetCode = lib_player_trade:recover_trade_goods(Status, TradeUId),
	RetRecord = #s2c_recover_trade_goods{
					ret_code = RetCode,
					trade_uid = TradeUId
				},
	?PRINT("s2c_recover_trade_goods PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 18006, RetRecord, Seq);

%% 领取已出售的物品
handle_cmd(18007, Seq, Status, Data) ->
	?PRINT("c2s_pickup_traded_goods PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[TradeUId] = Data,
	{NewStatus, RetCode} = lib_player_trade:pickup_traded_goods(Status, TradeUId),
	RetRecord = #s2c_pickup_traded_goods{
					ret_code = RetCode,
					trade_uid = TradeUId
				},
	?PRINT("s2c_pickup_traded_goods PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 18007, RetRecord, Seq),
	{ok, NewStatus};

%% 赠送玩家物品
handle_cmd(18008, Seq, Status, Data) ->
	?PRINT("c2s_give_player_goods PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[GoodsUId, Num, TargetSpecialId] = Data,
	{RetCode, NewStatus} = lib_player_trade:give_player_goods(Status, GoodsUId, Num, TargetSpecialId),
	RetRecord = #s2c_give_player_goods{
					ret_code = RetCode,
					goods_uid = GoodsUId,
					num = Num,
					target_special_id = TargetSpecialId
				},
	?PRINT("s2c_give_player_goods PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 18008, RetRecord, Seq),
	{ok, NewStatus};

%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_goods no match"}.

%%.
pack_proto_trade_goods(TradeGoods) ->
	#proto_trade_goods{
		trade_uid = TradeGoods#trade_goods.trade_uid,
		status = TradeGoods#trade_goods.status,
		goods_type = TradeGoods#trade_goods.goods_type,
		goods_id = TradeGoods#trade_goods.goods_id,
		goods_quality = TradeGoods#trade_goods.goods_quality,
		goods_num = TradeGoods#trade_goods.goods_num,
		goods_price = TradeGoods#trade_goods.price,
		timeout = TradeGoods#trade_goods.timeout,
		seller_id = TradeGoods#trade_goods.seller_id,
		seller_name = TradeGoods#trade_goods.seller_name,
		seller_msg = TradeGoods#trade_goods.seller_msg
	}.





