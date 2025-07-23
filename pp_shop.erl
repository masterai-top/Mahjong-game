%%%--------------------------------------
%%% @Module  : pp_shop
%%% @Author  : xws
%%% @Created : 2016-8-16
%%% @Description:  商城操作
%%%--------------------------------------
-module(pp_shop).
-include("common.hrl").
-include("record.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("log.hrl").
-include("debug.hrl").
-include("proto_14_shop.hrl").
-include("shop.hrl").


-compile(export_all).

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 获取商城物品
handle_cmd(14000, Seq, Status, Data) ->
	?PRINT("c2s_get_shop_goods PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[SGoodsType] = Data,
    SGoodsList = lib_shop:get_shop_goods_list_by_type(SGoodsType),
	PSGoodsList = 
		[
			begin
				#shop_goods{goods_id=GId, goods_num=Num} = ShopGoods,
				#proto_shop_goods{goods_type=SGoodsType, goods_id=GId, goods_num=Num}
			end || ShopGoods <- SGoodsList
		],
	RetRecord = #s2c_get_shop_goods{goods_type=SGoodsType, goods_list=PSGoodsList},
	?PRINT("s2c_get_shop_goods PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 14000, RetRecord, Seq),
	{ok, Status#player{exchange_red_point = 0}};

%% 兑换物品
handle_cmd(14001, Seq, Status, Data) ->
	%%?PRINT("c2s_exchange_shop_goods PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	io:format("~n==14001== PlayerId: ~p, Data: ~p~n", [Status#player.id, Data]),
	%%[SGoodsId, Num] = Data,
	%%{NStatus, RetCode, LeftNum, OrderId} = lib_shop:exchange_shop_goods(Status, SGoodsId, Num),
	[SGoodsId, GoodsList] = Data,
	{NStatus, RetCode, LeftNum, OrderId} = lib_shop:exchange_shop_goods(Status, SGoodsId, GoodsList),
	RetRecord = #s2c_exchange_shop_goods{
					ret_code = RetCode,
					goods_id = SGoodsId,
					goods_num = LeftNum,
					order_id = OrderId
				},
	?PRINT("s2c_exchange_shop_goods PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 14001, RetRecord, Seq),
	{ok, NStatus};

%% 获取兑换物品记录列表
handle_cmd(14002, Seq, Status, Data) ->
	?PRINT("c2s_get_exchange_his_list PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	BuyHisList = lib_shop:get_buy_history_list(Status#player.id),
	BuyHisList1 = [
					begin
						#exchange_order{order_id=OrderId, goods_id=GId, goods_num=GNum, status=OStatus, time=Time} = ExchangeOrder,
						#proto_shop_goods_his{order_id=OrderId, time=Time, goods_id=GId, goods_num=GNum, status=OStatus}
					end || ExchangeOrder <- BuyHisList
				  ],
	RetRecord = #s2c_get_exchange_his_list{
					his_list = BuyHisList1
				},
	?PRINT("s2c_get_exchange_his_list PlayerId: ~p", [Status#player.id]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 14002, RetRecord, Seq),
	{ok, Status};

%% 获取收货信息
handle_cmd(14003, Seq, Status, Data) ->
	?PRINT("c2s_get_receive_info PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[Name, PhoneNum, QqNum, Mail, Address] = lib_shop:get_player_receive_info(Status#player.id),
	RetRecord = #s2c_get_receive_info{
					name = Name,
					phone_num = PhoneNum,
					qq_num = QqNum,
					mail = Mail,
					address = Address
				},
	?PRINT("s2c_get_receive_info PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 14003, RetRecord, Seq),
	{ok, Status};

%% 修改收货信息
handle_cmd(14004, Seq, Status, Data) ->
	?PRINT("c2s_change_receive_info PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[Name, PhoneNum, QqNum, Mail, Address] = Data,
	Ret = lib_shop:change_player_receive_info(Status#player.id, Name, PhoneNum, QqNum, Mail, Address),
	RetRecord = #s2c_change_receive_info{
					ret_code = Ret,
					name = Name,
					phone_num = PhoneNum,
					qq_num = QqNum,
					mail = Mail,
					address = Address
				},
	?PRINT("s2c_change_receive_info PlayerId: ~p, RetRecord:~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 14004, RetRecord, Seq),
	{ok, Status};


%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_goods no match"}.





