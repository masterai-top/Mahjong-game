%%%--------------------------------------
%%% @Module  : pp_share_shop
%%% @Author  : xws
%%% @Created : 2017-1-2
%%% @Description:  商城操作
%%%--------------------------------------
-module(pp_share_shop).
-include("common.hrl").
-include("record.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("log.hrl").
-include("debug.hrl").
-include("proto_27_share_shop.hrl").
-include("share_shop.hrl").
-include("record/data_share_shop_goods_record.hrl").


-compile(export_all).

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 获取商城物品
handle_cmd(27000, Seq, Status, Data) ->
	?PRINT("c2s_get_share_shop_goods PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[SGoodsType] = Data,
    SGoodsList = lib_player_share_shop:get_shop_goods_list_by_type(SGoodsType),
    Fun = 
    	fun(#share_shop_goods{goods_id=GId, goods_num=Num, state=State}, RetList) when Num =/= 0 ->
%%     		io:format("shop good num: ~p~n", [Num]),
    		{SaleUpTime, SaleDownTime} = lib_player_share_shop:get_shop_goods_time(GId),
			[#proto_share_shop_goods{
				goods_type = SGoodsType, 
				goods_id = GId, 
				goods_num = Num, 
				state = State,
				sale_up_time = SaleUpTime,
				sale_down_time = SaleDownTime
			} | RetList];
			(_, RetList) ->
				RetList
		end,
	PSGoodsList = lists:foldl(Fun, [], SGoodsList),
	RetRecord = #s2c_get_share_shop_goods{goods_type=SGoodsType, goods_list=PSGoodsList},
	?PRINT("s2c_get_share_shop_goods PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 27000, RetRecord, Seq);

%% 兑换物品
handle_cmd(27001, Seq, Status, Data) ->
	?PRINT("c2s_buy_share_shop_goods PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	[SGoodsId, Num] = Data,
	{NStatus, RetCode, LeftNum} = lib_player_share_shop:buy_shop_goods(Status, SGoodsId, Num),
	RetRecord = #s2c_buy_share_shop_goods{
					ret_code = RetCode,
					goods_id = SGoodsId,
					goods_num = LeftNum
				},
	?PRINT("s2c_buy_share_shop_goods PlayerId: ~p, RetRecord: ~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 27001, RetRecord, Seq),
	{ok, NStatus};

%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_goods no match"}.





