%%%--------------------------------------
%%% @Module  : pp_goods
%%% @Author  : 
%%% @Created : 
%%% @Description:  物品操作
%%%--------------------------------------
-module(pp_goods).
-include("common.hrl").
-include("record.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("log.hrl").
-include("debug.hrl").
-include("proto/proto_13_goods.hrl").


-compile(export_all).

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 查询指定位置物品Location = 0 表示背包
%% GoodsList = [{goods_record, GTId, Id, Num, Type, SubType, Location, Quality} || 
%% 				 #goods{gtid=GTId, id=Id, num=Num, location = Location, type = Type, subtype = SubType, quality = Quality} <- Data],
handle_cmd(13000, Seq, Status, Data) ->
	[Location] = Data,
	List = lib_goods:get_goods_list(Status, Location),
	{NewStatus, NewList} = lib_goods:clear_out_of_validity_goods(Status, List),
	Fun = fun(Goods) ->
				  {goods_record, Goods#goods.gtid, Goods#goods.id, Goods#goods.num, Goods#goods.type, Goods#goods.subtype, Goods#goods.location, Goods#goods.quality}
		  end,
	GoodsList = lists:map(Fun, NewList),
	lib_send:pack_and_send(NewStatus#player.other#player_other.pid_send, 13000, {s2c_goods_info, GoodsList}, Seq),
	{ok, NewStatus};

%% %% 使用物品
%% handle_cmd(13001, Seq, Status, Data) ->
%% 	[Goods_id, Goods_num] = Data,
%%     RetCode = lib_goods:use_goods(Status, Goods_id, Goods_num),
%% 	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 13000, {s2c_use_goods, RetCode}, Seq);

%% 开宝箱
handle_cmd(13002, Seq, Status, [GoodsId, MinQualityGTID, Num]) ->
	case goods_util:open_treasure_box(Status, [GoodsId, MinQualityGTID, Num]) of
		{ok, NewPlayer, EffectType, GoodsRecord, _UsePrice, UpdateGoods} ->
			%% 完成开宝箱的任务
			lib_task_event:open_chest(NewPlayer#player.id),
			%% 开启新功能
			NewPS = lib_system_config:open_lottery_system(NewPlayer),
			_OpenSystemList = NewPS#player.open_system_list,
			ReturnInfo = {s2c_open_treasure_box, ?FLAG_SUCC, EffectType, GoodsRecord},
			UpdateGoodsRecord = {s2c_goods_info, UpdateGoods},
			{ok, UpdateGoodsBin} = pt_13:write(13000, [0, UpdateGoodsRecord]),
			{ok, GoodsBin} = pt_13:write(13002, [Seq, ReturnInfo]),
			ReturnBin = <<UpdateGoodsBin/binary, GoodsBin/binary>>;			
		{error, Code} ->
%% 			io:format("~n=====open_treasure_box====Code:~p======~n", [Code]),
			_UsePrice = 0,
			NewPS = Status,
			_OpenSystemList = Status#player.open_system_list,
			ReturnInfo = {s2c_open_treasure_box, Code, 0, []},
			{ok, ReturnBin} = pt_13:write(13002, [Seq, ReturnInfo]);
		_Error ->
%% 			io:format("~n=====open_treasure_box====_Error:~p======~n", [_Error]),
			_UsePrice = 0,
			NewPS = Status,
			_OpenSystemList = Status#player.open_system_list,
			ReturnInfo = {s2c_open_treasure_box, ?FLAG_FAIL, 0, []},
			{ok, ReturnBin} = pt_13:write(13002, [Seq, ReturnInfo])
	end,
	lib_send:send_to_sid(NewPS#player.other#player_other.pid_send, ReturnBin),
	%%lib_send:pack_and_send(NewPS#player.other#player_other.pid_send, 13002, ReturnInfo, Seq),
	%%{ok, Status#player{coin = Status#player.coin - UsePrice, open_system_list = OpenSystemList}};
	{ok, NewPS};

%% 穿装备
handle_cmd(13003, Seq, Status, [GoodsId]) ->
	%%io:format("~n======13003======GoodsId:~p======~n", [GoodsId]),
	case goods_util:equip_goods(Status, GoodsId) of
		{ok, GTID, _IconId} ->
			lib_player:update_skin(Status, GTID),
%% 			case lib_player:change_facelook(Status, IconId) of %% 更新玩家头像
%% 				{ok, FacelookId} ->					
%% 					FaceLook = FacelookId;
%% 				_ ->
%% 					FaceLook = Status#player.facelook
%% 			end,
			
			NewStatus = Status#player{skin = GTID},
			%% 通知客户端 头像有改变
			%%lib_send:pack_and_send(NewStatus#player.other#player_other.pid_send, 11007, {s2c_change_facelook, ?FLAG_SUCC, FaceLook}, 0),
			ReturnInfo = {s2c_equip_goods, ?FLAG_SUCC};
		{error, Code} ->
			NewStatus = Status,
			ReturnInfo = {s2c_equip_goods, Code};
		_ ->
			NewStatus = Status,
			ReturnInfo = {s2c_equip_goods, ?FLAG_FAIL}
	end,
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 13003, ReturnInfo, Seq),		
	{ok, NewStatus};

%% 卸装备
handle_cmd(13004, Seq, Status, [GoodsId]) ->
	case goods_util:demount_equipment(Status, GoodsId, 1) of
		{ok, _GTID} ->
			lib_player:update_skin(Status, 0),
%% 			case lib_player:get_system_facelook_list() of
%% 				SysFacelookL when is_list(SysFacelookL) andalso length(SysFacelookL) > 0 ->
%% 					IconId = util:rand_list(SysFacelookL); %% 系统头像
%% 				_ ->  %% 没有系统头像怎么办
%% 					IconId = 0
%% 			end,
%% 			%%IconId = 100000001, 
%% 			case lib_player:change_facelook(Status, IconId) of %% 更新玩家头像
%% 				{ok, _IconId} ->
%% 					FaceLook = IconId;
%% 				_ ->
%% 					FaceLook = 0
%% 			end,
			NewStatus = Status#player{skin = 0},
			%% 通知客户端 头像有改变
			%%lib_send:pack_and_send(NewStatus#player.other#player_other.pid_send, 11007, {s2c_change_facelook, ?FLAG_SUCC, FaceLook}, 0),
			ReturnInfo = {s2c_demount_equipment, ?FLAG_SUCC};
		{error, Code} ->
			NewStatus = Status,
			ReturnInfo = {s2c_demount_equipment, Code};
		_ ->
			NewStatus = Status,
			ReturnInfo = {s2c_demount_equipment, ?FLAG_FAIL}
	end,
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 13004, ReturnInfo, Seq),		
	{ok, NewStatus};


%% 13006 进入背包
handle_cmd(13006, Seq, Status, Data) ->
	?PRINT("c2s_enter_backpack PlayerId: ~p, Data: ~p", [Status#player.id, Data]),
	RetCode = 
		case Status#player.table_attr of
			[] ->
				?FLAG_SUCC;
			[lottery | _] ->
				?FLAG_SUCC;
			_ ->
				mod_player:notify_player_system_limit(Status#player.table_attr, Status),
				?FLAG_FAIL
		end,
	RetRecord = #s2c_enter_backpack{
					ret_code = RetCode
				},
	?PRINT("s2c_enter_backpack PlayerId: ~p, RetRecord:~p", [Status#player.id, RetRecord]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 13006, RetRecord, Seq);

%% 13007 合成英雄
handle_cmd(13007, Seq, Status, [GoodsID, CharmNum]) ->
	%%io:format("~n=====handle_cmd(13007====[GoodsID, CharmNum]:~p======~n", [[GoodsID, CharmNum]]),
	case goods_util:compose_goods(Status, [GoodsID, CharmNum]) of
		{ok, NewPS, Code, UpdateGoods} ->
			lib_player:send_player_coin_redcard(NewPS),
			UpdateGoodsRecord = {s2c_goods_info, UpdateGoods},
			{ok, UpdateGoodsBin} = pt_13:write(13000, [0, UpdateGoodsRecord]),
			ReturnInfo = {s2c_compose_hero, Code},
			{ok, RtnBin} = pt_13:write(13007, [Seq, ReturnInfo]),
			ReturnBin = <<UpdateGoodsBin/binary, RtnBin/binary>>;			
		{error, Code} ->
			%%io:format("~n=====handle_cmd(13007====Code:~p======~n", [Code]),
			NewPS = Status,
			ReturnInfo = {s2c_compose_hero, Code},
			{ok, ReturnBin} = pt_13:write(13007, [Seq, ReturnInfo]);
		_Error ->
			%%io:format("~n=====handle_cmd(13007====_Error:~p======~n", [_Error]),
			NewPS = Status,
			ReturnInfo = {s2c_compose_hero, ?FLAG_FAIL},
			{ok, ReturnBin} = pt_13:write(13007, [Seq, ReturnInfo])
	end,
	lib_send:send_to_sid(NewPS#player.other#player_other.pid_send, ReturnBin),
	{ok, NewPS};


%% 13008 获取熔炼列表
handle_cmd(13008, Seq, Status, [UID]) when UID =:= Status#player.id ->
	%%io:format("~n=====handle_cmd(13008====UID:~p======~n", [UID]),
	SmeltList = lib_goods_smelt:get_player_smelt_list(Status),
	%%io:format("~n===13008======SmeltList:~p=======~n", [SmeltList]),
	RecordSmelt = [{smelt_info, ID, State} || {ID, State} <- SmeltList],
	ReturnInfo = {s2c_get_smelt_list, RecordSmelt},
	{ok, ReturnBin} = pt_13:write(13008, [Seq, ReturnInfo]),
	lib_send:send_to_sid(Status#player.other#player_other.pid_send, ReturnBin),
	{ok, Status};


%% 13009 熔炼物品
handle_cmd(13009, Seq, Status, [SmeltID, IsUseProtectCharm, GoodsList]) ->
	%%io:format("~n=====handle_cmd(13009====[SmeltID, IsUseProtectCharm, GoodsList]:~p======~n", [[SmeltID, IsUseProtectCharm, GoodsList]]),
	case lib_goods_smelt:smelt_goods(Status, GoodsList, SmeltID, IsUseProtectCharm) of
		{ok, Code, NewPS, DeleteGoodsList} ->
			lib_player:send_player_coin_redcard(NewPS),
			DeleteRecord = [{smelt_goods_info, GTID, Num} || {GTID, Num} <- DeleteGoodsList],
			ReturnInfo = {s2c_smelt_goods, Code, DeleteRecord},
			{ok, ReturnBin} = pt_13:write(13009, [Seq, ReturnInfo]);		
		{error, Code} ->
			%%io:format("~n=====handle_cmd(13009====Code:~p======~n", [Code]),
			NewPS = Status,
			ReturnInfo = {s2c_smelt_goods, Code, []},
			{ok, ReturnBin} = pt_13:write(13009, [Seq, ReturnInfo]);
		_Error ->
			%%io:format("~n=====handle_cmd(13009====_Error:~p======~n", [_Error]),
			NewPS = Status,
			Code = ?FLAG_FAIL,
			ReturnInfo = {s2c_smelt_goods, Code, []},
			{ok, ReturnBin} = pt_13:write(13009, [Seq, ReturnInfo])
	end,
	lib_send:send_to_sid(NewPS#player.other#player_other.pid_send, ReturnBin),
	if
		Code =:= ?FLAG_SUCC ->
			handle_cmd(13008, 0, NewPS, [NewPS#player.id]);
		true ->
			skip
	end,
	{ok, NewPS};


%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "pp_goods no match"}.





