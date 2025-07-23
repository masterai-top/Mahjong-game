%%%--------------------------------------
%%% @Module  : pp_vip
%%% @Author  : mxb
%%% @Created : 
%%% @Description:  VIP相关
%%%--------------------------------------

-module(pp_vip). 

-include("common.hrl").
-include("log.hrl").
-include("record.hrl").

-export([handle/4]).

%%
%% API Functions
%%

handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 31001 vip购买金币, 消耗钻石
handle_cmd(31001, Seq, Player, [RechargeID]) ->
	%%io:format("~n====31001====RechargeID:~p======~n", [RechargeID]),
	case lib_vip:vip_bug_coin(Player, RechargeID) of
		{ok, NewPlayer} ->
			{s2c_vip_info, Code, UID, VIP, Rmb, NextLV, NeedMoney, GiftID, SkinList, VipGiftNum, VipRewardList, VipCoinDoubleNum, DayPayNum} = lib_vip:get_vip_info(NewPlayer), %% , _DayPayNum
			VipReturnInfo = {s2c_vip_info, Code, UID, VIP, Rmb, NextLV, NeedMoney, GiftID, SkinList, VipGiftNum, VipRewardList, VipCoinDoubleNum, DayPayNum},
			lib_send:pack_and_send(NewPlayer#player.other#player_other.pid_send, 11010, VipReturnInfo, 0),
			Code = ?FLAG_SUCC;
		{error, Code} ->
			NewPlayer = Player;
		_ ->
			NewPlayer = Player,
			Code = ?FLAG_FAIL
	end,
	%%io:format("~n====31001====Code:~p======~n", [Code]),
	ReturnInfo = {s2c_vip_buy_coin, Code},
	lib_send:pack_and_send(Player#player.other#player_other.pid_send, 31001, ReturnInfo, Seq),
	{ok, NewPlayer};

%% 31002 SVIP信息
handle_cmd(31002, Seq, Player, [UID]) when Player#player.id =:= UID ->
	ReturnInfo = lib_vip:get_svip_info(Player),
	{_, _, _, LeftTime, _} = ReturnInfo,
	if
		LeftTime > 0 ->
			NewPS = Player;
		true ->
			NewPS = Player#player{vip = 0}
	end,
	lib_send:pack_and_send(NewPS#player.other#player_other.pid_send, 31002, ReturnInfo, Seq),
	{ok, NewPS};


%% 容错处理
handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	
	{error, "pp_vip no match"}.