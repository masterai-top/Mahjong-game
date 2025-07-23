%% @author mxb
%% @doc @todo 兑奖


-module(pp_lottery).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/4]).


-include("common.hrl").
-include("record.hrl").
-include("debug.hrl").
-include("record/data_goods_record.hrl").
-include("data_system_limit_record.hrl").

handle(Cmd, Seq, Player, Data) ->
	if
		Cmd =:= 17012 ->
			handle_cmd(Cmd, Seq, Player, Data);
		true ->
			case lists:member(?LOTTERY_SYSTEM, Player#player.open_system_list) of
				true ->
					%%io:format("~n===handle_cmd==Cmd:~p===table_attr:~p====~n", [Cmd, Player#player.table_attr]),
					handle_cmd(Cmd, Seq, Player, Data);
				_ ->					
					case data_system_limit:get("lottery") of
						#system_limit_config{vip = LimitVip, hero_num = HeroLimit} ->
							HeroNum = lib_goods:get_goods_by_goods_type_num(Player#player.id, 2),
							if
								Player#player.vip < LimitVip ->							
									Code = 11014005;
								HeroNum < HeroLimit ->
									Code = 11014006;
								true ->
									Code = ?FLAG_SUCC
							end;
						_ ->
							Code = ?FLAG_FAIL
					end,
					if
						Code =:= ?FLAG_SUCC ->
							NewPlayer = lib_system_config:open_lottery_system(Player),
							handle_cmd(Cmd, Seq, NewPlayer, Data);
						true ->					
							ReturnInfo = {s2c_player_system_limit, Code},
							lib_send:pack_and_send(Player#player.other#player_other.pid_send, 11014, ReturnInfo, 0),
							{ok, Player}
					end
			end
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%%--------------------------------------
%%Protocol: 17000 请求进入房间
%%--------------------------------------
handle_cmd(17000, Seq, Status, _Data) ->
	%%io:format("~n===handle_cmd=======17000===================~n"),
	GoodsWorth = lib_goods:get_player_goods_value(Status),
	LotWinNum = Status#player.lot_win_num,
	LotLoseNum = Status#player.lot_lose_num,
	SendGoodsWorth = Status#player.send_goods_value,
	ReceiveGoodsWorth = Status#player.receive_goods_value,
	PoolValue = Status#player.pool_value,
	Coin = Status#player.coin,
	PayAmount = Status#player.total_recharge_rmb,
	
	LotteryControl = lottery_util:get_player_lottery_control(Status#player.id),
	
	NowTime = util:unixtime(),
	PlayerGame = lib_player:get_player_game(Status),
	if
		PlayerGame#player_game.first_lottery_time =:= 0 ->
			IsNewer = 1,
			lib_player:update_player_game(9, Status);
		true ->
			IsNewer = ?IF((NowTime - PlayerGame#player_game.first_lottery_time) =< 3 , 1, 0),
			skip
	end,
	%%io:format("~n===handle_cmd===17000===IsNewer:~p===first_lottery_time:~p=~n", [IsNewer, NowTime - PlayerGame#player_game.first_lottery_time]), 
	F = fun(Domain, ServerId, Node, Data, PS) ->
				case mod_lottery:get_mod_lottery_pid(Domain, ServerId) of 
					Pid when is_pid(Pid) ->
						gen_server:cast(Pid, {enter_lottery, Data}),
						{ok, Status};
						%%{ok, PS#player{table_attr = [lottery, Pid, Domain, ServerId, Node]}};
					_Error ->
						%%io:format("~n===handle_cmd=======17000========get_mod_lottery_pid:~p========~n", [_Error]),
						ReturnInfo = {s2c_join_lot_room, ?FLAG_FAIL, 0, {state_info, 0, 0}, 0, 0, [], [], {star_info, 0, "", 0, 0, 0}, []},
						lib_send:pack_and_send(PS#player.other#player_other.pid_send, 17000, ReturnInfo, Seq),
						{ok, PS#player{table_attr = []}}
				end
		end,
	
	case Status#player.table_attr of
		[lottery, _LotteryPid, Domain, ServerId, GameNode, _] ->
			%%gen_server:cast(LotteryPid, {enter_lottery, [Seq, Status#player.id, Status#player.nick, Status#player.facelook, Status#player.level, node(),
			%%											 GoodsWorth, SendGoodsWorth, ReceiveGoodsWorth, PoolValue, LotWinNum, LotLoseNum, Coin, PayAmount]});
			%%io:format("~n~n~n===handle_cmd=====17000=1===[Node, ServerId, Domain]:~p====~n~n~n", [[Node, ServerId, Domain]]),
			RequestData = [Seq, Status#player.id, Status#player.nick, Status#player.facelook, Status#player.vip, node(), LotteryControl, IsNewer,
				   GoodsWorth, SendGoodsWorth, ReceiveGoodsWorth, PoolValue, LotWinNum, LotLoseNum, Coin, PayAmount, ServerId, Domain, GameNode],
			F(Domain, ServerId, GameNode, RequestData, Status);
		[] -> %% 第一次进入
			[GameNode, ServerId, Domain] = lib_game_online:select_game_server(lottery,200),
			%%io:format("~n~n~n===handle_cmd=====17000==2==[Node, ServerId, Domain]:~p====~n~n~n", [[Node, ServerId, Domain]]),
			RequestData = [Seq, Status#player.id, Status#player.nick, Status#player.facelook, Status#player.vip, node(), LotteryControl, IsNewer,
				   GoodsWorth, SendGoodsWorth, ReceiveGoodsWorth, PoolValue, LotWinNum, LotLoseNum, Coin, PayAmount, ServerId, Domain, GameNode],
			F(Domain, ServerId, GameNode, RequestData, Status);
		%% 			case mod_lottery:get_mod_lottery_pid(Domain, ServerId) of 
		%% 				Pid when is_pid(Pid) ->
		%% 					gen_server:cast(Pid, {enter_lottery, [Seq, Status#player.id, Status#player.nick, Status#player.facelook, Status#player.level, node(),
		%% 														 GoodsWorth, SendGoodsWorth, ReceiveGoodsWorth, PoolValue, LotWinNum, LotLoseNum, Coin, PayAmount]}),
		%% 					{ok, Status#player{table_attr = [lottery, Pid, Domain, ServerId, Node]}};
		%% 				_Error ->
		%% 					io:format("~n===handle_cmd=======17000========get_mod_lottery_pid:~p========~n", [_Error]),
		%% 					ReturnInfo = {s2c_join_lot_room, ?FLAG_FAIL, 0, {state_info, 0, 0}, 0, 0, [], []},
		%% 					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17000, ReturnInfo, Seq),
		%% 					{ok, Status}
		%% 			end;
		_Error1 ->
			%%io:format("~n===handle_cmd=======17000=======Error1:~p========~n", [_Error1]),
			ReturnInfo = {s2c_join_lot_room, ?FLAG_FAIL, 0, {state_info, 0, 0}, 0, 0, [], [], {star_info, 0, "", 0, 0, 0}, []},  %%{rate_info, 0, "", 0, 0}
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17000, ReturnInfo, Seq),
			{ok, Status}
	end;


%%--------------------------------------
%%Protocol: %% 17001 离开房间
%%--------------------------------------
handle_cmd(17001, Seq, Status, _Data) ->
	%%io:format("~n===handle_cmd=======17001===================~n"),
	case Status#player.table_attr of
		[lottery, _LotteryPid, DomainID, ServerID, _Node, _] ->
			case mod_lottery:get_mod_lottery_pid(DomainID, ServerID) of 
				Pid when is_pid(Pid) ->
					gen_server:cast(Pid, {leave_lottery, [Seq, Status#player.id]});
				_ ->  %% 活动进程已经挂掉了
					ReturnInfo = {s2c_exit_lot_room, ?FLAG_FAIL},
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17001, ReturnInfo, Seq),
					{ok, Status#player{table_attr = []}}
			end;		
		_Error ->
			%%io:format("~n===handle_cmd=======17001=========_Error:~p=~n", [_Error]),
			ReturnInfo = {s2c_exit_lot_room, ?FLAG_FAIL},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17001, ReturnInfo, Seq),
			{ok, Status}
	end;

%%--------------------------------------
%%Protocol: %% 17002 押注
%%--------------------------------------
handle_cmd(17002, Seq, Status, [Type, GoodsID, Num]) ->
	%%io:format("~n===handle_cmd=======17002========[uid, Type, GoodsID, Num]:~p============~n", [[Status#player.id, Type, GoodsID, Num]]),
	case Status#player.table_attr of
		[lottery, _LotteryPid, DomainID, ServerID, _Node, _] -> 
			%%case lib_goods:get_goods_by_id(Status#player.id, GoodsID) of
			case get_goods_record(Type, [GoodsID, Num, Status#player.id]) of
				#goods{location = Location, num = HasNum} = Goods ->
					case data_goods:get(Goods#goods.gtid) of
						#goods_config{buy_price = BuyPrice} ->
							if
								Type =:= 2 andalso Status#player.coin < BuyPrice -> %% 金币不足
									NewPS = Status,
									Info = {s2c_make_bet, 17002008, 0, 0, 0, 0},
									lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17002, Info, Seq);
								Goods#goods.type =/= 3 ->		
									IsGoodsEnough = ?IF(Location =:= 1 , (HasNum - 1) >= Num, HasNum >= Num),
									%%case lib_goods:is_enough_goods(Status, [{Goods#goods.gtid, Num}]) of	
									case IsGoodsEnough of									
										true ->
											case mod_lottery:get_mod_lottery_pid(DomainID, ServerID) of 
												Pid when is_pid(Pid) ->
													Return = gen_server:call(Pid, {make_bet, [Seq, Status#player.id, GoodsID, Goods#goods.gtid, Num]}),
													NewPS = make_bet_return(Type, [BuyPrice, GoodsID, Return, Status, Seq]);
												_Error5 ->  %% 活动进程已经挂掉了
													%%io:format("~n===handle_cmd=======17002========_Error5:~p============~n", [_Error5]),
													NewPS = Status,
													Info = {s2c_make_bet, ?FLAG_FAIL, 0, 0, 0, 0},
													lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17002, Info, Seq)
											end;
										_Error1 -> %% 物品数量不足
											%%io:format("~n===handle_cmd=======17002========_Error1:~p============~n", [_Error1]),
											NewPS = Status,
											Info = {s2c_make_bet, 17002001, 0, 0, 0, 0},
											lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17002, Info, Seq)
									end;
								true ->   %% 宝箱不可押注
									%%io:format("~n===handle_cmd=======17007================~n"),
									NewPS = Status,
									Info = {s2c_make_bet, 17002007, 0, 0, 0, 0},
									lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17002, Info, Seq)
							end;
						_Error4 ->
							%%io:format("~n===handle_cmd=======17002========_Error4:~p============~n", [_Error4]),
							NewPS = Status,
							Info = {s2c_make_bet, 17002001, 0, 0, 0, 0},
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17002, Info, Seq)
					end;
				_Error2 ->  %% 物品不存在
					%%io:format("~n===handle_cmd=======17002========_Error2:~p============~n", [_Error2]),
					NewPS = Status,
					Info = {s2c_make_bet, 17002001, 0, 0, 0, 0},
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17002, Info, Seq)
			end;
		_Error3 -> %% 不在当前活动
			%%io:format("~n===handle_cmd=======17002========_Error3:~p============~n", [_Error3]),
			NewPS = Status,
			Info = {s2c_make_bet, ?FLAG_FAIL, 0, 0, 0, 0},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17002, Info, Seq)
	end,
	{ok, NewPS}; 
	
	
%%--------------------------------------
%%Protocol: %% 17010 排行榜
%%--------------------------------------
handle_cmd(17010, Seq, Status, [Type]) ->
	%%io:format("~n===handle_cmd====17010=====[Type]:~p=======~n", [[Type]]),
	case Status#player.table_attr of
		[lottery, _LotteryPid, DomainID, ServerID, _Node, _] when Type > 0 andalso Type =< 3->
			case mod_lottery:get_mod_lottery_pid(DomainID, ServerID) of
				Pid when is_pid(Pid) ->
					gen_server:cast(Pid, {get_lottery_rank, [Seq, Status#player.id, Type]});
				_ -> %% 进程已经挂掉了
					Info = {s2c_lottery_rank, ?FLAG_FAIL, [], {star_info, 0, "", 0, 0, 0}},
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17010, Info, Seq),
					{ok, Status}
			end;
		_ ->
			Info = {s2c_lottery_rank, ?FLAG_FAIL, [], {star_info, 0, "", 0, 0, 0}},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17010, Info, Seq),
			{ok, Status}
	end;
					
%%--------------------------------------
%%Protocol: %% 17011 幸运之星
%%--------------------------------------
%% handle_cmd(17011, Seq, Status, _) ->
%% 	%%io:format("~n===handle_cmd====17010=====[Type]:~p=======~n", [[Type]]),
%% 	case Status#player.table_attr of
%% 		[lottery, _LotteryPid, DomainID, ServerID, _Node] ->
%% 			case mod_lottery:get_mod_lottery_pid(DomainID, ServerID) of
%% 				Pid when is_pid(Pid) ->
%% 					gen_server:cast(Pid, {get_lottery_lucky_star, [Seq, Status#player.id]});
%% 				_ -> %% 进程已经挂掉了
%% 					Info = {s2c_lottery_rank, {star_info, 0, "", 0, 0, 0}},
%% 					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17011, Info, Seq),
%% 					{ok, Status}
%% 			end;
%% 		_ ->
%% 			Info = {s2c_lottery_rank, {star_info, 0, "", 0, 0, 0}, []},
%% 			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17011, Info, Seq),
%% 			{ok, Status}
%% 	end;

%%--------------------------------------
%%Protocol: %% 17012 天降祥瑞活动状态
%%--------------------------------------
handle_cmd(17012, Seq, Status, [UID]) when Status#player.id =:= UID ->
	State = lottery_util:get_good_omen_state(),
	%%io:format("~n====handle_cmd(17012======UID:~p==State:~p=~n", [UID, State]),
	Info = {s2c_good_omen_state, State},
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17012, Info, Seq),
	{ok, Status};

%%--------------------------------------
%%Protocol: %% 17014 领取天降祥瑞活动奖励
%%--------------------------------------
handle_cmd(17014, Seq, Status, [UID, GoodsList]) when Status#player.id =:= UID ->
	case Status#player.table_attr of
		[lottery, _LotteryPid, DomainID, ServerID, _Node, _] ->
			case mod_lottery:get_mod_lottery_pid(DomainID, ServerID) of
				Pid when is_pid(Pid) ->
					gen_server:cast(Pid, {get_good_omen_reward, [Seq, Status#player.id, GoodsList]});
				_ -> %% 进程已经挂掉了
					Info = {s2c_get_good_omen_reward, ?FLAG_FAIL},
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17014, Info, Seq),
					{ok, Status}
			end;
		_ ->
			Info = {s2c_get_good_omen_reward, 17014003},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17014, Info, Seq),
			{ok, Status}
	end;


handle_cmd(_Cmd, _, _, _) ->
	{error, "pp_lottery no match"}.



pack_goods_lottery(UID, GoodsInfo, LotteryInfo) ->
	GoodsList = [{goods_record, GTId, Id, Num, Type, SubType, Location, Quality} || 
				 #goods{gtid=GTId, id=Id, num=Num, location = Location, type = Type, subtype = SubType, quality = Quality} <- [GoodsInfo]],
	GoodsMsgRecord = {s2c_goods_info, GoodsList}, %%{s2c_goods_info, GoodsList}
	{ok,GoodsBin} = pt_13:write(13000, [0, GoodsMsgRecord]),
	{ok,LotteryBin} = pt_17:write(17002, LotteryInfo),
	DataBin = <<GoodsBin/binary, LotteryBin/binary>>,
	lib_send:send_to_uid(UID, DataBin),
	ok.


%% 获取道具
get_goods_record(1, [GoodsID, _Num, UID]) ->
	lib_goods:get_goods_by_id(UID, GoodsID);
get_goods_record(2, [GTID, Num, UID]) ->
	case data_goods:get(GTID) of
		#goods_config{} = GoodsConfig ->
			#goods{uid = UID, location = 0, num = Num, type = GoodsConfig#goods_config.type, gtid = GTID};
		_ ->
			error
	end;
get_goods_record(_Type, _Data) ->
	error.


%% 押注返回
make_bet_return(1, [_BuyPrice, GoodsID, Return, Status, Seq]) ->
	case Return of
		{s2c_make_bet, ?FLAG_SUCC, GoodTID, Num1, RBValue, PBValue} ->
			case lib_goods:delete_goods_no_notify(Status, GoodsID, Num1, 17001, 17) of %% 删除背包中的数据
				#goods{} = GoodsInfo ->																	
					Info = {s2c_make_bet, ?FLAG_SUCC, GoodTID, Num1, RBValue, PBValue},
					pack_goods_lottery(Status#player.id, GoodsInfo, [Seq, Info]);
				_ ->
					Info = {s2c_make_bet, ?FLAG_FAIL, 0, 0, 0, 0},
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17002, Info, Seq)
			end;															
		{s2c_make_bet, 2, _GoodTID, Num1, _RBValue, _PBValue} ->
			lib_goods:delete_goods(Status, GoodsID, Num1, 17001, 17); %% 删除背包中的数据
		{s2c_make_bet, Code, GoodTID, Num1, RBValue, PBValue} ->
			Info = {s2c_make_bet, Code, GoodTID, Num1, RBValue, PBValue},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17002, Info, Seq);
		_Error4 ->
			Info = {s2c_make_bet, ?FLAG_FAIL, 0, 0, 0, 0},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17002, Info, Seq)
	end,
	Status;
make_bet_return(2, [BuyPrice, _GoodsID, Return, Status, Seq]) ->
	case Return of
		{s2c_make_bet, ?FLAG_SUCC, GoodTID, Num1, RBValue, PBValue} ->
			NewPS = lib_resurce:reduce_resurce_list(Status, [{?MONEY_T_COIN, BuyPrice}], 17002, 17),
			Info = {s2c_make_bet, ?FLAG_SUCC, GoodTID, Num1, RBValue, PBValue},
			lib_send:pack_and_send(NewPS#player.other#player_other.pid_send, 17002, Info, Seq);															
		{s2c_make_bet, 2, _GoodTID, _Num1, _RBValue, _PBValue} ->
			NewPS = lib_resurce:reduce_resurce_list(Status, [{?MONEY_T_COIN, BuyPrice}], 17002, 17);
		{s2c_make_bet, Code, GoodTID, Num1, RBValue, PBValue} ->
			NewPS = Status,
			Info = {s2c_make_bet, Code, GoodTID, Num1, RBValue, PBValue},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17002, Info, Seq);
		_Error4 ->
			NewPS = Status,
			Info = {s2c_make_bet, ?FLAG_FAIL, 0, 0, 0, 0},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 17002, Info, Seq)
	end,
	NewPS.









