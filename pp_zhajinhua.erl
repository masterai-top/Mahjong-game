%%%-------------------------------------------------------------------
%%% @author lkh
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 五月 2017 10:08
%%%-------------------------------------------------------------------
-module(pp_zhajinhua).
-author("lkh").
-include("common.hrl").
-include("proto/proto_40_zhajinhua.hrl").
-include("record.hrl").
-include("log.hrl").
-include("zhajinhua.hrl").

%% API
-export([handle/4]).


handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 进入桌子
handle_cmd(40000, Seq, #player{table_attr = []} = Player, _Data) ->
	[_Node, ServerID, DomainID] = lib_game_online:select_game_server(?GAME_TYPE, 1000),
	RoleZhajinhuaInfo = lib_zhajinhua_rule:to_zhajinhua_roleinfo(Player),
	case catch mod_zhajinhua_server:enter_table(DomainID, ServerID, RoleZhajinhuaInfo, Seq) of
		{ok, TableID, TablePid} ->
			?INFO_MSG("enter table ... :~w ~w", [TablePid, TableID]),

			NewTableAttr = [#zhajinhua_table_attr{server_id = ServerID, domain_id = DomainID, table_id = TableID, table_pid = TablePid}],
			{ok, Player#player{table_attr = NewTableAttr}};
		{error, Code} ->
			cast_enter_table_fail(Player, Seq, Code),
			ok;
		What ->
			cast_enter_table_fail(Player, Seq),
			?ERROR_MSG("enter zhajinhua table err ~w", [What]),
			ok
	end;
%% 已经在桌子上 估计是断线之类的 需要重新进入
handle_cmd(40000, Seq, #player{table_attr = [#zhajinhua_table_attr{table_id = TableID, table_pid = TablePid}]} = Player, _Data) ->
	?INFO_MSG("re_enter.... :~w ~w", [TablePid, TableID]),
	RoleZhajinhuaInfo = lib_zhajinhua_rule:to_zhajinhua_roleinfo(Player),
	case rpc:call(node(TablePid), erlang, is_process_alive, [TablePid]) of
		true ->
			mod_zhajinhua_server:re_enter_table(TablePid, TableID, RoleZhajinhuaInfo, Seq),
			ok;
		_ ->
			?ERROR_MSG("PROCESS DEAD ", []),
			handle_cmd(40000, Seq, Player#player{table_attr = []}, _Data)
	end;

%% 下注
handle_cmd(40004, Seq, #player{id = RoleID, table_attr = [#zhajinhua_table_attr{table_pid = TablePid}]} = Player, Request) ->
	case mod_zhajinhua_table:check_xiazhu(TablePid, RoleID, Request) of
		{ok, NeedCoin} ->
			%% 扣取玩家金币
			case lib_money:has_enough_money(Player, NeedCoin, ?MONEY_T_COIN) of
				true ->
					NewPlayer = lib_resurce:reduce_resurce_list(Player, [{?MONEY_T_COIN, NeedCoin}], ?LOG_ZHAJINHUA_XIAZHU, 40),
					mod_zhajinhua_table:do_xiazhu(TablePid, RoleID, {Seq, Request}),
					{ok, NewPlayer};
				_ ->
					%% 金币不够 下发通知
					?ERROR_MSG("xiazhu fail.... no money", []),
					cast_xiazhu_err(RoleID, Seq, Request, ?ERROR_CODE_MONEY_NOT_ENOUGH),
					ok
			end;
		{error, Code} ->
			%% 下发通知
			?ERROR_MSG("xiazhu fail.... :~w ~w", [RoleID, Code]),
			cast_xiazhu_err(RoleID, Seq, Request, Code),
			ok
	end;

%% 弃牌
handle_cmd(40005, Seq, #player{id = RoleID, table_attr = [#zhajinhua_table_attr{table_pid = TablePid}]} = _Player, _Data) ->
	mod_zhajinhua_table:giveup(TablePid, RoleID, Seq);

%% 申请比牌
handle_cmd(40006, Seq, #player{id = RoleID, table_attr = [#zhajinhua_table_attr{table_pid = TablePid}]} = Player,
	#c2s_zhajinhua_compare{opt = ?OPT_COMPARE_REQUEST, dest_role_id = DestRoleID} = Request) ->
	case mod_zhajinhua_table:check_compare_card(TablePid, RoleID, DestRoleID) of
		{ok, NeedCoin} ->
			%% 扣取玩家金币
			case lib_money:has_enough_money(Player, NeedCoin, ?MONEY_T_COIN) of
				true ->
					%% todo
					NewPlayer = lib_resurce:reduce_resurce_list(Player, [{?MONEY_T_COIN, NeedCoin}], ?LOG_ZHAJINHUA_COMPARE_REQUEST, 40),
					cast_compare_card_succ(RoleID, DestRoleID, Seq),
					mod_zhajinhua_table:do_compare_card_request(TablePid, RoleID, DestRoleID, Seq),
					{ok, NewPlayer};
				_ ->
					%% 金币不够 下发通知
					cast_compare_card_err(RoleID, Seq, Request, ?ERROR_CODE_MONEY_NOT_ENOUGH),
					ok
			end;
		{error, Code} ->
			%% 下发通知
			cast_compare_card_err(RoleID, Seq, Request, Code),
			ok
	end;
%% 同意比牌
handle_cmd(40006, Seq, #player{id = RoleID, table_attr = [#zhajinhua_table_attr{table_pid = TablePid}]} = _Player,
	#c2s_zhajinhua_compare{opt = ?OPT_COMPARE_AGREE} = _Request) ->
	cast_compare_card_succ(0, RoleID, Seq),
	mod_zhajinhua_table:do_compare_card_agree(TablePid, RoleID, Seq);
%% 拒绝比牌
handle_cmd(40006, Seq, #player{id = RoleID, table_attr = [#zhajinhua_table_attr{table_pid = TablePid}]} = Player,
	#c2s_zhajinhua_compare{opt = ?OPT_COMPARE_REJECT} = Request) ->
	case mod_zhajinhua_table:check_compare_card_reject(TablePid, RoleID) of
		{ok, NeedCoin} ->
			%% 扣取玩家金币
			case lib_money:has_enough_money(Player, NeedCoin, ?MONEY_T_COIN) of
				true ->
					NewPlayer = lib_resurce:reduce_resurce_list(Player, [{?MONEY_T_COIN, NeedCoin}], ?LOG_ZHAJINHUA_COMPARE_REJECT, 40),
					cast_compare_card_succ(0, RoleID, Seq),
					mod_zhajinhua_table:do_compare_card_reject(TablePid, RoleID, Seq),
					{ok, NewPlayer};
				_ ->
					%% 金币不够 下发通知
					cast_compare_card_err(RoleID, Seq, Request, ?ERROR_CODE_MONEY_NOT_ENOUGH),
					ok
			end;
		{error, Code} ->
			%% 下发通知
			cast_compare_card_err(RoleID, Seq, Request, Code),
			ok
	end;

%% 孤注一掷
handle_cmd(40007, Seq, #player{id = RoleID, table_attr = [#zhajinhua_table_attr{table_pid = TablePid}]} = Player,
	#c2s_zhajinhua_guzhuyizhi{} = Request) ->
	case mod_zhajinhua_table:check_guzhuyizhi(TablePid, RoleID) of
		{ok, CostCoin} ->
%%			 扣取玩家金币
			NewPlayer = lib_resurce:reduce_resurce_list(Player, [{?MONEY_T_COIN, CostCoin}], ?LOG_ZHAJINHUA_GUZHUYIZHI, 40),
			cast_guzhuyizhi_succ(RoleID, Seq),
			mod_zhajinhua_table:do_guzhuyizhi(TablePid, RoleID),
			{ok, NewPlayer};
		{error, Code} ->
			%% 下发通知
			cast_guzhuyizhi_err(RoleID, Seq, Request, Code),
			ok
	end;

%% 亮牌
handle_cmd(40010, _Seq, #player{id = RoleID, table_attr = [#zhajinhua_table_attr{table_pid = TablePid}]} = _Player, _Data) ->
	mod_zhajinhua_table:showcard(TablePid, RoleID);

%% 看牌
handle_cmd(40011, Seq, #player{id = RoleID, table_attr = [#zhajinhua_table_attr{table_pid = TablePid}]} = _Player, _Data) ->
	mod_zhajinhua_table:lookcard(TablePid, RoleID, Seq);

%% 40015 玩家聊天
handle_cmd(40015, _Seq, #player{id = RoleID, table_attr = [#zhajinhua_table_attr{table_pid = TablePid}]} = _Player, Data) ->
	mod_zhajinhua_table:talk(TablePid, RoleID, Data);

%% 40016 退出
handle_cmd(40016, _Seq, #player{id = _RoleID, table_attr = [#zhajinhua_table_attr{table_pid = TablePid} = TableAttr]} = Player, _Data) ->
	mod_zhajinhua_table:save_data(TablePid, Player),
	mod_zhajinhua_server:exit_table(TableAttr, Player, ?ERROR_CODE_SUCC),
	{ok, Player#player{table_attr = []}};

%% 40017 换桌
handle_cmd(40017, Seq, #player{id = RoleID, table_attr = [#zhajinhua_table_attr{table_pid = TablePid, table_id = TableID} = TableAttr]} = Player, _Data) ->
	RoleZhajinhuaInfo = lib_zhajinhua_rule:to_zhajinhua_roleinfo(Player),
	?INFO_MSG("change table ... :~w ~w", [RoleID, TablePid]),
	case mod_zhajinhua_server:change_table(RoleZhajinhuaInfo, TablePid, TableID, Seq) of
		{ok, NewTableID, NewTablePid} ->
			?INFO_MSG("after change table ... :~w ~w", [NewTableID, NewTablePid]),
			NewTableAttr = [TableAttr#zhajinhua_table_attr{table_id = NewTableID, table_pid = NewTablePid}],
			{ok, Player#player{table_attr = NewTableAttr}};
		{error, Code} ->
			cast_change_table(RoleID, Seq, Code),
			ok;
		What ->
			cast_change_table(RoleID, Seq, ?ERROR_CODE_SYSTEM_ERR),
			?ERROR_MSG("change zhajinhua table err ~w", [What]),
			ok
	end;

%% allin
handle_cmd(40019, Seq, #player{id = RoleID, table_attr = [#zhajinhua_table_attr{table_pid = TablePid}]} = Player, Request) ->
	case mod_zhajinhua_table:check_allin(TablePid, RoleID) of
		{ok, NeedCoin} ->
			%% 扣取玩家金币
			case lib_money:has_enough_money(Player, NeedCoin, ?MONEY_T_COIN) of
				true ->
					NewPlayer = lib_resurce:reduce_resurce_list(Player, [{?MONEY_T_COIN, NeedCoin}], ?LOG_ZHAJINHUA_XIAZHU, 40),
					mod_zhajinhua_table:do_allin(TablePid, RoleID, Seq),
					{ok, NewPlayer};
				_ ->
					%% 金币不够 下发通知
					?ERROR_MSG("allin fail.... no money", []),
					cast_allin_err(RoleID, Seq, Request, ?ERROR_CODE_MONEY_NOT_ENOUGH),
					ok
			end;
		{error, Code} ->
			%% 下发通知
			?ERROR_MSG("allin fail.... :~w ~w", [RoleID, Code]),
			cast_allin_err(RoleID, Seq, Request, Code),
			ok
	end;

%% 容错处理
handle_cmd(_Cmd, _Seq, _Player, _Data) ->
	?ERROR_MSG("no handle msg.... :~w ~w ~w", [_Cmd, _Player#player.table_attr, _Data]),
	{error, "pp_zhajinhua no match"}.

%% 进入桌子失败
cast_enter_table_fail(Player, Seq) ->
	cast_enter_table_fail(Player, Seq, ?FLAG_FAIL).
cast_enter_table_fail(Player, Seq, Code) ->
	RetRecord = #s2c_zhajinhua_enter{
		code = Code,
		table_id = 0
	},
	?PRINT("s2c_zhajinhua_enter PlayerId: ~p, RetRecord: ~p", [Player#player.id, RetRecord]),
	lib_send:pack_and_send(Player#player.other#player_other.pid_send, 40000, RetRecord, Seq).

%% 下注失败
cast_xiazhu_err(RoleID, Seq, Request, Code) ->
	Data = #s2c_zhajinhua_xiazhu{
		role_id = RoleID,
		type = Request#c2s_zhajinhua_xiazhu.type,
		add_money = Request#c2s_zhajinhua_xiazhu.add_money,
		code = Code},
	lib_send:pack_and_send(RoleID, 40004, Data, Seq).

%% allin失败
cast_allin_err(RoleID, Seq, Request, Code) ->
	Data = #s2c_zhajinhua_all_in{
		role_id = RoleID,
		coin = 0,
		code = Code},
	lib_send:pack_and_send(RoleID, 40019, Data, Seq).

%% 比牌失败
cast_compare_card_err(RoleID, Seq, _Request, Code) ->
	Data = #s2c_zhajinhua_compare{
		code = Code,
		result = #proto_compare_result{src_role_id = 0, dest_role_id = 0, result = ?OPT_COMPARE_REQUEST}
	},
	lib_send:pack_and_send(RoleID, 40006, Data, Seq).

%% 比牌请求发起成功
cast_compare_card_succ(RoleID, DestRoleID, Seq) ->
	Data = #s2c_zhajinhua_compare{
		code = ?ERROR_CODE_SUCC,
		result = #proto_compare_result{src_role_id = RoleID, dest_role_id = DestRoleID, result = ?OPT_COMPARE_REQUEST}
	},
	lib_send:pack_and_send(RoleID, 40006, Data, Seq).

%% 发起孤注一掷失败
cast_guzhuyizhi_err(RoleID, Seq, _Request, Code) ->
	Data = #s2c_zhajinhua_guzhuyizhi{
		code = Code,
		result = [],
		list = []
	},
	lib_send:pack_and_send(RoleID, 40007, Data, Seq).

%% 发起孤注一掷请求成功
cast_guzhuyizhi_succ(RoleID, Seq) ->
	Data = #s2c_zhajinhua_guzhuyizhi{
		code = ?ERROR_CODE_SUCC,
		result = [],
		list = []
	},
	lib_send:pack_and_send(RoleID, 40007, Data, Seq).

cast_change_table(RoleID, Seq, Code) ->
	Data = #s2c_change_table{
		code = Code
	},
	lib_send:pack_and_send(RoleID, 40017, Data, Seq).