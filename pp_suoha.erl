%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 五月 2017 15:00
%%%-------------------------------------------------------------------
-module(pp_suoha).
-author("Administrator").

-include("common.hrl").
-include("proto/proto_41_suoha.hrl").
-include("record.hrl").
-include("suoha.hrl").

%% API
-export([handle/4]).

handle(Cmd, Seq, Player, Data) ->
    handle_cmd(Cmd, Seq, Player, Data).

%% 进入桌子
handle_cmd(41000, Seq, #player{table_attr = []} = Player, Data) ->
    [_Node, ServerID, DomainID] = lib_game_online:select_game_server(?GAME_TYPE, 1000),
    RoleSuohaInfo = lib_suoha_rule:to_suoha_roleinfo(Player),
    case catch mod_suoha_server:enter_table(DomainID, ServerID, RoleSuohaInfo, Seq, Data#c2s_suoha_enter.type) of
        {ok, TableID, TablePid} ->
            ?INFO_MSG("enter table ... :~w ~w", [TablePid, TableID]),

            NewTableAttr = [#suoha_table_attr{server_id = ServerID, domain_id = DomainID, table_id = TableID, table_pid = TablePid, type = Data#c2s_suoha_enter.type}],
            {ok, Player#player{table_attr = NewTableAttr}};
        {error, Code} ->
            cast_enter_table_fail(Player, Seq, Code),
            ok;
        What ->
            cast_enter_table_fail(Player, Seq),
            ?ERROR_MSG("enter suoha table err ~w", [What]),
            ok
    end;
%% 已经在桌子上 估计是断线之类的 需要重新进入
handle_cmd(41000, Seq, #player{table_attr = [#suoha_table_attr{table_id = TableID, table_pid = TablePid}]} = Player, _Data) ->
    ?INFO_MSG("re_enter.... :~w ~w", [TablePid, TableID]),
    RoleSuohaInfo = lib_suoha_rule:to_suoha_roleinfo(Player),
    case rpc:call(node(TablePid), erlang, is_process_alive, [TablePid]) of
        true ->
            mod_suoha_server:re_enter_table(TablePid, TableID, RoleSuohaInfo, Seq),
            ok;
        _ ->
            ?ERROR_MSG("PROCESS DEAD ", []),
            handle_cmd(41000, Seq, Player#player{table_attr = []}, _Data)
    end;

%% 玩家操作
handle_cmd(41004, Seq, #player{id = RoleID, table_attr = [#suoha_table_attr{table_pid = TablePid}]} = Player, Request) ->
    case mod_suoha_table:check_player_opt(TablePid, RoleID, Request, Seq) of
        ok ->
            mod_suoha_table:do_player_opt(TablePid, RoleID, {Seq, Request}),
            ok;
        {ok, NeedCoin} ->
            %% 扣取玩家金币
            case lib_money:has_enough_money(Player, NeedCoin, ?MONEY_T_COIN) of
                true ->
                    NewPlayer = lib_resurce:reduce_resurce_list(Player, [{?MONEY_T_COIN, NeedCoin}], 0, 0),
                    mod_suoha_table:do_player_opt(TablePid, RoleID, {Seq, Request}),
                    {ok, NewPlayer};
                _ ->
                    %% 金币不够 下发通知
                    ?ERROR_MSG("xiazhu fail.... no money", []),
                    cast_player_opt_err(RoleID, Seq, Request, ?ERROR_CODE_MONEY_NOT_ENOUGH),
                    ok
            end;
        {error, Code} ->
            %% 下发通知
            ?ERROR_MSG("xiazhu fail.... :~w ~w", [RoleID, Code]),
            cast_player_opt_err(RoleID, Seq, Request, Code),
            ok
    end;

%% 40016 退出
handle_cmd(41008, _Seq, #player{id = _RoleID, table_attr = [#suoha_table_attr{table_pid = TablePid} = TableAttr]} = Player, _Data) ->
    mod_suoha_table:save_data(TablePid, Player),
    mod_suoha_server:exit_table(TableAttr, Player, ?ERROR_CODE_SUCC),
    {ok, Player#player{table_attr = []}};

%% 41009 换桌
handle_cmd(41009, Seq, #player{id = RoleID, table_attr = [#suoha_table_attr{table_pid = TablePid, table_id = TableID, type = Type} = TableAttr]} = Player, _Data) ->
    ?INFO_MSG("change table ... :~w ~w ~w", [RoleID, TablePid, Type]),
    case mod_suoha_server:change_table(Player, TablePid, TableID, Seq, Type) of
        {ok, NewTableID, NewTablePid} ->
            ?INFO_MSG("after change table ... :~w ~w", [NewTableID, NewTablePid]),
            NewTableAttr = [TableAttr#suoha_table_attr{table_id = NewTableID, table_pid = NewTablePid}],
            {ok, Player#player{table_attr = NewTableAttr}};
        {error, Code} ->
            cast_change_table(RoleID, Seq, Code),
            ok;
        What ->
            cast_change_table(RoleID, Seq, ?ERROR_CODE_SYSTEM_ERR),
            ?ERROR_MSG("change suoha table err ~w", [What]),
            ok
    end;

%% 41011 看牌
handle_cmd(41011, Seq, #player{id = RoleID, table_attr = [#suoha_table_attr{table_pid = TablePid}]} = _Player, Data) ->
    mod_suoha_table:look_card(TablePid, RoleID, {Seq, Data}),
    ok;

%% 41012 玩家聊天
handle_cmd(41012, _Seq, #player{id = RoleID, table_attr = [#suoha_table_attr{table_pid = TablePid}]} = _Player, Data) ->
    mod_suoha_table:talk(TablePid, RoleID, Data);

%% 容错处理
handle_cmd(_Cmd, _Seq, _Player, _Data) ->
    ?ERROR_MSG("no handle msg.... :~w ~w ~w", [_Cmd, _Player#player.table_attr, _Data]),
    {error, "pp_suoha no match"}.

cast_change_table(RoleID, Seq, Code) ->
    Data = #s2c_suoha_change_table{
        code = Code
    },
    lib_send:pack_and_send(RoleID, 41009, Data, Seq).

%% 进入桌子失败
cast_enter_table_fail(Player, Seq) ->
    cast_enter_table_fail(Player, Seq, ?FLAG_FAIL).
cast_enter_table_fail(Player, Seq, Code) ->
    RetRecord = #s2c_suoha_enter{
        code = Code,
        table_id = 0
    },
    ?PRINT("s2c_suoha_enter PlayerId: ~p, RetRecord: ~p", [Player#player.id, RetRecord]),
    lib_send:pack_and_send(Player#player.other#player_other.pid_send, 41000, RetRecord, Seq).

%%
cast_player_opt_err(RoleID, Seq, Request, Code) ->
    Data = #s2c_suoha_opt{
        role_id = RoleID,
        type = Request#c2s_suoha_opt.type,
        add_money = Request#c2s_suoha_opt.add_money,
        code = Code},
    lib_send:pack_and_send(RoleID, 41004, Data, Seq).
