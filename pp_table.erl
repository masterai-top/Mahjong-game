%%%--------------------------------------
%%% @Module  : pp_table
%%% @Author  : kexp
%%% @Created : 2016.08.19
%%% @Description:斗地主
%%%--------------------------------------
-module(pp_table).
-export([handle/4]).
-include("common.hrl").
-include("record.hrl").
-include("debug.hrl").
-include("room.hrl").
-include("record/data_room_record.hrl").
-compile(export_all). 

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

%% 进入斗地主牌局类型
handle_cmd(16000, Seq, Status, Data) ->
	[RoomId] = Data,
	case Status#player.table_attr of
		[] ->
			case data_room:get(RoomId) of
				DataRoom when is_record(DataRoom, room_config) ->						
					case lib_poke_room:join_room(Status, RoomId, Seq) of
						{ok, NewStatus} ->
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16000, {s2c_join_room,0,RoomId}, Seq),
							{ok, NewStatus};
						{error, Code} ->
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16000, {s2c_join_room,Code,RoomId}, Seq);
						_ ->
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16000, {s2c_join_room,1,RoomId}, Seq)
					end;
				_ ->
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16000, {s2c_join_room,16000001,RoomId}, Seq)
			end;
		[poke,RoomId1,ServerId, Domain,RoomPid,Node,_BattleId,_RecordId,_BattlePid, _UseNote,_IsExit] ->
			if
				RoomId =/= 0 ->
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16000, {s2c_join_room,11014001,RoomId}, Seq),
					{ok, Status};
				true ->
					try
						case gen_server:call(RoomPid,{re_enter, Status#player.id, Status#player.other#player_other.pid, false}) of
							{ok, 0,0,undefine} ->
								NewStatus = Status#player{table_attr = []},
								lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16000, {s2c_join_room,16000001,RoomId}, Seq),
								{ok, NewStatus};
							{ok, BattleId1, RecordId1, BattlePid1} ->								
								lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16000, {s2c_join_room,0,RoomId1}, Seq),				
								NewStatus = Status#player{table_attr = [poke,RoomId1,ServerId, Domain,RoomPid,Node,BattleId1, RecordId1, BattlePid1,_UseNote, false]},
								{ok, NewStatus};
							_ ->
								NewStatus = Status#player{table_attr = []},
								lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16000, {s2c_join_room,16000001,RoomId}, Seq),
								{ok, NewStatus}
						end
					catch 
						_:_ ->
							{ok, Status}
					end
			end;
		_ ->
			
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16000, {s2c_join_room,1,RoomId}, Seq)
	end;
			

%% 16005玩家牌局操作
handle_cmd(16005, Seq, Status, Data) ->
	[State,StatePos, StateType] = Data,
	PlayerId = Status#player.id,
	case Status#player.table_attr of
		[poke,RoomId,_ServerId, _Domain,_RoomPid,_Node,BattleId,RecordId,BattlePid,_UseNote,_IsExit] ->
			if
				BattleId =/= 0 andalso RecordId =/= 0 ->
					DataRoom = data_room:get(RoomId),
					case [State, StateType] of
						[3, 1] ->
							NewStatus = Status,
							NeedCoin = DataRoom#room_config.double_limie,
							Code =  0;
						[3, 2] -> %% 超级加倍
							NeedCoin = 0,
							case lib_goods:use_goods_by_type(Status, [{600010001,1}], 16008, 16 * 10 + DataRoom#room_config.type) of
								{error, ErrCode} ->
									case lib_player_share_shop:buy_share_shop_goods_in_ddz(Status, 201, 16008, 16 * 10 + DataRoom#room_config.type) of
										{ok, NewStatus} ->
											Code = 0;
										_ ->
											NewStatus = Status,
											Code = ErrCode
									end;
								_ ->
									NewStatus = Status,
									Code = 0
							end;					
						_ ->
							NewStatus = Status,
							NeedCoin = 0,
							Code = 0
					end,
					if
						Code =/= 0 ->
							lib_send:pack_and_send(NewStatus#player.other#player_other.pid_send, 16005, {s2c_player_choose_state, Code, State,StatePos, StateType,[]}, Seq),
							{ok, NewStatus};							
						Status#player.coin < NeedCoin ->
							lib_send:pack_and_send(NewStatus#player.other#player_other.pid_send, 16005, {s2c_player_choose_state, 16000007, State,StatePos, StateType,[]}, Seq),
							{ok, NewStatus};
						true ->
							if
								BattlePid =/= 0 ->
									gen_server:cast(BattlePid, {player_choose_state,[RecordId,PlayerId,State,StatePos,StateType, NewStatus#player.coin]}),
									{ok, NewStatus};
								true ->
									case mod_table:get_alive_table_pid(BattleId) of
										NewBattlePid when is_pid(NewBattlePid) ->
											gen_server:cast(NewBattlePid, {player_choose_state,[RecordId,PlayerId,State,StatePos,StateType, NewStatus#player.coin]}),
											{ok, NewStatus};
										_ ->
											lib_send:pack_and_send(NewStatus#player.other#player_other.pid_send, 16005, {s2c_player_choose_state, 1, State,StatePos, StateType,[]}, Seq),
											ok
									end
							end
					end;					
				true ->
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16005, {s2c_player_choose_state, 1, State,StatePos, StateType,[]}, Seq),
					ok
			end;
		_ ->			
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16005, {s2c_player_choose_state, 1, State,StatePos, StateType,[]}, Seq),
			ok
	end;

%% 16006玩家出牌
handle_cmd(16006, Seq, Status, Data) ->
	[Cards, Laizi] = Data,
	PlayerId = Status#player.id,
%% 	io:format("====16006===~p~n", [Status#player.table_attr]),
	case Status#player.table_attr of
		[poke,_RoomId, _ServerId, _Domain,_RoomPid,_Node,BattleId,RecordId,BattlePid,_UseNote,_IsExit] ->
			if
				 BattleId =/= 0 andalso RecordId =/= 0 ->
					 if
						 BattlePid =/= 0 ->
							  gen_server:cast(BattlePid, {player_play_cards,[RecordId,PlayerId,lists:sort(Cards), Laizi]});
						 true ->
							 case mod_table:get_alive_table_pid(BattleId) of
								 NewBattlePid when is_pid(NewBattlePid) ->
									 gen_server:cast(NewBattlePid, {player_play_cards,[RecordId,PlayerId,lists:sort(Cards), Laizi]});
								 _ ->
									 lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16006, {s2c_player_play_cards, 1, 0,[], []}, Seq)
							 end
					 end,					
					 ok;
				 true ->
					 lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16006, {s2c_player_play_cards, 1, 0,[], []}, Seq),
					 ok
			end;
		_ ->			
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16006, {s2c_player_play_cards, 1, 0,[],[]}, Seq),
			ok
	end;

%% 16009玩家重新开始
handle_cmd(16009, Seq, Status, Data) ->
	[_Uid, Isopen] = Data,
	PlayerId = Status#player.id,
	case Status#player.table_attr of
		[poke, RoomId,_ServerId, _Domain,RoomPid,_Node,BattleId,RecordId,_BattlePid,_UseNote,_IsExit] ->
			if
				 BattleId =:= 0 andalso RecordId =:= 0 ->
					 case data_room:get(RoomId) of
						 DataRoom when is_record(DataRoom, room_config) ->
							 if
								 Status#player.coin < DataRoom#room_config.start_min ->									 
									 lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16009, {s2c_player_restart, 16000002, PlayerId, Isopen}, Seq),
									 ok;
								 RoomPid =:= 0  ->
									 lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16009, {s2c_player_restart, 16000001, PlayerId, Isopen}, Seq),
									 {ok,  Status#player{table_attr = []}};									 
								 true ->	
									 PlayerData = lib_poke_room:get_player_room_game(Status),
									 [_K,_Num,Turn,_Point,_MinTime,_MaxTime] = DataRoom#room_config.table_match,
									 MatchPoint = PlayerData#player_room_game.table_point,
									 MatchInfo = lists:sublist(PlayerData#player_room_game.match_info, Turn),
									 gen_server:cast(RoomPid, {player_restart,[PlayerId, Isopen, Seq, Status#player.coin, Status#player.skin, MatchPoint, MatchInfo]}),
									 ok
							 end;
						 _ ->							 
							 lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16009, {s2c_player_restart, 16000001, PlayerId, Isopen}, Seq),
							 {ok,  Status#player{table_attr = []}}
					 end;
				 true ->
					 lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16009, {s2c_player_restart, 16000004, PlayerId, Isopen}, Seq),
					 ok
			end;
		_ ->
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16009, {s2c_player_restart, 16000004, PlayerId, Isopen}, Seq),
			
			ok
	end;

%% 16010玩家退出斗地主
handle_cmd(16010, Seq, Status, _Data) ->
%% 	io:format("==16010===~p~n", [Status#player.table_attr]),
	case Status#player.table_attr of
		[poke,RoomId,ServerId, Domain,RoomPid,_Node,_BattleId,_RecordId,_BattlePid,_UseNote,_IsExit] ->
			if
				RoomPid =:= 0  ->
					case mod_poke_room:get_mod_room_pid(RoomId,ServerId, Domain)of
						undefined ->
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16010, {s2c_player_leave, 0}, Seq),
							NewStatus = Status#player{table_attr = []},
							{ok, NewStatus};
						Pid ->	
							gen_server:cast(Pid, {player_leave, Status#player.id, Seq})
					end;
				true ->
					gen_server:cast(RoomPid, {player_leave, Status#player.id, Seq})
			end;					
		_ ->			
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16010, {s2c_player_leave, 0}, Seq),
			ok
	end;

%% 16012 玩家设置托管
handle_cmd(16012, Seq, Status, Data) ->
%% 	io:format("==16012===~p,~p~n", [Data, Status#player.table_attr]),
	[State] = Data,
	PlayerId = Status#player.id,
	case Status#player.table_attr of
		[poke, RoomId, ServerId, Domain,RoomPid,_Node,BattleId,RecordId,_BattlePid,_UseNote,_IsExit] ->
			if
				 BattleId =:= 0 andalso RecordId =:= 0 ->
					 lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16012, {s2c_player_trust, 16000012, 0, State}, Seq),					 
					 ok;
				 RoomPid =:= 0  ->
					 case mod_poke_room:get_mod_room_pid(RoomId,ServerId, Domain)of
						undefined ->
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16012, {s2c_player_trust, 16000012, 0, State}, Seq),
							NewStatus = Status#player{table_attr = []},
							{ok, NewStatus};
						Pid ->	
							gen_server:cast(Pid, {player_trust,[PlayerId, State, Seq]})
					end;					 
				 true ->
%% 					 io:format("==16012 1===~p,~p~n", [Data, Status#player.table_attr]),
					 gen_server:cast(RoomPid, {player_trust,[PlayerId, State, Seq]}),
					 ok
			end;
		_ ->			
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16012, {s2c_player_trust, 16000012, 0, State}, Seq),
			ok
	end;

%% 16013 玩家发表情
handle_cmd(16013, Seq, Status, Data) ->
	%% 	io:format("==16013===~p,~p~n", [Data, Status#player.table_attr]),
	[Id, Pos] = Data,
	PlayerId = Status#player.id,
	case Status#player.table_attr of
		[poke, RoomId, _ServerId, _Domain,_RoomPid,_Node,BattleId,RecordId,BattlePid,_UseNote,_IsExit] ->
			if
				Pos > 3 orelse Pos < 1 ->
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16013, {s2c_player_emoticon, 16000011, Id, 0, 0}, Seq),					 
					ok;
				BattleId =:= 0 andalso RecordId =:= 0 ->
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16013, {s2c_player_emoticon, 16000012, Id, Pos, 0}, Seq),					 
					ok;
				true ->
					DataRoom = data_room:get(RoomId),
					if
						Status#player.coin >= DataRoom#room_config.magic_limit ->	
							NewStatus = lib_money:cost_money(Status, DataRoom#room_config.magic_cost, ?MONEY_T_COIN, 16001, 16*1000 + RoomId),
							lib_player:send_player_coin_redcard(NewStatus),
							lib_task_event:doudizhu_use_magic_look(PlayerId, DataRoom#room_config.type, RoomId),
							gen_server:cast(BattlePid, {player_magic,[RecordId,PlayerId,Id, Pos, NewStatus#player.coin]}),							 
							{ok, NewStatus};
						true ->
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16013, {s2c_player_emoticon, 16000013, Id, Pos, 0}, Seq),
							ok
					end
			end;
		_ ->			
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16013, {s2c_player_emoticon, 16000012, Id, Pos, 0}, Seq),
			ok
	end;


%% 16014 获取比赛场信息
handle_cmd(16014, Seq, Status, Data) ->
	[RoomType] = Data,
	Ids = data_room:get_ids(),
	Fun = fun(RoomId, Acc) ->
				  case data_room:get(RoomId) of
					  DataRoom when is_record(DataRoom, room_config) ->
						  if
							  DataRoom#room_config.type =:= RoomType ->
								  [RoomId] ++ Acc;
							  true ->
								  Acc
						  end;
					  _ ->
						  Acc
				  end
		  end,
							  
				  
	RoomIds = lists:foldl(Fun, [], Ids),
	if
		RoomType =:= ?ROOM_TYPE_SPORT ->
			PlayMatchData = lib_poke_room:get_player_room_match_data(Status);
		true ->
			PlayMatchData = []
	end,
	
	if
		RoomIds =/= [] ->
			mod_poke_room:get_match_info(Status#player.id, RoomType, RoomIds, PlayMatchData, Seq),
			ok;			
		true ->
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16014, {s2c_get_match_info,16000001,[]}, Seq)
	end;	

%% 16015 报名参加比赛场
handle_cmd(16015, Seq, Status, Data) ->
	[RoomId] = Data,
	case Status#player.table_attr of
		[] ->
			case data_room:get(RoomId) of
				DataRoom when is_record(DataRoom, room_config) ->					
					case lib_poke_room:enroll_match(Status, DataRoom, RoomId, Seq) of
						{ok, NewStatus, StartTime} ->							
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16015, {s2c_enroll_match,0,RoomId, StartTime}, Seq),
							{ok, NewStatus};
						{error, Code} ->
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16015, {s2c_enroll_match,Code,RoomId, 0}, Seq);
						_ ->
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16015, {s2c_enroll_match,16000021,RoomId, 0}, Seq)
					end;				
				_ ->
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16015, {s2c_enroll_match,16000001,RoomId, 0}, Seq)
			end;
		[poke,_RoomId1,_ServerId, _Domain,_RoomPid,_Node,_BattleId,_RecordId,_BattlePid, _UseNote,_IsExit] ->
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16015, {s2c_enroll_match,11014001,RoomId, 0}, Seq);			
		_ ->			
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16015, {s2c_enroll_match,16000020,RoomId, 0}, Seq)
	end;	

%% 16017 取消报名比赛场
handle_cmd(16017, Seq, Status, Data) ->
	[RoomId] = Data,
	case Status#player.table_attr of
		[poke,RoomId,_ServerId, _Domain,RoomPid,_Node,BattleId,RecordId,_BattlePid,_UseNote,_IsExit] ->
			if
				BattleId =/= 0 andalso RecordId =/= 0 ->
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16017, {s2c_cancel_match, 16000025}, Seq),					
					ok;
				true ->
					gen_server:cast(RoomPid, {player_cancel_match, RoomId, Status#player.id, Seq})	
			end;
		_ ->			
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16017, {s2c_cancel_match, 16000024}, Seq),
			ok
	end;

%% 16019获取比赛局数信息
handle_cmd(16019, Seq, Status, Data) ->
%% 	io:format("==16017===~p~n", [Status#player.table_attr]),
	[RoomId] = Data,
	case Status#player.table_attr of
		[poke,RoomId,_ServerId, _Domain,_RoomPid,_Node,BattleId,RecordId,BattlePid,_UseNote,_IsExit] ->
			if
				 BattleId =/= 0 ->
					 gen_server:cast(BattlePid, {get_match_turn_info,[RecordId,Status#player.id, Seq]}),
					 ok;
				 true ->
					 lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16019, {s2c_get_match_turn_info, 16000019, 0, 0, 0, 0, 0}, Seq),
					 ok
			end;
		_ ->
%% 			RecordData = {s2c_get_match_turn_info, 0, Type, T2, Rank2, NewLen, TurnPoint},
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16019, {s2c_get_match_turn_info, 16000019, 0, 0, 0, 0, 0}, Seq),
			ok
	end;

%% 16024 使用记牌器
handle_cmd(16024, Seq, Status, _Data) ->
	PlayerId = Status#player.id,
	case Status#player.table_attr of
		[poke, RoomId, ServerId, Domain,RoomPid,Node,BattleId,RecordId,BattlePid,UseNote,IsExit] ->
			if
				 BattleId =:= 0 andalso RecordId =:= 0 ->
					 lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16024, {s2c_poke_use_note, 16000012, []}, Seq),					 
					 ok;
				 UseNote =:= 1 ->
					 lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16024, {s2c_poke_use_note, 16000027, []}, Seq),					 
					 ok;
				 true ->
					 DataRoom = data_room:get(RoomId),
					 case lib_goods:use_goods_by_type(Status, [{500010001,1}], 16002, 16 * 10 + DataRoom#room_config.type) of
						 {error, Code} ->
							 case lib_player_share_shop:buy_share_shop_goods_in_ddz(Status, 119, 16002, 16 * 10 + DataRoom#room_config.type) of
								 {ok, NewStatus} ->
									 lib_task_event:doudizhu_use_card_record(PlayerId, DataRoom#room_config.type, RoomId),
									 gen_server:cast(BattlePid, {poke_use_note,[PlayerId,Status#player.coin,Seq]}),							 
									 {ok, NewStatus#player{table_attr = [poke, RoomId, ServerId, Domain,RoomPid,Node,BattleId,RecordId,BattlePid,1,IsExit]}};
								 _ ->									 
									 lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16024, {s2c_poke_use_note, Code, []}, Seq),
									 ok
							 end;
						 _ ->
							 lib_task_event:doudizhu_use_card_record(PlayerId, DataRoom#room_config.type, RoomId),
							 gen_server:cast(BattlePid, {poke_use_note,[PlayerId,Status#player.coin,Seq]}),							 
							 {ok, Status#player{table_attr = [poke, RoomId, ServerId, Domain,RoomPid,Node,BattleId,RecordId,BattlePid,1,IsExit]}}
					 end
			end;
		_ ->			
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16024, {s2c_poke_use_note, 16000012, []}, Seq),
			ok
	end;

%% 16027 获取房间人数
handle_cmd(16027, Seq, Status, _Data) ->
	OnlineList = lib_game_online:get_room_num(),
	MsgRecord = [{room_num_info, ID, Num} || {ID, _, Num} <- OnlineList],
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16027, {s2c_poke_room_num, MsgRecord}, Seq),
	ok;

%% 16028 玩家触发技能buff
handle_cmd(16028, Seq, Status, SkillData) ->
	PlayerId = Status#player.id,
	[SkillId, BuffType, Data] = SkillData,
	case Status#player.table_attr of
		[poke, RoomId, _ServerId, _Domain,_RoomPid,_Node,BattleId,RecordId,BattlePid,_UseNote,_IsExit] ->
			if
				 BattleId =:= 0 andalso RecordId =:= 0 ->					 
					 lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16028, {s2c_trigger_skill, 16000012, 0, SkillId, BuffType, []}, Seq),					 
					 ok;
				 true ->
					 DataRoom = data_room:get(RoomId),					 
					 if
						 DataRoom#room_config.type =:= ?ROOM_TYPE_HERO_COM orelse DataRoom#room_config.type =:= ?ROOM_TYPE_HERO_LZ ->
							 gen_server:cast(BattlePid, {trigger_skill,[PlayerId,SkillId, BuffType, Data, Seq]}),							 
							 ok;
						 true ->
							 lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16028, {s2c_trigger_skill, 16000029, 0, SkillId, BuffType, []}, Seq),
							 ok
					 end
			end;
		_ ->	
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16028, {s2c_trigger_skill, 16000012, 0, SkillId, BuffType, []}, Seq),
			ok
	end;

%% 16030 玩家准备开始实物大奖赛
handle_cmd(16030, Seq, Status, Data) ->
	[RoomId] = Data,
	case Status#player.table_attr of
		[] ->
			case data_room:get(RoomId) of
				DataRoom when is_record(DataRoom, room_config) ->					
					case lib_poke_room:ready_enroll_match(Status, DataRoom, RoomId, Seq) of
						ok ->
							ok;
%% 							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16030, {s2c_enroll_match,0,RoomId, StartTime}, Seq),
%% 							{ok, NewStatus};
						{error, Code} ->
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16030, {s2c_enroll_match,Code,RoomId, 0}, Seq);
						_ ->
							lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16030, {s2c_enroll_match,16000021,RoomId, 0}, Seq)
					end;				
				_ ->
					lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16030, {s2c_enroll_match,16000001,RoomId, 0}, Seq)
			end;
		[poke,_RoomId1,_ServerId, _Domain,_RoomPid,_Node,_BattleId,_RecordId,_BattlePid, _UseNote,_IsExit] ->
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16030, {s2c_enroll_match,11014001,RoomId, 0}, Seq);			
		_ ->			
			lib_send:pack_and_send(Status#player.other#player_other.pid_send, 16030, {s2c_enroll_match,16000020,RoomId, 0}, Seq)
	end;

handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "handle_table no match"}.










