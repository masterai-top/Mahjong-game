%%------------------------------------
%% @Module     : pp_mail
%% @Author     : water
%% @Created    : 2013.02.06
%% @Description: GM/信件系统
%%------------------------------------
-module(pp_mail).
-export([handle/4, handle_cmd/4, pack_and_send/3]).

-include("common.hrl").
-include("record.hrl"). 
-include("debug.hrl").
-include("mail.hrl").

%% API Functions
handle(Cmd, Seq, Player, Data) ->
    ?TRACE("pp_mail: Cmd:~p, Player:~p, Data:~p~n", [Cmd, Player#player.id, Data]),
    handle_cmd(Cmd, Seq, Player, Data).

%%--------------------------------------
%%Protocol: 19001 系统公告
%%--------------------------------------
handle_cmd(19001, Seq, Status, [_UID]) ->
    gen_server:cast(mod_misc, {get_call_board_announce, [Status#player.other#player_other.pid_send, Seq]}),
	{ok, Status};

%% 获取邮件列表
handle_cmd(19003, Seq, Status, _) ->
    [MailList, IsNewMail] = lib_mail:get_mail(Status),
	%%io:format("~n~n~n==19003===uid:~p==mail len === ~p===~n~n~n", [Status#player.id, length(MailList)]),
	MsgRecord = {s2c_get_mail, IsNewMail, MailList},
	%%io:format("~n~n~n==19003===MsgRecord:~p===~n~n~n", [MsgRecord]),
    pack_and_send(Status, 19003, [Seq, MsgRecord]),
	{ok, Status};

%% 19005 领取附件
handle_cmd(19005, Seq, Status, [Type, MailID]) ->
    {Code, NewPS, ReturnGoods} = lib_mail:get_attachment(Status, MailID, Type),
	%%io:format("~n~n~n==19005===uid:~p==MailID: ~p==Code:~p==~n~n~n", [Status#player.id, MailID, Code]),
	MsgRecord = {s2c_get_mail_attachment, Code, ReturnGoods},
	%%io:format("~n~n~n==19005===MsgRecord:~p===~n~n~n", [MsgRecord]),
    pack_and_send(NewPS, 19005, [Seq, MsgRecord]),
	{ok, NewPS};

%% 19006 查看邮件
handle_cmd(19006, Seq, Status, [MailID]) ->
    lib_mail:read_mail(Status, MailID),
	%%io:format("~n~n~n==19006===uid:~p==MailID: ~p==~n~n~n", [Status#player.id, MailID]),
	MsgRecord = {s2c_read_mail, ?FLAG_SUCC},
	%%io:format("~n~n~n==19006===MsgRecord:~p===~n~n~n", [MsgRecord]),
    pack_and_send(Status, 19006, [Seq, MsgRecord]),
	{ok, Status};

%% 19007 删除邮件
handle_cmd(19007, Seq, Status, [Type, MailID]) ->
    Code = lib_mail:delete_mail(Status, MailID, Type),
	%%io:format("~n~n~n==19007===uid:~p==MailID: ~p==Code:~p==Type:~p==~n~n~n", [Status#player.id, MailID, Code, Type]),
	MsgRecord = {s2c_delete_mail, Code},
	%%io:format("~n~n~n==19007===MsgRecord:~p===~n~n~n", [MsgRecord]),
    pack_and_send(Status, 19007, [Seq, MsgRecord]),
	{ok, Status};



handle_cmd(Cmd, _, Status, Data) ->
    ?ERROR_MSG("Undefine handler: Cmd ~p, Status:~p, Data:~p~n", [Cmd, Status, Data]),
    {ok, error}.

pack_and_send(Player, Cmd, Data) ->
    ?TRACE("pp_mail:pack_and_send Cmd:~p, Player:~p, Data:~p~n", [Cmd, Player#player.id, Data]),
    {ok, BinData} = pt_19:write(Cmd, Data),
    lib_send:send_to_sid(Player#player.other#player_other.pid_send, BinData).

