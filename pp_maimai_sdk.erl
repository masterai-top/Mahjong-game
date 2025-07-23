%%%--------------------------------------
%%% @Module  : pp_maimai_sdk
%%% @Author  : xws
%%% @Created : 2016-10-28
%%% @Description:  充值操作
%%%--------------------------------------
-module(pp_maimai_sdk).
-include("common.hrl").
-include("record.hrl").
-include("log.hrl").
-include("debug.hrl").

-compile(export_all).

%% API Functions
handle(Cmd, Data, Socket) ->
	handle_cmd(Cmd, Data, Socket).

%% 25_maimai_sdk_account_register 注册账号
handle_cmd("25_maimai_sdk_account_register", Data, Socket) ->
	?PRINT("25_maimai_sdk_account_register Data: ~p", [Data]),
	Ip = misc:get_ip(Socket),
	[Type, Account, Password, Captcha, Device, Did] = Data,
    RetCode = lib_maimai_sdk:account_register(Type, Account, Password, Captcha, Device, Did, Ip),
	RetData = [RetCode],
	%io:format("25_maimai_sdk_account_register RetData: ~p", [RetData]),
	pack_and_send(Socket, "25_maimai_sdk_account_register", RetData);

%% 25_maimai_sdk_get_phone_captcha 获取手机验证码
handle_cmd("25_maimai_sdk_get_phone_captcha", Data, Socket) ->
	?PRINT("25_maimai_sdk_get_phone_captcha Data: ~p", [Data]),
	[PhoneNum, Type] = Data,
	Ip = misc:get_ip(Socket),
    RetCode = lib_maimai_sdk:get_phone_captcha(PhoneNum, Type, Ip),
	RetData = [RetCode, PhoneNum],
	?PRINT("25_maimai_sdk_get_phone_captcha RetData: ~p", [RetData]),
	pack_and_send(Socket, "25_maimai_sdk_get_phone_captcha", RetData);

%% 25_maimai_sdk_account_binding 账号绑定手机
handle_cmd("25_maimai_sdk_account_binding", Data, Socket) ->
	?PRINT("25_maimai_sdk_account_binding Data: ~p", [Data]),
	[PhoneNum, Account, Password, Captcha] = Data,
    RetCode = lib_maimai_sdk:account_bingding(PhoneNum, Account, Password, Captcha),
	RetData = [RetCode],
	?PRINT("25_maimai_sdk_account_binding RetData: ~p", [RetData]),
	pack_and_send(Socket, "25_maimai_sdk_account_binding", RetData);

%% 25_maimai_sdk_change_account_password 修改账号密码
handle_cmd("25_maimai_sdk_change_account_password", Data, Socket) ->
	?PRINT("25_maimai_sdk_change_account_password Data: ~p", [Data]),
	[Account, OldPassword, NewPassword] = Data,
    RetCode = lib_maimai_sdk:change_password(Account, OldPassword, NewPassword),
	RetData = [RetCode],
	?PRINT("25_maimai_sdk_change_account_password RetData: ~p", [RetData]),
	pack_and_send(Socket, "25_maimai_sdk_change_account_password", RetData);

%% 25_maimai_sdk_change_account_password_by_phone 通过手机验证修改密码
handle_cmd("25_maimai_sdk_change_account_password_by_phone", Data, Socket) ->
	?PRINT("25_maimai_sdk_change_account_password_by_phone Data: ~p", [Data]),
	[PhoneNum, Captcha, NewPassword] = Data,
    RetCode = lib_maimai_sdk:change_password_by_phone(PhoneNum, Captcha, NewPassword),
	RetData = [RetCode],
	?PRINT("25_maimai_sdk_change_account_password_by_phone RetData: ~p", [RetData]),
	pack_and_send(Socket, "25_maimai_sdk_change_account_password_by_phone", RetData);

%% 25_maimai_sdk_upgrade_account 升级账号
handle_cmd("25_maimai_sdk_upgrade_account", Data, Socket) ->
	?PRINT("25_maimai_sdk_upgrade_account Data: ~p", [Data]),
	Ip = misc:get_ip(Socket),
	[VistorAccount, Type, Account, Password, Captcha, Device, DId] = Data,
    RetCode = lib_maimai_sdk:upgrade_account(VistorAccount, Type, Account, Password, Captcha, Device, DId, Ip),
	RetData = [RetCode],
	?PRINT("25_maimai_sdk_upgrade_account RetData: ~p", [RetData]),
	pack_and_send(Socket, "25_maimai_sdk_upgrade_account", RetData);

%% 25_maimai_sdk_login 登录账号
handle_cmd("25_maimai_sdk_login", Data, Socket) ->
	?PRINT("25_maimai_sdk_login Data: ~p", [Data]),
	Ip = misc:get_ip(Socket),
	[Type, Account, Password] = Data,
    {RetCode, IsBindingPhone, Token, UId} = lib_maimai_sdk:login(Type, Account, Password, Ip),
	RetData = [RetCode, Type, IsBindingPhone, Token, UId],
%% 	io:format("25_maimai_sdk_login RetData: ~p", [RetData]),
	pack_and_send(Socket, "25_maimai_sdk_login", RetData);

%% 25_maimai_sdk_phone_login 手机登录账号
handle_cmd("25_maimai_sdk_phone_login", Data, Socket) ->
	?PRINT("25_maimai_sdk_phone_login Data: ~p", [Data]),
	Ip = misc:get_ip(Socket),
	[PhoneNum, Captcha, Device, DId] = Data,
    RetData = lib_maimai_sdk:phone_login(PhoneNum, Captcha, Device, DId, Ip),
	io:format("25_maimai_sdk_phone_login RetData: ~p", [tuple_to_list(RetData)]),
	pack_and_send(Socket, "25_maimai_sdk_phone_login", tuple_to_list(RetData));

%% 25_maimai_sdk_check_captcha 验证验证码
handle_cmd("25_maimai_sdk_check_captcha", Data, Socket) ->
	?PRINT("25_maimai_sdk_check_captcha Data: ~p", [Data]),
	[PhoneNum, Captcha] = Data,
    RetCode = lib_maimai_sdk:check_captcha(PhoneNum, Captcha),
	RetData = [RetCode, PhoneNum, Captcha],
	?PRINT("25_maimai_sdk_check_captcha RetData: ~p", [RetData]),
	pack_and_send(Socket, "25_maimai_sdk_check_captcha", RetData);

%% 容错处理
handle_cmd(_Cmd, _Socket, _Data) ->
	{error, "pp_maimai_sdk no match"}.


pack_and_send(Socket, Cmd, Data) ->
	{ok, SendData} = http_25:write(Cmd, Data),
	gen_tcp:send(Socket, SendData).








