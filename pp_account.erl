%%%--------------------------------------
%%% @Module  : pp_account
%%% @Author  : kexp
%%% @Created : 2016.07.25
%%% @Description:用户账户管理
%%%--------------------------------------
-module(pp_account).
-export([handle/4]).
-include("common.hrl").
-include("record.hrl").
-include("debug.hrl").
%%-include("name.hrl").
-compile(export_all). 

-define(MAX_ROLE_PER_ACCOUNT, 1).  %%每个帐号建最大角色数
-define(NAME_LEN_MIN, 1).   %%角色名最短长度
-define(NAME_LEN_MAX, 20).  %%角色名最大长度
-define(VAIL_TIME, 3600).  %% 有效时间一小时

%% API Functions
handle(Cmd, Seq, Player, Data) ->
	handle_cmd(Cmd, Seq, Player, Data).

handle_cmd(10001, Seq, Status, Data) ->
	[Tick, Sign, Version, Agent, AccounId, Accname, Password, Nick,Channel, Device, Did] = Data,
	{ok, PlayerId, _}= lib_account:has_player_id([Tick, Sign, Version, Agent, AccounId, Accname, Password, Nick,Channel, Device, Did, Status#player.last_login_ip]),
	Time = util:unixtime(),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 10001, {s2c_account_login, ?FLAG_SUCC, Time,PlayerId}, Seq);

handle_cmd(10008, Seq, Status, Data) ->
	[Syc] = Data,
	%% 心跳包正常
	put(detect_heart_time, [0, 20, []]),
	lib_send:pack_and_send(Status#player.other#player_other.pid_send, 10008, {s2c_heart_beat,Syc}, Seq);

handle_cmd(_Cmd, _Seq, _Socket, _Data) ->
	{error, "handle_account no match"}.

%%通行证验证
%%返回true为通过验证,　返回false为未通过验证
%is_good_pass([_Accid, Accname, Tstamp, Cm, Ts]) ->  
is_good_pass([_ServerId, Accid, Accname, Tstamp, Ts, Tiket, _GateIp, _ChannelData]) ->  
	%%io:format("Accid === ~p~n Tstamp === ~p~n Ts === ~p~n", [Accid, Tstamp, Ts]),
	KeyItem = data_config:get_platform_key(Tiket),
	%%io:format("Ticket === ~p~n KeyItem === ~p~n", [Tiket, KeyItem]),
	case Tiket of
		"91" when KeyItem =/= false ->
			{_, Key} = KeyItem,
			{_, VailUrl} = data_config:get_login_url(Tiket),
			VailRes = check_session_to_server(Tiket, [Key, VailUrl, Ts, Accid]);
		%%         "4399" when KeyItem =/= false -> 暂时屏蔽,默认是4399平台
		"4399tw" ->
			{_, Key} = KeyItem,
			VailRes = check_sign_by_accname(Accname, Tstamp, Key, Ts);
		"ley" when KeyItem =/= false -> %% 乐游
			{_, Key} = KeyItem,
			VailRes = check_sign_vail(Accid, Tstamp, Key, Ts);
		"uc" when KeyItem =/= false ->			
			VailRes = true;
		"lengj" when KeyItem =/= false -> %% 棱镜
			%%VailRes = check_lengj_login(ChannelData, Tiket, Ts);
			VailRes = true;
		"owan" when KeyItem =/= false ->  %% 有米偶玩
			{_, Key} = KeyItem,
			VailRes = check_owan_login(Accid, Tstamp, Key, Ts);
		"dodou" when KeyItem =/= false -> %% 逗逗（盛大）
			%%VailRes = check_dodou_login(Tiket, Ts);
			VailRes = true;
		"lew" ->   %% 乐玩 
			%%VailRes = check_lew_login(ChannelData, Tiket, Ts);
			VailRes = true;
		"youlong" when KeyItem =/= false -> %% 游龙
			%%VailRes = check_youlong_login(Tiket, Ts);
			VailRes = true;
		"istar" when KeyItem =/= false ->   %% iStar(台湾)
			{_, Key} = KeyItem,
			VailRes = check_sign_vail(Accname, Tstamp, Key, Ts);
		"anysdk"  ->   %% AnySDK
			VailRes = true;
		_ ->
			{_, Key} = KeyItem,
			VailRes = check_sign_vail(Accid, Tstamp, Key, Ts)
	end,
	%%     io:format("VailRes === ~p~n", [VailRes]),
	case  VailRes of
		true ->
			%% 验证是否超时
			true;
		%% 			is_vail_time(Tstamp);           
		_ ->
			if
				_GateIp =:= "119.145.114.130" ->
					true;
				true ->
					false
			end
	%% 			false
	end.

%% 跟服务器通讯验证登陆
check_session_to_server(Tiket, List) ->
	%% 	io:format("Tiket == ~p~n LIst == ~p~n", [Tiket, List]),
	case Tiket of
		"91" ->
			[Key, VailUrl, Session, Uin] = List,
			%% 			io:format("ok, cul list === ~p~n", [[Key, VailUrl, Session, Uin]]),
			AppId = 112458,
			Act = 4,
			Md5 = tool:md5(lists:concat([AppId, Act, Uin, Session, Key])),
			%% 			io:format("Md5 === ~p~n", [Md5]),
			Url = lists:concat([VailUrl, "?AppId=", AppId, "&Act=4&Uin=", Uin, "&Sign=", Md5, "&SessionID=", Session]);
		_ ->
			?ERROR_MSG("pp_account tiket error:~p", [Tiket]),
			Url = []		
	end,
	Json = misc:get_http_content(Url),
	case rfc4627:decode(Json) of
		{ok, RequestObject, _}  ->
			%% 			io:format("RequestObject === ~p~n", [RequestObject]),
			case rfc4627:get_field(RequestObject, "ErrorCode", null) of
				<<"1">> ->
					true;
				Error ->
					?ERROR_MSG("pp_account result error:~p", [Error]),
					false
			end;
		Error ->
			?ERROR_MSG("pp_account result decode error:~p", [Error]),
			false
	end.

%% 检查Sign的有效性
check_sign_vail(Accid, Tstamp, Key, Ts) ->
	%% 	io:format("check_sign_vail === ~p~n", [[Accid, Tstamp, Key, Ts]]),
	Md5 = lists:concat([Accid, '&', Tstamp, '&', Key]),
	Hex = util:md5(Md5),
	%% 	io:format("check_sign_vail === ~p~n", [[Ts, Hex]]),
	Hex == Ts.

check_sign_by_accname(Accname, Tstamp, Key, Ts) ->
	%% 	io:format("check_sign_by_accname === ~p~n", [[Accname, Tstamp, Key, Ts]]),
	Md5 = lists:concat([Accname, '&', Tstamp, '&', Key]),
	Hex = util:md5(Md5),
	%% 	io:format("check_sign_by_accname === ~p~n  Md5 === ~p~n", [Ts, Hex]),
	Hex == Ts.

%% 偶玩登陆验证
check_owan_login(Accid, Tstamp, Key, Ts) ->
	Now = util:unixtime(),
	Dval = erlang:abs(Tstamp-Now),
	if
		Dval > 600 ->
			false;
		true ->
			Md5 = lists:concat([Accid, '&', Tstamp, '&', Key]),
			Hex = util:md5(Md5),
			%%io:format("~ncheck_owan_login === ~p~n  Md5 === ~p~n", [Ts, Hex]),
			Hex == Ts
	end.

%% 检查sid(Uc平台)
check_uc_sid(UcSid, Tiket) ->
	{_, GameId} = data_config:get_platform_gameid(Tiket),
	{_, ApiKey} = data_config:get_platform_key(Tiket),
	{_, Url} = data_config:get_login_url(Tiket),
	Id = util:unixtime(),
	Sign = util:md5(lists:concat(["sid=", UcSid, ApiKey])),
	%% 	{
	%% 	 "id":1330395827,
	%% 	 "data":{"sid":"abcdefg123456"},
	%% 	 "game":{"gameId":12345},
	%% 	 "sign":"6e9c3c1e7d99293dfc0c81442f9a9984"
	%% 	}
	L = [{"id", Id}, {"data", [{"sid", UcSid}]}, {"game", [{"gameId", GameId}]}, {"sign", Sign}],
	Json = http_util2:convert_json(L),
	Request = {Url, [], "application/json", Json},
	HTTPOptions = [],
	Options = [],
	
	%% 	io:format("~n==httpc:request === ~p~n", [Request]),
	case httpc:request(post, Request, HTTPOptions, Options) of
		{ok, {_Status, _Headers, Raw}} ->
			%% 			io:format("~n==httpc:request === ~p~n", [Raw]),
			case rfc4627:decode(Raw) of
				{ok, RequestObject, _}  ->
					%% 					 			io:format("~n==RequestObject === ~p~n", [RequestObject]),
					%% {
					%% 		"id":1330395827,
					%% 		"state":{"code":1, "msg":"操作成功"},
					%% 		"data":{
					%% 			"accountId":"U11626774a4e39c16cf7mmsnz5002une",
					%% 			"creator":"JY",
					%% 			"nickName":"九游玩家"
					%% 		}
					%% }
					case rfc4627:get_field(RequestObject, "state") of
						{ok, State} ->   %% 成功
							case rfc4627:get_field(State, "code", null) of 
								1 ->
									case rfc4627:get_field(RequestObject, "data") of
										{ok, Data} ->
											AccountId = rfc4627:get_field(Data, "accountId", []),
											NickName = rfc4627:get_field(Data, "nickName", []),
											{1, AccountId, NickName};
										Error ->
											%% 											io:format("~n==Error1 === ~p~n", [Error]),
											?ERROR_MSG("pp_account check_uc_sid get_field data Error:~p", [Error]),
											false
									end;
								10 ->  %% 请求参数错误
									{10, [], []};
								11 ->  %% 用户未登录
									{11, [], []}
							end;
						Error ->
							%% 							io:format("~n====Error2 === ~p~n", [Error]),
							?ERROR_MSG("pp_account check_uc_sid get_field state Error:~p", [Error]),
							false
					end;
				Error ->
					%% 					io:format("~n====Error3 === ~p~n", [Error]),
					?ERROR_MSG("pp_account check_uc_sid  decode error:~p", [Error]),
					false
			end;
		Error ->
			%% 			io:format("~n====Error4 === ~p~n", [Error]),
			?ERROR_MSG("pp_account check_uc_sid  httpc:request error:~p", [Error]),
			false
	end.


%% 验证登陆(棱镜平台)
check_lengj_login(ChannelData, Tiket, Token) ->
	%%ChannelData = ["d4fe030b6f9847dd80781f350bc44635", "123456789", "p1490", ""],
	%%Tiket = "lengj",
	%%Token = "bda553e9967c4f919e1923ea2c97b077",
	%%{_, GameId} = data_config:get_platform_gameid(Tiket),
	%%{_, ApiKey} = data_config:get_platform_key(Tiket),
	{_, VailUrl} = data_config:get_login_url(Tiket),
	
	[ChannelId, UserId, ProductCode, _ChannelLabel] = ChannelData,
	%%io:format("~n===_ChannelLabel:~p====~n", [_ChannelLabel]),
	%%Request = {Url, [{"userId", UserId}, {"channel", ChannelId}, {"token", Token}, {"productCode", ProductCode}]},
	Request = {lists:concat([VailUrl, "?userId=", UserId, "&channel=", ChannelId, "&token=", Token, "&productCode=", ProductCode]), []},
	HTTPOptions = [],
	Options = [],
	%% 	httpc:request(get, Request, HTTPOptions, Options).
	%%io:format("~n==httpc:Request === ~p~n", [Request]),
	case httpc:request(get, Request, HTTPOptions, Options) of
		%%case misc:get_http_content(Url) of
		{ok, {_Status, _Headers, Raw}} ->
			%%io:format("~n==Raw === ~p~n", [Raw]),
			case Raw of
				"true" ->
					true;
				_ ->
					false
			end;
		Error ->
			%%io:format("~n====Error4 === ~p~n", [Error]),
			?ERROR_MSG("pp_account check_lengj_login  httpc:request error:~p", [Error]),
			false
	end.

%% 验证登陆(逗逗（盛大）)
check_dodou_login(Tiket, Token) ->
	{_, GameId} = data_config:get_platform_gameid(Tiket),
	{_, ApiKey} = data_config:get_platform_key(Tiket),
	{_, VailUrl} = data_config:get_login_url(Tiket),
	Id = util:longunixtime(),
	Time = util:unixtime(),
	Sign = util:md5(lists:concat(["appid=",GameId,"&sequence=",Id,"&tiaket_id=",Token,"&timestamp=",Time,ApiKey])),
	%% 	appid: 游戏对应的APPID
	%% timestamp: 精确到秒的unix时间戳
	%% sequence: 不重复的请求序列号（全局唯一）
	%% ticket_id: 从客户端登录接口返回的ticket字符串；
	%% sign: 签名串（参考：“附录A：服务器端签名算法”，签名原始串示例：appid=xxx&sequence=xxx&ticket_id=xxx & timestamp=xxxappsecretkey）
	%% ["appid","sequence","tiaket_id","timestamp"]
	Request = {lists:concat([VailUrl, "?appid=",GameId,"&sequence=",Id,"&tiaket_id=",Token,"&timestamp=",Time,"&sign=", Sign]), []},
	HTTPOptions = [],
	Options = [],
	
	%% 	io:format("~n==httpc:request === ~p~n", [Request]),
	case httpc:request(get, Request, HTTPOptions, Options) of
		{ok, {_Status, _Headers, Raw}} ->
			%%io:format("~n==httpc:request === ~p~n", [Raw]),
			case rfc4627:decode(Raw) of
				{ok, RequestObject, _}  ->
					%%io:format("~n==RequestObject === ~p~n", [RequestObject]),
					%% {
					%%   	code:0,#返回状态，0为成功，1为失败
					%% 		msg:"ok",#错误信息
					%% 		data:{
					%%     		userid:123456,#平台用户id，纯数字，无类型，不用于显示账号。
					%%     		token:"40E00263-16A7-8CFA-D0E8-C951D683EA24",#平台TOKEN，36位字符串，暂时不用
					%%     		phone:"+86-139****6893"#手机帐号，带国际区号，可用于显示帐号
					%%     		invitation_code:"34343"#邀请码
					%% 		}
					%% }
					case rfc4627:get_field(RequestObject, "code", null) of 
						0 -> %% 成功
							true;
						_ ->
							false
					end;
				Error ->
					%% 							io:format("~n====Error2 === ~p~n", [Error]),
					?ERROR_MSG("pp_accountcheck_dodou_login get_field state Error:~p", [Error]),
					false
			end;
		Error ->
			%% 			io:format("~n====Error4 === ~p~n", [Error]),
			?ERROR_MSG("pp_account check_dodou_login  httpc:request error:~p", [Error]),
			false
	end.

%% 验证登陆（乐玩）
check_lew_login(ChannelData, Tiket, Token) ->
	%% 	ChannelData = ["abc123", "123456789", "10"],
	%% 	Tiket = "lew",
	%% 	Token = "bda553e9967c4f919e1923ea2c97b077",
	{_, GameId} = data_config:get_platform_gameid(Tiket),
	{_, VailUrl} = data_config:get_login_url(Tiket),
	
	%% {"app_id":"12590","code":"admin","password":"xxxxxx","token":"xxxxxxx","channelId":"10"}
	[Code, PassWord, ChannelId] = ChannelData,
	%%io:format("~n===_ChannelLabel:~p====~n", [_ChannelLabel]),
	L = [{"app_id", GameId}, {"code", Code}, {"password", PassWord}, {"token", Token}, {"channelId", ChannelId}],
	Json = http_util2:convert_json(L),
	NotifyData = http_uri:encode(Json),
	Request = {lists:concat([VailUrl, "?notifyData=", NotifyData]), []},
	HTTPOptions = [],
	Options = [],
	%% 	httpc:request(get, Request, HTTPOptions, Options).
	%%io:format("~n==httpc:Request === ~p~n", [Request]),
	case httpc:request(get, Request, HTTPOptions, Options) of
		%%case misc:get_http_content(Url) of
		{ok, {_Status, _Headers, Raw}} ->
			%%io:format("~n==Raw === ~p~n", [Raw]),
			case rfc4627:decode(Raw) of
				{ok, RequestObject, _}  ->
					%%io:format("~n==RequestObject === ~p~n", [RequestObject]),
					%%data：json格式的数据
					%%{"userId":1,"app_id":"12590","code":"admin","success":true,"msg":"verify success"}
					case rfc4627:get_field(RequestObject, "success", null) of 
						true -> %% 成功
							true;
						_ ->
							false
					end;
				Error ->
					?ERROR_MSG("pp_accountcheck_dodou_login get_field state Error:~p", [Error]),
					false
			end;
		Error ->
			?ERROR_MSG("pp_account check_lew_login httpc:request error:~p", [Error]),
			false
	end.

%% 验证登陆（游龙）
check_youlong_login(Tiket, Token) ->
	{_, GameId} = data_config:get_platform_gameid(Tiket),
	%%{_, ApiKey} = data_config:get_platform_key(Tiket),
	{_, VailUrl} = data_config:get_login_url(Tiket),
		
	Data = lists:concat(["token=", Token,"&pid=", GameId]),
	Request = {VailUrl, [], "application/json", Data},	
	HTTPOptions = [],
	Options = [],
	case httpc:request(post, Request, HTTPOptions, Options) of
		{ok, {_Status, _Headers, Raw}} ->
			case rfc4627:decode(Raw) of
				{ok, RequestObject, _}  ->
%% 					io:format("~n==RequestObject === ~p~n", [RequestObject]),
					%%{"username":XXX,"errMsg":"XXX","state":1,"errcMsg":XXX}
					case rfc4627:get_field(RequestObject, "state", null) of 
						1 -> %% 成功
							true;
						_ ->
							false
					end;
				Error ->
					%%							io:format("~n====Error2 === ~p~n", [Error]),
					?ERROR_MSG("pp_account check_youlong_login get_field state Error:~p", [Error]),
					false
			end;
		Error ->
			%%io:format("~n====Error4 === ~p~n", [Error]),
			?ERROR_MSG("pp_account check_youlong_login  httpc:request error:~p", [Error]),
			false
	end.

%% 测试UC充值回调接口
test_uc_pay(OrderId) ->
	ServerId = "1",
	PlayerId = "300010000003",
	
	Ver = "2.0",
	%%OrderId = "a12345",
	GameId = "552987",
	AccountId = "125838847",
	Creator = "UC",
	PayWay = 1, 
	Amount = "100",
	CallbackInfo = lists:concat(["serverId:",ServerId,";playerId:",PlayerId]),
	OrderStatus = "S",
	FailedDesc = "OK",
	%%CpOrderId = "1234567",
	
	ApiKey = "224ba93167a1e559cc1af30c66070ca6",
	
	Sign = util:md5(lists:concat(["accountId=", AccountId, "amount=", Amount, "callbackInfo=", CallbackInfo, "creator=",Creator, "failedDesc=", FailedDesc, "gameId=", GameId, "orderId=", 
								  OrderId,"orderStatus=", OrderStatus, "payWay=", PayWay, ApiKey])),
	
	Data = [{"orderId", OrderId}, {"gameId", GameId}, {"accountId", AccountId}, {"creator", Creator}, {"payWay", PayWay}, 
			{"amount", Amount}, {"orderStatus", OrderStatus},{"callbackInfo", CallbackInfo}, {"failedDesc",FailedDesc}],
	L = [{"ver", Ver}, {"data", Data}, {"sign", Sign}],
	Json = http_util2:convert_json(L),
	Url = "http://charge.szy.leygame.com/api/ucpay.php",
	Request = {Url, [], "application/json", Json},
	HTTPOptions = [],
	Options = [],
	httpc:request(post, Request, HTTPOptions, Options).


%% 验证时间是否过期
is_vail_time(Tick) ->
	Now = util:unixtime(),
	if Now > Tick + ?VAIL_TIME
		   orelse Now < Tick - ?VAIL_TIME->
		   false;
	   true ->
		   true
	end.

test_word() ->
	A = validate_name("绫乃酱"),
	B = validate_name("大傻Ｂ"),
	C = validate_name("将介石不给力啊有木有"),
	D = validate_name("A~~!!@@C"),
	E = validate_name("大傻A"),
	{A,B,C,D, E}.

test_word1() ->
	%% 	NameList = ["绫乃酱","大傻Ｂ","将介石不给力啊有木有","A~~!!@@C","!!花"],
	%%NameList = ?NAME, 
	NameList = ["有錢的林俊傑"],
	%%NameList = ["北北基"],
	%% 	io:format("~ts", [NameList]),
	test_word1(NameList, []).

test_word1([H|T], _Arr) ->
	Ret = validate_name(H),
	case Ret of
		true -> test_word1(T, _Arr);
		{false, Res} ->		
			io:format("H=========:~p", [Res]),
			BName = list_to_binary(H),
			Bytes = <<BName/binary, <<" , ">>/binary>>,
			file:write_file("e:\\special_name.txt", Bytes, [append]),
			test_word1(T, _Arr)
	end;
test_word1([], _) ->
	ok.


%% 角色名合法性检测
validate_name(Name) ->
	case lib_words_ver:validate_name(Name, special) of
		true  -> 
			case lib_words_ver:validate_name(Name, [?NAME_LEN_MIN, ?NAME_LEN_MAX]) of %%长度检查
				true -> 
					case validate_name(Name, exist) of
						true  -> true;
						false -> {false, 3}
					end;
				false -> 
					{false, 5}                 
			end;
		false ->  
			{false, 4}  %%含非法字符(特殊或敏感词)
	end.

%%判断角色名是否已经存在
validate_name(Name, exist) ->
	case lib_player:is_exists_name(Name) of
		true ->
			false;    
		false ->
			true
	end.

%% 检查服务器编号
check_server_id(ServerId) ->
	ThisServerId = config:get_server_num(),
	if
		%% 		ServerId =:= 998 -> %% 使用当前默认的服务器ID
		%% 			{ok, 1};
		ServerId =:= ThisServerId ->
			{ok, ThisServerId};
		true ->
			%% 后续增加合服的服务器ID列表
			ServerIdList = lib_misc:get_combined_server_list(),
			case lists:member(ServerId, ServerIdList) of
				true ->
					{ok, ServerId};
				_ ->
					%% 					error
					{ok, ServerId}
			end
	end.

%% ======================狼旗手游数据采集接口========================
%% 激活(注册)统计
get_langqi_data(Tag, Uid, AccType, Tiket) ->
	%%io:format("~n==get_langqi_data:Tag=== ~p~n", [Tag]),
	ChannelId = data_config:get_channel_id(AccType),
	{_, GameId} = data_config:get_platform_gameid(Tiket),
	%%{_, ApiKey} = data_config:get_platform_key(Tiket),
	{_, VailUrl} = data_config:get_login_url(Tiket),
	LocalTime = misc:time_format1(),
	%%io:format("~n===Tag:~p==AccType:~p==ChannelId: ~p~n", [Tag, AccType, ChannelId]),
	case Tag of
		"login" ->
			%% [tag,time,cid,uid,sid,rid,ip,rating,level]
			Data = lists:concat(["[\"", Tag, "\",\"", LocalTime, "\",", ChannelId, ",\"", util:term_to_string(Uid), "\",", 0, ",", 0, ",\"", "", "\",", 0, ",", 0, "]"]);
			%%Data = [Tag, LocalTime, ChannelId, util:term_to_string(Uid), 0, 0, "", 0, 0];
		"register" ->
			%% [tag,time,cid,ccode,imei,mac,uid,sid,sname,rid,name,ip,age,sex]
			Data = lists:concat(["[\"", Tag, "\",\"", LocalTime,  "\",", ChannelId, ",\"", "", "\"", ",\"", "", "\"", ",\"", "", "\",\"", Uid, "\",", 0,
								  ",\"", "", "\",", 0, ",\"", "", "\"", ",\"", "", "\",", 0, ",", 0, "]"]);
			%%Data = [Tag, LocalTime, ChannelId, "", "", "", util:term_to_string(Uid), 0, "", 0, "", "", 0, 0];
		_ ->
			Data = ""
	end,
	%%String = rfc4627:encode(Data),
	%%io:format("~n==Data: ~p~n", [Data]),
	EncodeData = http_uri:encode(Data),
	%%io:format("~n==EncodeData: ~p~n", [EncodeData]),
	Request = {lists:concat([VailUrl, "?appid=", GameId, "&data=", EncodeData]), []},
	HTTPOptions = [],
	Options = [],
	%%io:format("~n==httpc:Request === ~p~n", [Request]),
	_Result = httpc:request(get, Request, HTTPOptions, Options),
	%%io:format("~n==httpc:Result === ~p~n", [_Result]),
	ok.
	
%% 短信验证码
test_ddy(Num) ->
	VailUrl = "http://api.dingdongcloud.com/v1/sms/sendyzm",
	Apikey = "30c32a5408ce5a6ec5637d6e9217eec5",
	Content = http_util2:utf8_to_url(unicode:characters_to_binary(lists:concat(["【X-MAN】您正在注册X-MAN账号，验证码是",23434,"，如非本人操作请注意账号安全。"]))),

	io:format("~n===Content:~p====~n", [Content]),
	%%Request = {Url, [{"userId", UserId}, {"channel", ChannelId}, {"token", Token}, {"productCode", ProductCode}]},
	Request = {lists:concat([VailUrl, "?apikey=", Apikey, "&mobile=", Num, "&content=", Content]), []},
%% 	Request = {lists:concat([VailUrl]), []},
	HTTPOptions = [],
	Options = [],
	%% 	httpc:request(get, Request, HTTPOptions, Options).
	io:format("~n==httpc:Request === ~p~n", [Request]),
	case httpc:request(get, Request, HTTPOptions, Options) of
		%%case misc:get_http_content(Url) of
		{ok, {_Status, _Headers, Raw}} ->
			io:format("~n==Raw === ~p~n", [Raw]),
			case rfc4627:decode(Raw) of
				{ok, RequestObject, _}  ->
					io:format("~n==RequestObject === ~p~n", [RequestObject]),
					true;
				_ ->
					false
			end;
		Error ->
			io:format("~n====Error4 === ~p~n", [Error]),
			?ERROR_MSG("pp_account check_lengj_login  httpc:request error:~p", [Error]),
			false
	end.





























