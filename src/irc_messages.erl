%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Irc message parser library. Turns raw irc commands/responses
%% into erlang terms.
%% @end
%%%-------------------------------------------------------------------
-module(irc_messages).

-include_lib("irc.hrl").
-include_lib("logging.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([parse_args/1,
         parse_line/1,
         to_list/1,
         now_to_unix_ts/1,
         encode_ctcp_delims/1,
         decode_ctcp_delims/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------

%% @spec parse_line(Line::string()) -> #irc_cmd{}
%% @doc Converts Line to an irc_cmd record, crashing if the line isn't parsable.
%% @end 
parse_line(Line) ->
    parse_args(irc_parser:parse_line(Line)).

%%--------------------------------------------------------------------
parse_args(Cmd) when is_record(Cmd, irc_cmd) ->
    parse_args(Cmd#irc_cmd.name, Cmd#irc_cmd.args, Cmd).

parse_args(error, ":" ++ Args, Cmd) ->
    parse_error(Cmd, lists:split(string:chr(Args, $:), Args));

parse_args(Name, Args, Cmd) when Name == notice; Name == privmsg ->
    {Target, Msg} = irc_parser:split($:, Args),
    parse_notice_or_privmsg(Cmd#irc_cmd{target=string:strip(Target, right, $\s)},
                            Msg);

parse_args(needmoreparams, Args, Cmd) ->
    parse_error(needmoreparams, string:tokens(Args, ":"), Cmd);

parse_args(pass, ":" ++ Pass, Cmd) ->
    Cmd#irc_cmd{args=[{pass, Pass}]};
parse_args(pass, " " ++ Pass, Cmd) ->
    Cmd#irc_cmd{args=[{pass, Pass}]};

parse_args(server, Args, Cmd) ->
    parse_server(Cmd, Args);

parse_args(nick, Args, Cmd) ->
    parse_p10_nick(Cmd, Args);

parse_args(end_of_burst, _Args, Cmd) ->
    Cmd#irc_cmd{args = []};
parse_args(burst, Args, Cmd) ->
    parse_burst(Cmd, string:tokens(Args, " "));

parse_args(PingPong, Servers, Cmd) when PingPong == ping; PingPong == pong ->
    Cmd#irc_cmd{args=[{servers, irc_parser:split(Servers)}]};


parse_args(join, [$:|Chans], Cmd) ->
    Cmd#irc_cmd{args=[{channels, string:tokens(Chans, ",")}]};

parse_args(part, Args, Cmd) ->
    {Chans, Message} = irc_parser:split($:, Args),
    Cmd#irc_cmd{args=[{channels, string:tokens(string:strip(Chans,both,$\s), ",")},
                      {message, Message}]};

parse_args(notopic, _Args, Cmd) ->
    Cmd;
parse_args(topic, Args, Cmd) ->
    {NickChan, Topic} = irc_parser:split($:, Args),
    [Nick, Chan] = string:tokens(NickChan, " "),
    Cmd#irc_cmd{args=[{topic, Topic},
                      {channel, Chan}],
                target=Nick};

parse_args(topicinfo, Args, Cmd) ->
    case string:tokens(Args," ") of
        [Nick, Chan, Setter, Ts] ->
            Cmd#irc_cmd{target=Nick,
                        args=[{channel, Chan},
                              {topic_set_by, Setter},
                              {topic_set_at, unix_ts_to_datetime(Ts)}]}
    end;

parse_args(namreply, Arg, Cmd) ->
    case string:tokens(Arg," ") of
        [Nick, ChanType, Chan | MemberStrs] when length(MemberStrs) >= 1 ->
            Members = [string:strip(hd(MemberStrs), left, $:)|tl(MemberStrs)],
            Cmd#irc_cmd{args=[{channel, string:strip(Chan, right, $\s)},
                              {members, lists:map(fun parse_nick/1, Members)},
                              {channel_type, list_to_chantype(ChanType)}],
                        target=Nick}
    end;

parse_args(endofnames, Arg, Cmd) ->
    {NickChan, Message} = irc_parser:split($:, Arg),
    [Nick, Chan] = string:tokens(NickChan, " "),
    Cmd#irc_cmd{args=[{channel, Chan},
                      {message, Message}],
                target=Nick};

parse_args(user, Args, Cmd) ->
    parse_user(Cmd, split_one_prefix_many_space(Args));

parse_args(quit, [$:|Arg], Cmd) ->
    Cmd#irc_cmd{args=[{message, Arg}]};
parse_args(quit, [], Cmd) ->
    Cmd#irc_cmd{args=[]};

%% parse_args(Name, Args, Cmd) when Name == welcome
%%                                  ;Name == yourhost
%%                                  ;Name == created
%%                                  ;Name == myinfo
%%                                  ;Name == isupport
%%                                  ;Name == luserclient
%%                                  ;Name == luserop
%%                                  ;Name == luserchannels
%%                                  ;Name == luserme
%%                                  ;Name == luserconns
%%                                  ;Name == motdstart
%%                                  ;Name == motd
%%                                  ;Name == endofmotd
%%                                  ;Name == n_global
%%                                  ;Name == n_local
%%                                  ;Name == nomotd
%%                                  ->
parse_args(_Name, Args, Cmd) ->
    {Target, Msg} = irc_parser:split(Args),
    Cmd#irc_cmd{target=Target,
                args=[{message, string:strip(Msg, left, $:)}]}.

%%--------------------------------------------------------------------

parse_nick([$@|Nick]) ->
    {Nick, op};
parse_nick([$\%|Nick]) ->
    {Nick, halfop};
parse_nick([$+|Nick]) ->
    {Nick, voice};
parse_nick(Nick) ->
    {Nick, user}.

parse_user(Cmd, {[UserName, Mode, _Unused], RealName}) ->
    Cmd#irc_cmd{args=[{user_name, UserName},
		      {real_name, RealName},
		      {mode, Mode}]}.

list_to_chantype("@") -> secret;
list_to_chantype("*") -> private;
list_to_chantype("=") -> public.

chantype_to_list(secret) -> "@";
chantype_to_list(private) -> "*";
chantype_to_list(public) -> "=".


%%--------------------------------------------------------------------

parse_notice_or_privmsg(Cmd, Msg) ->
    case lists:member(1, Msg) of
        true -> parse_ctcp_msg(Cmd, Msg);
        false -> Cmd#irc_cmd{args=[{message, Msg}]}
    end.

parse_ctcp_msg(Cmd, Msg) ->
    {Ctcp, NonCtcp} = lists:partition(fun ({ctcp, _}) -> true; (_) -> false end,
                                      decode_ctcp_delims(Msg)),
    Cmd#irc_cmd{args=[{message,irc_parser:join(" ", lists:append([M||{_,M}<-NonCtcp]))}],
                ctcp=[parse_ctcp_cmd(C)||{_,C} <- Ctcp]}.

parse_ctcp_cmd(Msg) ->
    case irc_parser:split(Msg) of
        {Cmd,[]} ->
            #ctcp_cmd{name=irc_commands:ctcp_from_list(Cmd),
                      args=[]};
        {Cmd, Arg} ->
            Name = irc_commands:ctcp_from_list(Cmd),
            parse_ctcp_cmd(Name, #ctcp_cmd{name=Name}, Arg)
    end.

parse_ctcp_cmd(action, C, Msg) ->
    C#ctcp_cmd{args=[{action, Msg}]};
parse_ctcp_cmd(finger, C, [$:|Msg]) ->
    C#ctcp_cmd{args=[{info, Msg}]};
parse_ctcp_cmd(version, C, Msg) ->
    case string:tokens(Msg, ":") of
        [Client, Version, Environment] ->
            C#ctcp_cmd{args=[{client, Client},
                             {version, Version},
                             {environment, Environment}]};
        _ ->
            C#ctcp_cmd{args=[{unparsed, Msg}]}
    end;
parse_ctcp_cmd(userinfo, C, [$:|Info]) ->
    C#ctcp_cmd{args=[{info, Info}]};
parse_ctcp_cmd(ping, C, Ts) ->
    C#ctcp_cmd{args=[{token, Ts}]};
parse_ctcp_cmd(Cmd, _C, _) ->
    throw({not_implemented, ctcp, Cmd}).

ctcp_to_list(#ctcp_cmd{name=version, args=A}) ->
    Client = proplists:get_value(client, A, "erlirc"),
    Version = proplists:get_value(version, A, "0.0.1"),
    Environment = proplists:get_value(environment, A, "erlang"),
    io_lib:format("VERSION ~s:~s:~s", [Client, Version, Environment]);
ctcp_to_list(#ctcp_cmd{name=action, args=[{action, A}]}) ->
    "ACTION " ++ A;
ctcp_to_list(C) ->
    throw({not_implemented, ctcp_to_list, C}).

decode_ctcp_delims(Msg) ->
    decode_ctcp_delims(non_ctcp, Msg, [], []).


decode_ctcp_delims(State, This, ThisPart, Parts) when This == [1]; This == [] ->
    lists:reverse(tag_part(State, ThisPart, Parts));
decode_ctcp_delims(non_ctcp, [1|Rest], ThisPart, Parts) ->
    decode_ctcp_delims(ctcp, Rest, [], tag_part(non_ctcp, ThisPart, Parts));
decode_ctcp_delims(ctcp, [1|Rest], ThisPart, Parts) ->
    decode_ctcp_delims(non_ctcp, Rest, [], tag_part(ctcp, ThisPart, Parts));
decode_ctcp_delims(State, [This|Rest], ThisPart, Parts) ->
    decode_ctcp_delims(State, Rest, [This|ThisPart], Parts).

tag_part(_Tag, [], Parts) ->
    Parts;
tag_part(Tag, Part, Parts) ->
    [{Tag, lists:reverse(Part)}|Parts].

encode_ctcp_delims(Parts) ->
    encode_ctcp_delims(non_ctcp, Parts, []).

encode_ctcp_delims(ctcp, [], Acc) ->
    lists:reverse([1|Acc]);
encode_ctcp_delims(non_ctcp, [], Acc) ->
    lists:reverse(Acc);
encode_ctcp_delims(ctcp, [{ctcp, Part}|Parts], Acc) ->
    encode_ctcp_delims(ctcp, Parts, lists:reverse(Part) ++ [1,1|Acc]);
encode_ctcp_delims(non_ctcp, [{non_ctcp, Part}|Parts], Acc) ->
    encode_ctcp_delims(non_ctcp, Parts, lists:reverse(Part) ++ Acc);
encode_ctcp_delims(_State, [{OtherState, Part}|Parts], Acc) ->
    encode_ctcp_delims(OtherState, Parts, lists:reverse(Part) ++ [1|Acc]).

%%--------------------------------------------------------------------
parse_error(Cmd, {Reason, Text}) ->
    Cmd#irc_cmd{args = [{error, string:strip(Reason, right, $:)},
                    {text, string:strip(Text, both, $\s)}]};
parse_error(Cmd, [Reason | Text]) ->
    Cmd#irc_cmd{args = [{error, Reason}, {text, lists:append(Text)}]}.

parse_error(Code, [Info, Text], Cmd) ->
    case string:tokens(Info, " ") of
        [Target, Command] ->
            Cmd#irc_cmd{name=error,
                        target=Target,
                        args=[{code, Code},
                              {command, Command},
                              {text, Text}]}
    end.

%%--------------------------------------------------------------------
parse_server(Cmd, Args) when is_list(Args) ->
    parse_server(Cmd, irc_parser:split($:, Args));
parse_server(Cmd, {Args,Description}) ->
    parse_server(Cmd, string:tokens(Args, " "), Description).

parse_server(Cmd, [Name, HopCount, BootTS, LinkTS, Proto, [A,B|MaxClient], Flags], Description) ->
    Cmd#irc_cmd{target=#p10server{numeric=irc_numerics:p10b64_to_int([A,B]),
                              name=Name,
                              hopcount=HopCount,
                              boot_ts=BootTS,
                              link_ts=LinkTS,
                              protocol=Proto,
                              max_client=MaxClient,
                              flags=Flags,
                              description=Description}}.


%%--------------------------------------------------------------------
parse_p10_nick(Cmd, Args) when is_list(Args) ->
    parse_p10_nick(Cmd, split_one_prefix_many_space(Args));
parse_p10_nick(Cmd, {[Nick,_Something,NickTS,UserName,HostName,"+" ++ UMode,AuthName,Numeric], Description}) ->
    Cmd#irc_cmd{target=#p10user{numeric=Numeric,
                                nick=Nick,
                                nick_ts=NickTS,
                                user=UserName,
                                host=HostName,
                                authname=AuthName,
                                mode=UMode,
                                description=Description}};
parse_p10_nick(Cmd, {[Nick,_Something,NickTS,UserName,HostName,AuthName,Numeric], Description}) ->
    Cmd#irc_cmd{target=#p10user{numeric=Numeric,
                                nick=Nick,
                                nick_ts=NickTS,
                                user=UserName,
                                host=HostName,
                                authname=AuthName,
                                mode="",
                                description=Description}};
parse_p10_nick(Cmd, {[Nick], []}) ->
    Cmd#irc_cmd{args=[{name, Nick}]};
parse_p10_nick(Cmd, {[], Nick}) when is_list(Nick) ->
    Cmd#irc_cmd{args=[{name, Nick}]}.

%%--------------------------------------------------------------------
parse_burst(Cmd, [Name, ChanTS, "+" ++ ChanMode, UserData]) ->
    parse_burst_userdata(Cmd#irc_cmd{target=#chan{name=Name,
                                              chan_ts=ChanTS,
                                              mode=ChanMode}},
                         string:tokens(UserData, ":"));
parse_burst(Cmd, [Name, ChanTS, UserData]) ->
    parse_burst_userdata(Cmd#irc_cmd{target=#chan{name=Name,
                                                  chan_ts=ChanTS,
                                                  mode=""}},
                         string:tokens(UserData, ":")).

parse_burst_userdata(#irc_cmd{target = Chan} = Cmd, ["o," ++ Users | Rest]) ->
    UserNumerics = string:tokens(Users, ","),
    NewMembers = [{op, N} || N <- UserNumerics],
    OldMembers = Chan#chan.members,
    parse_burst_userdata(Cmd#irc_cmd{target=Chan#chan{members=lists:umerge(NewMembers,
                                                                           OldMembers)}},
                         Rest);
parse_burst_userdata(#irc_cmd{target = Chan} = Cmd, ["v," ++ Users | Rest]) ->
    UserNumerics = string:tokens(Users, ","),
    NewMembers = [{voice, N} || N <- UserNumerics],
    OldMembers = Chan#chan.members,
    parse_burst_userdata(Cmd#irc_cmd{target=Chan#chan{members=lists:umerge(NewMembers,
                                                                           OldMembers)}},
                         Rest);
parse_burst_userdata(#irc_cmd{target = Chan} = Cmd, ["ov," ++ Users | Rest]) ->
    UserNumerics = string:tokens(Users, ","),
    NewMembers = [{op_voice, N} || N <- UserNumerics],
    OldMembers = Chan#chan.members,
    parse_burst_userdata(Cmd#irc_cmd{target=Chan#chan{members=lists:umerge(NewMembers,
                                                                           OldMembers)}},
                         Rest);
parse_burst_userdata(#irc_cmd{target = Chan} = Cmd, [Users | Rest]) ->
    UserNumerics = string:tokens(Users, ","),
    NewMembers = [{user, N} || N <- UserNumerics],
    OldMembers = Chan#chan.members,
    parse_burst_userdata(Cmd#irc_cmd{target=Chan#chan{members=lists:umerge(NewMembers,
                                                                           OldMembers)}},
                         Rest);
parse_burst_userdata(Cmd, []) ->
    Cmd.

%%--------------------------------------------------------------------
to_list(Cmd = #irc_cmd{source=#irc_server{},
                       target=Nick}) when is_list(Nick) ->
    to_list(Cmd#irc_cmd{target=#user{nick=Nick}});
to_list(Cmd = #irc_cmd{source=#irc_server{},
                       target=#user{nick=Nick},
                       name=Name,
                       args=Args}) ->
    case irc_numerics:atom_to_numeric(Name) of
        atom_not_numeric -> to_list(Name, Args, Cmd) ++ "\r\n";
        Numeric ->
            to_list(Cmd#irc_cmd.source) ++ " "
                ++ Numeric ++ " "
                ++ Nick ++ " "
                ++ to_list(Name, Args, Cmd) ++ "\r\n"
    end;
to_list(Cmd=#irc_cmd{source=undefined,
                     target=#user{},
                     name=Name,
                     args=Args}) ->
    case irc_numerics:atom_to_numeric(Name) of
        atom_not_numeric ->
            ":" ++ to_list(Cmd#irc_cmd.target) ++ " "
                ++ to_list(Name, Args, Cmd) ++ "\r\n";
        Numeric ->
            ":" ++ to_list(Cmd#irc_cmd.target) ++ " "
                ++ Numeric ++ " "
                ++ to_list(Name, Args, Cmd) ++ "\r\n"
    end;
to_list(Cmd = #irc_cmd{}) ->
    to_list(Cmd#irc_cmd.name, Cmd#irc_cmd.args, Cmd) ++ "\r\n";
to_list(#user{} = User) ->
    user_to_list(User);
to_list(#irc_server{host=H}) ->
    [ $: | H ].

to_list(pass, Args, _Cmd) when length(Args) >= 3 ->
    Pass = proplists:get_value(password, Args),
    Ver = proplists:get_value(version, Args),
    Flags = proplists:get_value(flags, Args),
    "PASS " ++ Pass ++ " " ++ Ver ++ " " ++ Flags;
to_list(pass, [{pass, Pass}], _Cmd) ->
    "PASS " ++ Pass;

to_list(server, Args, _Cmd) when length(Args) == 4 ->
    Name = proplists:get_value(servername, Args),
    Hopcount = proplists:get_value(hopcount, Args),
    Token = proplists:get_value(token, Args),
    Info = proplists:get_value(info, Args),
    "SERVER " ++ Name ++
        " " ++ integer_to_list(Hopcount) ++
        " " ++ integer_to_list(Token) ++
        " :" ++ Info;
to_list(server, Args, _Cmd) when length(Args) == 8 ->
    Name = proplists:get_value(servername, Args),
    Hopcount = proplists:get_value(hopcount, Args),
    BootTS = proplists:get_value(boot_time, Args),
    LinkTS = proplists:get_value(link_time, Args),
    Protocol = proplists:get_value(protocol, Args),
    ServerNumeric = proplists:get_value(numeric, Args),
    MaxClient = proplists:get_value(max_client, Args),
    Info = proplists:get_value(info, Args),
    "SERVER " ++ Name ++ " " ++
        integer_to_list(Hopcount) ++ " " ++
        now_to_unix_ts_list(BootTS) ++ " " ++
        now_to_unix_ts_list(LinkTS) ++ " " ++
        Protocol ++ " " ++
        if is_list(ServerNumeric) -> ServerNumeric;
           is_integer(ServerNumeric) -> irc_numerics:int_to_p10b64(ServerNumeric, 2) end ++
        if is_list(MaxClient) -> MaxClient;
           is_integer(MaxClient) -> irc_numerics:int_to_p10b64(MaxClient, 3) end ++
        " 0 :" ++ Info;

to_list(pong, [{token, Token}], _Cmd) ->
    "PONG :" ++ Token;

to_list(nick, [{name, Name}], _Cmd) ->
    "NICK " ++ Name;

to_list(quit, [{message, Msg}], _Cmd) ->
    "QUIT :" ++ Msg;
to_list(quit, [], _Cmd) ->
    "QUIT";

to_list(join, Args, _cmd) ->
    Chans = proplists:get_value(channels, Args, []),
    Cs = case lists:partition(fun is_tuple/1, Chans) of
             {[], NonKeyed} ->
                 irc_parser:join(",", NonKeyed);
             {Keyed, NonKeyed} ->
                 irc_parser:join(",", [C || {C,_} <- Keyed] ++ NonKeyed) ++ " " ++
                     irc_parser:join(",", [K || {_C, K} <- Keyed])
         end,
    lists:flatten(["JOIN ", Cs]);

to_list(namreply, Args, _Cmd) ->
    Chan = proplists:get_value(channel,Args),
    Members = proplists:get_value(members,Args),
    Type = proplists:get_value(channel_type,Args),
    MemberList = [ lists:flatten([user_role_to_list(Role), Nick])
                   || {Nick, Role} <- Members ],
    lists:flatten([chantype_to_list(Type), " ", Chan, " :" |
                   string:join(MemberList, " ")]);
                   

to_list(Name, [{message, M}],
        #irc_cmd{target=T, ctcp = undefined}) when Name == notice;
                                                   Name == privmsg ->
    lists:flatten([irc_commands:to_list(Name), " ", nick(T), " :", M]);

to_list(Name, Args, 
        #irc_cmd{target=T, ctcp = Ctcp}) when Name == notice;
                                              Name == privmsg ->
    M = proplists:get_value(message, Args, ""),
    CtcpParts = [{non_ctcp, M}|
                 [{ctcp, ctcp_to_list(C)}||C<-Ctcp]],
    lists:flatten([irc_commands:to_list(Name),
                   " ", nick(T), " :", encode_ctcp_delims(CtcpParts)]);

to_list(part, Args, _Cmd) ->
    C = proplists:get_value(channels, Args, []),
    M = case proplists:get_value(message, Args) of
            undefined -> "";
            List -> " :" ++ List
        end,
    lists:flatten(["PART ", irc_parser:join(",", C), M]);

to_list(quit, Args, _Cmd) ->
    M = case proplists:get_value(message, Args) of
            undefined -> "";
            List -> " :" ++ List
        end,
    lists:flatten(["QUIT", M]);

to_list(user, Args, _Cmd) ->
    Name = proplists:get_value(user_name, Args),
    Mode = proplists:get_value(mode, Args, "+w"),
    RealName = proplists:get_value(real_name, Args, "Erlang Hacker"),
    lists:flatten(["USER ", Name, " ", Mode, " * :", RealName]);

to_list(welcome, [], #irc_cmd{target=User}) ->
    flatformat(":Welcome to the Internet Relay Network ~s", [to_list(User)]);
to_list(welcome, [{message, Msg}], _Cmd) ->
    ":\"" ++ Msg ++ "\"";

to_list(yourhost, Args, #irc_cmd{source=Server}) ->
    Host = proplists:get_value(host, Args, Server#irc_server.host),
    Version = proplists:get_value(version, Args, ?ERLIRC_VERSION),
    flatformat(":Your host is ~s, running version ~s", [Host, Version]);

to_list(created, Args, _Cmd) ->
    Date = proplists:get_value(created, Args, erlang:universaltime()),
    flatformat(":This server was created ~s", [iso_8601_fmt(Date)]);

to_list(myinfo, Args, #irc_cmd{source=Server}) ->
    Host = proplists:get_value(host, Args, Server#irc_server.host),
    Version = proplists:get_value(version, Args, ?ERLIRC_VERSION),
    UModes = proplists:get_value(usermodes, Args, "aios"), % XXX - the bare minumum?
    CModes = proplists:get_value(channelmodes, Args, "biklImnoPstv"), % XXX - pure lies?
    flatformat("~s ~s ~s ~s", [Host, Version, UModes, CModes]);

to_list(topicinfo, Args, #irc_cmd{source=#user{nick=Nick}}) ->
    Author = proplists:get_value(topic_set_by, Args),
    TS = proplists:get_value(topic_set_at, Args),
    Chan = proplists:get_value(channel, Args),
    string:join(" ", [irc_numeric:atom_to_numeric(topicinfo),
                      Nick,
                      Chan,
                      Author,
                      now_to_unix_ts_list(TS)]);

to_list(PingPong, [{servers, {S1, ""}}], _Cmd) when PingPong =:= ping; PingPong =:= pong ->
    string:to_upper(atom_to_list(PingPong)) ++ " " ++ S1;
to_list(PingPong, [{servers, {S1, S2}}], _Cmd) when PingPong =:= ping; PingPong =:= pong ->
    string:to_upper(atom_to_list(PingPong)) ++ " " ++ S1 ++ " " ++ S2;

to_list(unknowncommand, Args, #irc_cmd{source=_Server}) ->
    UnknownCommand = proplists:get_value(command, Args, unknowncommand),
    Message = proplists:get_value(message, Args, "Unknown command"),
    flatformat("~s :~s", [string:to_upper(atom_to_list(UnknownCommand)), Message]);

to_list(error, [{message, Msg}], _Cmd) ->
    "ERROR :" ++ Msg;

%% Catchall for simple messages.
to_list(CmdName, [{numeric, true}], _Cmd) ->
    irc_numerics:atom_to_numeric(CmdName);
to_list(_CmdName, [{message, Msg}], _Cmd) ->
    ":" ++ Msg;
to_list(CmdName, [], _Cmd) ->
    ":" ++ string:to_upper(atom_to_list(CmdName)).




numeric_to_list(Numeric, #irc_cmd{source=#irc_server{host=Server},target=#user{nick=Nick}}, Message, Args) ->
    numeric_to_list(Numeric, Server, Nick, Message, Args).
numeric_to_list(Numeric, Server, Nick, Message, Args) ->
    flatformat(":~s ~s ~s " ++ Message, 
               [Server, irc_numerics:atom_to_numeric(Numeric), Nick | Args]).

user_to_list(#user{nick=Nick,name=Uname,host={A,B,C,D}}) ->
    flatformat("~s!~s@~p.~p.~p.~p", [Nick, Uname, A,B,C,D]);
user_to_list(#user{nick=Nick,name=Uname,host=Host}) when is_list(Host) ->
    Nick ++ "!" ++ Uname ++ "@" ++ Host.

%%====================================================================
%% Internal functions
%%====================================================================

flatformat(String, Args) ->
    lists:flatten(io_lib:format(String, Args)).

iso_8601_fmt(DateTime) ->
    iso_8601_fmt(DateTime, 0).

iso_8601_fmt(DateTime, TzOffset) when TzOffset >= 0 ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B+~2.10.0B",
                  [Year, Month, Day, Hour, Min, Sec, TzOffset]);
iso_8601_fmt(DateTime, TzOffset) when TzOffset < 0 ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B-~2.10.0B",
                  [Year, Month, Day, Hour, Min, Sec, 0 - TzOffset]).

now_to_unix_ts(Tm) when is_tuple(Tm) ->
    calendar:datetime_to_gregorian_seconds(Tm) -
        calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}});
now_to_unix_ts(Tm) when is_integer(Tm) ->
    Tm.

now_to_unix_ts_list(Tm) ->
    integer_to_list(now_to_unix_ts(Tm)).

unix_ts_to_datetime(Ts) when is_list(Ts) ->
    unix_ts_to_datetime(list_to_integer(Ts));
unix_ts_to_datetime(Ts) when is_integer(Ts) ->
    Ts1970 = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    calendar:gregorian_seconds_to_datetime(Ts1970 + Ts).

split_one_prefix_many_space(Str) ->
    {SpaceSepArgs,PrefArg} = irc_parser:split($:, Str),
    {string:tokens(SpaceSepArgs, " "), PrefArg}.

nick(N) when is_atom(N) ->
    atom_to_list(N);
nick(#user{}) ->
    throw({not_implemented, nick, "#user"});
nick(N) when is_list(N) ->
    N.

user_role_to_list(op) -> "@";
user_role_to_list(halfop) -> "%";
user_role_to_list(voice) -> "+";
user_role_to_list(user) -> "".

-ifdef(EUNIT).

iso_8601_fmt_test() ->
    ?assertMatch("2007-01-01T00:00:00+00",
                 lists:flatten(iso_8601_fmt({{2007,1,1},{0,0,0}}, 0))),
    ?assertMatch("2007-01-01T00:00:00-01",
                 lists:flatten(iso_8601_fmt({{2007,1,1},{0,0,0}}, -1))).

irc_error_test() ->
    ?assertMatch(#irc_cmd{name=error},
                 parse_line("ERROR :Closing Link: erl.irc by ve.irc.dollyfish.net.nz (No C:line)\r\n")).

irc_server_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("SERVER ve.irc.dollyfish.net.nz 1 1164352162 1171089421 J10 ACAP] +h :ircd on ve\r\n")).
burst_server_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC S scorch.irc.dollyfish.net.nz 2 0 1164963563 P10 ABAP] +h :DollyNET ircd at irc.dollyfish.net.nz\r\n")).
burst_nick_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AB N Ned 2 1166709690 ~martyn 202-61-3-148.cable5.acsdata.co.nz +oiwg DKPQOU ABABc :Unknown\r\n")).
burst_service_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AB S x2.irc.dollyfish.net.nz 3 0 1164965565 P10 A0]]] +s :X2 Channel Service\r\n")).
burst_chang_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC B #wow 1167179822 ACAE[\r\n")).
burst_chan_2_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC B #blah 1164352204 +tn ABAFT,ACAKJ,ABAFQ,ACAJ9,ABAE7,ABAEp,ACAJH,ABAEf,ABABs:o,ABABc,A0AAA\r\n")).
burst_end_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC EB \r\n")).
burst_nick_2_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line("AC N shinsterw 1 1167197569 sian leibniz.catalyst.net.nz DKTvAH ACAE[ :shinster\r\n")).

ctcp_version_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line(":freenode-connect!freenode@freenode/bot/connect PRIVMSG nemerling :^AVERSION^A")).
namreply_test() ->
    ?assertMatch(":ve.irc.dollyfish.net.nz 353 nembot = #dullbots :nembot @nem\r\n",
                 to_list(parse_line(":ve.irc.dollyfish.net.nz 353 nembot = #dullbots :nembot @nem\r\n"))).

endofnames_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line(":ve.irc.dollyfish.net.nz 366 nembot #dullbots :End of /NAMES list.\r\n")).

topic_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line(":ve.irc.dollyfish.net.nz 332 nermerlin #dullbots :Foo.\r\n")).

topicinfo_test() ->
    X = parse_line(":ve.irc.dollyfish.net.nz 333 nermerlin #dullbots nem 1180326256\r\n"),
    ?assertMatch(#irc_cmd{},X),
    ?assertMatch(topicinfo, X#irc_cmd.name),
    ?assertMatch("#dullbots",
                 proplists:get_value(channel, X#irc_cmd.args)),
    ?assertMatch("nem",
                 proplists:get_value(topic_set_by, X#irc_cmd.args)),
    TS = unix_ts_to_datetime(1180326256),
    ?assertMatch(TS,
                 proplists:get_value(topic_set_at, X#irc_cmd.args)).

to_list_topicinfo_test() ->
    ?assertMatch(X when is_record(X, irc_cmd),
                 parse_line(":ve.irc.dollyfish.net.nz 333 nermerlin #dullbots nem 1180326256\r\n")).

to_list_join_test() ->
    ?assertMatch("JOIN #c1,#c2\r\n",
                 to_list(#irc_cmd{name=join,
                                  args=[{channels,
                                         ["#c1", "#c2"]}]})),
    ?assertMatch(":nem!nem@localhost JOIN #c1,#c2\r\n",
                 to_list(#irc_cmd{name=join,
                                  target=#user{nick="nem",name="nem",host="localhost"},
                                  args=[{channels,
                                         ["#c1", "#c2"]}]})).
    
decode_ctcp_delims_test() ->
    ?assertMatch([{ctcp, "VERSION"}],
                 decode_ctcp_delims([1] ++ "VERSION" ++ [1])), 
    ?assertMatch([{non_ctcp, "This is a "}, {ctcp, "VERSION"}, {non_ctcp, "test."}],
                 decode_ctcp_delims("This is a " ++ [1] ++ "VERSION" ++ [1] ++ "test.")).

encode_ctcp_delims_test() ->
    ?assertMatch([1,$V,$E,$R,$S,$I,$O,$N,1],
                 encode_ctcp_delims([{ctcp, "VERSION"}])), 
    ?assertMatch("This is a " ++ [1] ++ "VERSION" ++ [1] ++ "test.",
                 encode_ctcp_delims([{non_ctcp, "This is a "}, {ctcp, "VERSION"}, {non_ctcp, "test."}])).

nick_reply_test() ->
    ?assertMatch(#irc_cmd{name=nick, args=[{name, "nemor"}]},
                 parse_line(":nemerlng!nemerlng@121-73-3-252.cable.telstraclear.net NICK :nemor\r\n")).

user_test() ->
    Cmd = parse_line("USER nem nem localhost :Geoff Cant\r\n"),
    ?assertMatch(#irc_cmd{name=user}, Cmd),
    ?assertMatch("nem", proplists:get_value(user_name,Cmd#irc_cmd.args)),
    ?assertMatch("Geoff Cant", proplists:get_value(real_name,Cmd#irc_cmd.args)).

user_to_list_test() ->
    ?assertMatch("nem!nem@localhost", to_list(#user{nick="nem",name="nem",host="localhost"})).

pingpong_test() ->
    ?assertMatch(#irc_cmd{name=ping,args=[{servers, {"localhost", []}}]},
                 parse_line("PING localhost\r\n")),
    ?assertMatch(#irc_cmd{name=ping,args=[{servers, {"localhost", "foobar"}}]},
                 parse_line("PING localhost foobar\r\n")),
    ?assertMatch(#irc_cmd{name=pong,args=[{servers, {"localhost", []}}]},
                 parse_line("PONG localhost\r\n")),
    ?assertMatch(#irc_cmd{name=pong,args=[{servers, {"localhost", "foobar"}}]},
                 parse_line("PONG localhost foobar\r\n")).

pingpong_gen_test() ->
    ?assertMatch("PING localhost\r\n",
                 to_list(parse_line("PING localhost\r\n"))),
    ?assertMatch("PING localhost foobar\r\n",
                 to_list(parse_line("PING localhost foobar\r\n"))),
    ?assertMatch("PONG localhost\r\n",
                 to_list(parse_line("PONG localhost\r\n"))),
    ?assertMatch("PONG localhost foobar\r\n",
                 to_list(parse_line("PONG localhost foobar\r\n"))).
    

quit_test() ->
    ?assertMatch(#irc_cmd{name=quit},
                 parse_line("QUIT\r\n")),
    ?assertMatch(#irc_cmd{name=quit, args=[{message, "Foo"}]},
                 parse_line("QUIT :Foo\r\n")),
    ?assertMatch("QUIT\r\n",
                 to_list(parse_line("QUIT\r\n"))),
    ?assertMatch("QUIT :Foo\r\n",
                 to_list(parse_line("QUIT :Foo\r\n"))),
    ?assertMatch("ERROR :Foo\r\n",
                 to_list((parse_line("QUIT :Foo\r\n"))#irc_cmd{name=error})).

nomotd_to_list_test() ->
    ?assertMatch(":localhost 422 nem :NOMOTD\r\n",
                 to_list(#irc_cmd{source=#irc_server{host="localhost"},
                                  target=#user{nick="nem"},
                                  name=nomotd,
                                  args=[]})),
    ?assertMatch(":localhost 422 nem :No MOTD\r\n",
                 to_list(#irc_cmd{source=#irc_server{host="localhost"},
                                  target=#user{nick="nem"},
                                  name=nomotd,
                                  args=[{message, "No MOTD"}]})).
    

chantypes_test() ->
    ?assert(lists:all(fun (T) ->
                              T =:= list_to_chantype(chantype_to_list(T))
                      end,
                      [secret, public, private])).

-endif.

