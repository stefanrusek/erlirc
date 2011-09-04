%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Undocumented as yet. FIXME.
%% @end
%%%-------------------------------------------------------------------
-module(irc_bot).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("irc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/2,
         connect/2, connect/3,
         connections/1,
         disconnect/2, disconnect/3,
         add_plugin/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(conf, {nick     :: string(),
               username :: string(),
               realname :: string(),
               opts     :: [{term(), term()}] }).

-record(state, {conf :: #conf{} ,
                connections,
                plugin_mgr :: pid() | atom()
               }).

-record(coninfo, {host, port}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(term(),term(),term()) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Name, Opts) ->
    case parse_conf(Opts) of
        {ok, Conf} ->
            gen_server:start_link({local, Name}, ?MODULE, [Conf], []);
        {error, What} ->
            {error, What}
    end.

connect(Name, Host) ->
    connect(Name, Host, 6667).

connect(Name, Host, Port) ->
    call(Name, {connect, Host, Port}).

connections(Name) ->
    call(Name, connections).

disconnect(Name, Host) ->
    disconnect(Name, Host, 6667).

disconnect(Name, Host, Port) ->
    call(Name, {disconnect, Host, Port}).

add_plugin(Name, Plugin, Args) ->
    call(Name, {add_plugin, Plugin, Args}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------
init([Conf]) ->
    PGMgr = case look_conf(plugin_mgr, Conf) of
                {ok, Name} ->
                    Name;
                undefined ->
                    undefined
            end,
    {ok, #state{ conf=Conf,
                 connections = dict:new(),
                 plugin_mgr  = PGMgr }}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({disconnect, Host, Port}, _From, State = #state{connections=C}) ->
    Pids = [Pid || {Pid, [#coninfo{host=H,port=P}]} <- dict:to_list(C),
                   Host == H, P == Port],
    NewC = lists:foldl(fun (Pid, D) ->
                               unlink(Pid),
                               irc_connection:send_cmd(Pid, #irc_cmd{name=quit}),
                               dict:erase(Pid, D)
                       end,
                       C,
                       Pids),
    {reply, {ok, Pids}, State#state{connections=NewC}};
handle_call({add_plugin, Plugin, Args}, _From, #state { plugin_mgr = Name } = State) ->
    Reply = irc_bot_plugin_mgr:add_plugin(Name, Plugin, Args),
    {reply, Reply, State};
handle_call(connections, _From, State = #state{connections=C}) ->
    {reply, {ok, dict:to_list(C)}, State};
handle_call({connect, Host, Port}, _From, State) ->
    {ok, Pid} = irc_connection:start_link(Host, Port, [{sendfn, fun client_cmd/2}]),
    {reply, {ok, Pid}, 
     State#state{connections=dict:append(Pid, #coninfo{host=Host, port=Port},
                                         State#state.connections)}};
handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p.", [Call]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_cast(Msg, State) -> {noreply, State} |
%%                            {noreply, State, Timeout} |
%%                            {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({client, Pid, Term}, State) ->
    PTerm = irc_cmd:parse(Term),
    handle_client_cmd(Pid, hd(dict:fetch(Pid,State#state.connections)),
                      PTerm, State);
handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_info(Info, State) -> {noreply, State} |
%%                             {noreply, State, Timeout} |
%%                             {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
    {noreply, State}.

handle_client_cmd(Pid, _Coninfo, {status, connected}, State = #state{conf=Conf}) ->
    irc_connection:send_cmd(Pid, #irc_cmd{name=nick,
                                          args=[{name, nick(Conf)}]}),
    irc_connection:send_cmd(Pid, #irc_cmd{name=user,
                                          args=[{user_name, Conf#conf.username},
                                                {real_name, Conf#conf.realname}]}),
    {noreply, State};
handle_client_cmd(_Pid, #coninfo{host=Host,port=Port},
                  #irc_cmd{name=notice,
                           args=A}, State) ->
    ?INFO("~s:~p [~s] ~s", [Host, Port,
                            proplists:get_value(facility, A),
                            proplists:get_value(text, A)]),
    {noreply, State};
handle_client_cmd(Pid, _cmdinfo, {ping, S1, _}, State) ->
    irc_connection:send(Pid, {pong, S1}),
    {noreply, State};
handle_client_cmd(Pid, #coninfo { host = Host, port = Port} = ConnInfo,
                  Command, #state { plugin_mgr = Mgr } = State) ->
    irc_bot_plugin_mgr:notify(Mgr, Pid, ConnInfo, Command),
    info(Host, Port, Command),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

call(Who, What) ->
    gen_server:call(Who, What, infinity).

client_cmd(Owner, Cmd) ->
    gen_server:cast(Owner, {client, self(), Cmd}).

nick(#state{conf=Conf}) ->
    nick(Conf);
nick(#conf{nick=Nick}) ->
    Nick.


lookup(K, L) ->
    case proplists:get_value(K, L) of
        undefined ->
            not_found;
        Otherwise ->
            {ok, Otherwise}
    end.

look_conf(K, #conf { opts = Opts }) ->
    lookup(K, Opts).

parse_conf(PL) ->
    try
        {ok, Realname} = lookup(realname, PL),
        {ok, Username} = lookup(username, PL),
        {ok, Nick}     = lookup(nick,     PL),
        {ok, #conf { realname = Realname,
                     username = Username,
                     nick     = Nick,
                     opts = PL }}
    catch
        error:{badmatch, _} ->
            {error, config_error}
    end.

info(Host, Port, Command) ->
    ?INFO("~s:~p -- command:~n ~s", [Host, Port, irc_cmd:format(Command)]).
