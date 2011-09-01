%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(irc_server).

-behaviour(gen_server).

-include_lib("logging.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/2
         ,start/2
         ,debug/0
         ,shutdown/1
         ,gproc_name/2
         ,listen/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {net, name, users = [], channels = [], listener, self}).
-record(user, {nick, pid, ref}).
-record(chan, {name, pid, ref}).
-record(irc_server, {host,
                     net,
                     pid}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Net, Name) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Net, Name) ->
    gen_server:start_link(?MODULE, [#state{net=Net,name=Name}], []).

start(Net, Name) ->
    gen_server:start(?MODULE, [#state{net=Net,name=Name}], []).

debug() ->
    gen_server:start(?MODULE, [#state{net="localnet",name="localhost"}],
                     [{debug, [trace]}]).

gproc_name(#state{net=Net,name=Name})->
    gproc_name(Net, Name).
gproc_name(Net, Name) ->
    gproc:name({irc_server, Net, Name}).

shutdown(Server) ->
    gen_server:call(Server, shutdown).

listen(Server, Port) ->
    gen_server:call(Server, {listen, Port}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initialises the server's state
%% @end
%%--------------------------------------------------------------------
init([S = #state{}]) ->
    true = gproc:reg(gproc_name(S),self()),
    {ok, S#state{self=#irc_server{net=S#state.net,host=S#state.name,pid=self()}}}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Call message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_call({listen, Port}, _From, State = #state{listener=undefined}) ->
    F = fun (Parent, Sock) ->
                irc_s2c_fsm:sock_start(Parent, Sock, [{irc_server, State#state.self}])
        end,
    {ok, L} = tcp_server:listen(Port, [{client_handler, F}]),
    {reply, {ok, L}, State#state{listener=L}};
handle_call({listen, _Port}, _From, State = #state{listener=Pid}) when is_pid(Pid) ->
    {reply, {already_running, Pid}, State};

handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p.", [Call]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_cast(Msg, State) -> {noreply, State} |
%%                            {noreply, State, Timeout} |
%%                            {stop, Reason, State}
%% @doc Cast message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_info(Info, State) -> {noreply, State} |
%%                             {noreply, State, Timeout} |
%%                             {stop, Reason, State}
%% @doc Non gen-server message handler callbacks
%% @end
%%--------------------------------------------------------------------

%% New user
handle_info({irc, user, Ref, Pid, {nick, N, Pass}}, S = #state{users=U}) ->
    case nick_allowed(N,Pass,S) of
        {true, S2} ->
            case lists:keysearch(N, #user.nick, U) of
                {value, #user{}} ->
                    send(srv_msg({error, nicknameinuse}, Ref, S2), Pid),
                    {noreply, S2};
                false ->
                    URef = erlang:monitor(process, Pid),
                    NewUser = #user{nick=N, pid=Pid, ref=URef},
                    NewUsers = [NewUser|U],
                    send(srv_msg({ok, irc_user:gproc_name(S#state.net, N)}, Ref, S2), Pid),
                    {noreply, S2#state{users=NewUsers}}
            end;
        {false, S2} ->
            send(srv_msg({error, passwdmismatch}, Ref, S2), Pid),
            {noreply, S2}
    end;

%% Existing user changing nick
handle_info({irc, user, Ref, From = {Pid,Nick}, {nick, N}}, S = #state{users=U}) ->
    case lists:keysearch(N, #user.nick, U) of
        {value, #user{}} ->
            send(srv_msg({error, nicknameinuse}, Ref, S), From),
            {noreply, S};
        false ->
            case lists:keysearch(Nick, #user.nick, U) of
                {value, User = #user{}} ->
                    NewUser = User#user{nick=N},
                    NewUsers = [NewUser|lists:delete(User, U)],
                    send(srv_msg(ok, Ref, S), From),
                    {noreply, S#state{users=NewUsers}};
                false ->
                    URef = erlang:monitor(process, Pid),
                    NewUser = #user{nick=N, pid=Pid, ref=URef},
                    NewUsers = [NewUser|U],
                    send(srv_msg(ok, Ref, S), From),
                    {noreply, S#state{users=NewUsers}}
            end
    end;

%% User joining a channel.
handle_info({irc, channel, Ref, From, {join, Chan}},
            S = #state{channels=C}) ->
    case lists:keysearch(Chan, #chan.name, C) of
        {value, #chan{pid=P}} ->
            P ! gen_irc:msg(channel, Ref, From, join),
            {noreply, S};
        false ->
            case irc_channel:start_monitor(S#state.net, Chan) of
                {ok, CPid, CRef} ->
                    NewChan = #chan{pid=CPid,name=Chan,ref=CRef},
                    NewChans = [NewChan|C],
                    CPid ! gen_irc:msg(channel, Ref, From, join),
                    {noreply, S#state{channels=NewChans}};
                Error ->
                    ?WARN("Couldn't start channel ~p: ~p", [Chan, Error]),
                    send(gen_irc:msg(channel, Ref, S#state.name, {error, toomanychannels}), From),
                    {noreply, S}
            end
    end;

%% User or channel died.
handle_info(M = {'DOWN', Ref, process, _Pid, _}, S = #state{channels=C,users=U}) ->
    case lists:keysearch(Ref, #user.ref, U) of
        {value, User} ->
            {noreply, S#state{users=lists:delete(User,U)}};
        false ->
            case lists:keysearch(Ref, #chan.ref, C) of
                {value, Chan} ->
                    {noreply, S#state{channels=lists:delete(Chan,C)}};
                false ->
                    ?WARN("Unexpected monitor message ~p",[M]),
                    {noreply, S}
            end
    end;
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
    {noreply, State}.


nick_allowed(_Nick, _Pass, State) ->
    {true, State}.

%%--------------------------------------------------------------------
%% @private
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
%% @private
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

send(Msg, {To, _Nick}) when is_pid(To); is_atom(To) ->
    %%?INFO("To (~p)~p: ~p", [_Nick, To, Msg]),
    To ! Msg;
send(Msg, To) when is_pid(To); is_atom(To) ->
    %%?INFO("To ~p: ~p", [To, Msg]),
    To ! Msg.

srv_msg(Msg, Ref, C = #state{}) when is_reference(Ref) ->
    gen_irc:msg(server, Ref, C#state.name, Msg).
