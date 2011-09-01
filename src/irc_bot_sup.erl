%%%-------------------------------------------------------------------
%%% @author Jesper Louis andersen <>
%%% @copyright (C) 2011, Jesper Louis andersen
%%% @doc
%%%
%%% @end
%%% Created :  1 Sep 2011 by Jesper Louis andersen <>
%%%-------------------------------------------------------------------
-module(irc_bot_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, PMgr, Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Name, PMgr, Opts]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Name, PlugMgrName, Opts]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Opts2 = [{plugin_mgr, PlugMgrName} | Opts],
    BotClient = {'IRC Bot', {irc_bot, start_link, [Name, Opts2]},
                 permanent, 2000, worker, [irc_bot]},
    PluginMgr = {'Plugin Manager', {irc_bot_plugin_mgr, start_link, [{local, PlugMgrName}]},
                 permanent, 2000, worker, [irc_bot_plugin_mgr]},

    {ok, {SupFlags, [BotClient, PluginMgr]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================





