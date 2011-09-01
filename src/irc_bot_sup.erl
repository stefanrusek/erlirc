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
-export([start_link/2]).

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
start_link(Name, PMgr) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Name, PMgr]).

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
init([Name, PlugMgrName]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    BotClient = {'IRC Bot', {irc_bot, start_link, [Name, PlugMgrName]},
                 permanent, 2000, worker, [irc_bot]},
    PluginMgr = {'Plugin Manager', {irc_bot_plugin_mgr, start_link, [PlugMgrName]},
                 permanent, 2000, worker, [irc_bot_plugin_mgr]},

    {ok, {SupFlags, [BotClient, PluginMgr]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================





