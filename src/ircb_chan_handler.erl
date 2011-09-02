%%%-------------------------------------------------------------------
%%% @author Jesper Louis andersen <>
%%% @copyright (C) 2011, Jesper Louis andersen
%%% @doc An Event Handler for Channel management
%%% @end
%%% Created :  2 Sep 2011 by Jesper Louis andersen <>
%%%-------------------------------------------------------------------
-module(ircb_chan_handler).

-include("irc.hrl").

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { channels :: [string()], % Channels to be on
                 st       :: idle | connected
               }).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% @private
init([PL]) ->
    error_logger:info_report([starting_chan_handler]),
    case proplists:get_value(channels, PL) of
        undefined ->
            {error, no_channels};
        Chans ->
            {ok, #state { channels = Chans,
                          st = idle }} 
    end.

%% @private
handle_event({msg, SendPid, _CmdInfo, isupport},
             #state { st = idle, channels = Chans } = State) ->
    error_logger:info_report([got, isupport]),
    [join_channel(SendPid, C) || C <- Chans],
    {ok, State#state { st = connected }};
handle_event({msg, _, _, _}, #state { st = idle } = State) ->
    {ok, State};
handle_event({msg, _, _, _}, #state { st = connected } = State) ->
    {ok, State}.



%% @private
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================

join_channel(SendPid, Chan) ->
    irc_connection:send_cmd(SendPid, irc_cmd:join(Chan)).

