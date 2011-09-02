%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Shell #irc_cmd creation shortcut functions.
%% @end
%%%-------------------------------------------------------------------
-module(irc_cmd).

-include_lib("irc.hrl").

%% API
-export([join/1,
         part/1, part/2,
         nick/1,
         yourhost/2,
         created/1,
         topic/0]).

-export([format_irc_cmd/1]).
%%====================================================================
%% API
%%====================================================================

format_irc_cmd(#irc_cmd{ name = Name,
                         args = Args,
                         source = Source,
                         target = Target,
                         ref = Ref, ctcp = Ctcp }) ->
    io_lib:format("Cmd: ~p~n  args: ~p~n  source: ~p~n  target: ~p~n  ref/ctcp: ~p/~p~n",
                  [Name, Args, Source, Target, Ref, Ctcp]).

join(Channel) ->
    #irc_cmd{name=join,args=[{channels, [Channel]}]}.

part(Channel) ->
    #irc_cmd{name=part,args=[{channels, [Channel]}]}.
part(Channel, Message) ->
    #irc_cmd{name=part,args=[{channels, [Channel]},
                             {message, Message}]}.

nick(Nick) ->
    #irc_cmd{name=nick,args=[{name, Nick}]}.

yourhost(Host, Version) ->
    #irc_cmd{name=yourhost,
             args=[{host, Host}, {version, Version}]}.

created({Date,Time}) ->
    #irc_cmd{name=created, args = [{created, {Date,Time}}]}.

topic() ->
    #irc_cmd{name=topic}.

%%====================================================================
%% Internal functions
%%====================================================================
