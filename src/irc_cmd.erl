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
         pong/1,
         nick/1,
         yourhost/2,
         created/1,
         topic/0]).

-export([parse/1]).
-export([format/1]).

-type status() :: connected.

-type t() :: {ping, string(), string()}
           | isupport
           | {status, status()}
           | #irc_cmd{}.
-export_type([t/0, status/0]).

%%====================================================================
%% API
%%====================================================================

parse(connected) -> {status, connected};
parse(#irc_cmd { name = ping, args = [{servers, {Server1, Server2}}] }) ->
    {ping, Server1, Server2};
parse(#irc_cmd { name = isupport }) ->
    isupport;
parse(#irc_cmd{} = Cmd) ->
    Cmd.

format({ping, _, _}) -> "PING!";
format(isupport) -> "ISUPPORT";
format({status, Status}) -> "STATUS CHANGE --> " ++ atom_to_list(Status);
format(#irc_cmd{ name = Name,
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

pong(ServerToken) ->
    #irc_cmd { name = pong,
               args = [{servers, {ServerToken, ""}}] }.

%%====================================================================
%% Internal functions
%%====================================================================
