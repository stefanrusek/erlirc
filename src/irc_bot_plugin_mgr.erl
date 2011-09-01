-module(irc_bot_plugin_mgr).

-export([start_link/1,
         add_plugin/3,
         delete_handler/3]).

-export([notify/2]).

-define(SERVER, ?MODULE).

start_link(Reg) ->
    gen_event:start_link(Reg).

notify(Who, What) ->
    gen_event:notify(Who, What).

add_plugin(Who, Handler, Args) ->
    gen_event:add_sup_handler(Who, Handler, Args).

delete_handler(Who, Handler, Args) ->
    gen_event:delete_handler(Who, Handler, Args).
