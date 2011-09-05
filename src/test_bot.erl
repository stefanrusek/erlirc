-module(test_bot).

-export([t/0]).


t() ->
    application:start(gproc),
    application:start(sasl),
    Args = [{channels, ["#testchan"]}],
    Opts = [{realname, "My Bot Realname"},
            {username, "My Bot Username"},
            {nick, "erlangbot"},
            {connections, [{"irc.baconsvin.dk", 6667}]},
            {plugins, [{irc_chan_handler, [Args]}]}],
    {ok, Pid} = irc_bot_sup:start_link(test_bot, test_bot_plgin, Opts).
