-module(test_bot).

-export([t/0]).


t() ->
    application:start(gproc),
    application:start(sasl),
    Opts = [{realname, "My Bot Realname"},
            {username, "My Bot Username"},
            {nick, "erlangbot"}],
    {ok, _} = irc_bot_sup:start_link(test_bot, test_bot_plgin, Opts),
    Args = [{channels, ["#testchan"]}],
    irc_bot:add_plugin(test_bot, ircb_chan_handler, [Args]),
    irc_bot:connect(test_bot, "irc.baconsvin.dk").

