%%%------------------------------------------------------------------
%% @copyright 2006-2011 the authors. See COPYRIGHT for details.
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @author Sean Hunt <scshunt@csclub.uwaterloo.ca>
%% @version {@vsn}, {@date} {@time}
%% @doc Basic IRC protocol parser. It approximates the ABNF in
%%      RFC 2812.
%% @end
%%%------------------------------------------------------------------


-module(irc_parser).

-include_lib("irc.hrl").
-include_lib("logging.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([parse_line/1]).

-define(IS_DIGIT(Var), $0 =< Var, Var =< $9).

%%====================================================================
%% API
%%====================================================================

%%%-------------------------------------------------------------------
%% @spec parse_line(Line::string()) -> #irc_cmd{}
%% @doc Parses Line into an irc_cmd record. If the line cannot be
%%      parsed, then it may silently succeed or crash depending on
%%      the nature of the error. Any text after the first newline
%%      character in Line is ignored.
%% @end
%%%-------------------------------------------------------------------
parse_line(Line) ->
    Message = nonl(Line),
    message(Message, #irc_cmd{raw=Message}).

%%====================================================================
%% Internal Functions
%%====================================================================

%%%-------------------------------------------------------------------
%% @spec message(Line::string(), Cmd::#irc_cmd{}) -> #irc_cmd{}
%% @private
%% @doc Parse a full message, updating Cmd with its contents
%% @end
%%%-------------------------------------------------------------------
message([$:|Line], Cmd) ->
    {Prefix, Rest} = split(Line),
    CmdWithSource = prefix(Prefix, Cmd),
    message(Rest, CmdWithSource);
message(Line, Cmd) ->
    {CmdString, Rest} = split(Line),
    CmdWithCmd = command(CmdString, Cmd),
    params(Rest, CmdWithCmd).

%% Parse a source prefix.
prefix(Prefix, Cmd) ->
    case split($@, Prefix) of
        {Name, []} ->
            case lists:member($., Name) of
                true -> Source = #irc_server{host=Name};
                false -> Source = #user{nick=Name}
            end;
        {Name, Host} ->
            {Nick, User} = split($!, Name),
            Source = #user{nick=Nick, name=User, host=Host}
    end,
    Cmd#irc_cmd{source=Source}.

%% Parse the command name or number.
command(Num = [D1,D2,D2], Cmd) when ?IS_DIGIT(D1),
                                    ?IS_DIGIT(D2),
                                    ?IS_DIGIT(D2) ->
    Cmd#irc_cmd{name=irc_numerics:numeric_to_atom(Num)};
command(Name, Cmd) ->
    Cmd#irc_cmd{name=irc_commands:from_list(Name)}.

%% Parse the parameters.
params(Rest, Cmd) ->
    Args = param(Rest, 14, Cmd),
    Cmd#irc_cmd{args = lists:reverse(Args)}.

%% Parse one parameter, including the full trailing parameter.
param([], _N, Args) ->
    Args;
param([$:|Line], _N, Args) ->
    [Line|Args];
param(Line, 0, Args) ->
    [Line|Args];
param(Line, N, Args) ->
    {Arg, Rest} = split(Line),
    param(Rest, N-1, [Arg|Args]).

%% Split a line at the first space
split(Line) ->
    split($\s, Line).

%% Split a line at the first space or point where the fun first fails.
%% Like lists:splitwith, but it drops the matching character entirely
%% rather than leaving it in the second string.
split(Char, Line) when is_integer(Char) ->
    split(fun (X) when X == Char -> false; (_) -> true end, Line);
split(Fun, Line) when is_function(Fun) ->
    case lists:splitwith(Fun, Line) of
        {First, Second} when length(Second) > 0 ->            
            {First, tl(Second)};
        {First, []} ->
            {First, []}
    end.
    
%% Retrieve the part of the line up to the first newline character.
nonl(L) -> lists:takewhile(fun(C) -> ((C /= $\r) and (C /= $\n)) end, L).
