%%% File    : irc_parser.erl
%%% Author  : Geoff Cant <nem@lisp.geek.nz>
%%% Description : 
%%% Created : 27 Mar 2006 by Geoff Cant <nem@lisp.geek.nz>

-module(irc_parser).

-include_lib("irc.hrl").
-include_lib("logging.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(IS_DIGIT(Var), $0 =< Var, Var =< $9).

parse_line(Line) ->
    Message = nonl(Line),
    message(Message, #irc_cmd{raw=Message}).

message([$:|Line], Cmd) ->
    {Prefix, Rest} = split(Line),
    CmdWithSource = prefix(Prefix, Cmd),
    message(Rest, CmdWithSource);
message(Line, Cmd) ->
    {CmdString, Rest} = split(Line),
    CmdWithCmd = command(CmdString, Cmd),
    params(Rest, CmdWithCmd).

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

command(Num = [D1,D2,D2], Cmd) when ?IS_DIGIT(D1),
                                    ?IS_DIGIT(D2),
                                    ?IS_DIGIT(D2) ->
    Cmd#irc_cmd{name=irc_numerics:numeric_to_atom(Num)};
command(Name, Cmd) ->
    Cmd#irc_cmd{name=irc_commands:from_list(Name)}.

params(Rest, Cmd) ->
    Args = param(Rest, 14, Cmd),
    Cmd#irc_cmd{args = lists:reverse(Args)}.

param([], _N, Args) ->
    Args;
param([$:|Line], _N, Args) ->
    [Line|Args];
param(Line, 0, Args) ->
    [Line|Args];
param(Line, N, Args) ->
    {Arg, Rest} = split(Line),
    param(Rest, N-1, [Arg|Args]).

split(Line) ->
    split($\s, Line).

split(Char, Line) when is_integer(Char) ->
    split(fun (X) when X == Char -> false; (_) -> true end, Line);
split(Fun, Line) when is_function(Fun) ->
    case lists:splitwith(Fun, Line) of
        {First, Second} when length(Second) > 0 ->            
            {First, tl(Second)};
        {First, []} ->
            {First, []}
    end.
    
split_test() ->
    ?assertMatch({"this", "is a test"}, split("this is a test")).

is_digit(D) when ?IS_DIGIT(D) ->
    true;
is_digit(_) -> false.

%% No newline (nonl)
nonl(L) -> lists:takewhile(fun(C) -> ((C /= $\r) and (C /= $\n)) end, L).

join(_, []) ->
    [];
join(_, [String]) when is_list(String) ->
    String;
join(Sep, Strings) when is_integer(Sep) ->
    join([Sep], Strings);
join(Sep, Strings) when is_list(Sep), length(Strings) > 1 ->
     join(Sep, tl(Strings), [hd(Strings)]).

join(_Sep, [], Acc) ->
    lists:append(lists:reverse(Acc));
join(Sep, [Hd|Tl], Acc) ->
    join(Sep, Tl, [Hd,Sep|Acc]).

join_test() ->
    ?assertMatch("This is a test.",
                 join($\s, ["This", "is", "a", "test."])).

join_2_test() ->
    ?assertMatch("This",
                 join($\s, ["This"])).
