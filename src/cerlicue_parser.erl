-module(cerlicue_parser).
-export([parse/1, unparse/1]).

% Start by trying to parse a prefix.
parse(":" ++ Msg) ->
    case string:chr(Msg, $ ) of
        0 ->
            error;
        PrefixEnd ->
            Prefix = string:substr(Msg, 1, PrefixEnd - 1),
            Rest = string:substr(Msg, PrefixEnd + 1),
            parse(Rest, Prefix)
    end;
parse(Msg) ->
    parse(Msg, "").

% Given a prefix, try to parse a trail.
parse(Msg, Prefix) ->
    case string:str(Msg, " :") of
        0 ->
            parse(Msg, Prefix, "");
        TrailStart ->
            Trail = string:substr(Msg, TrailStart + 2),
            CommandAndArgs = string:substr(Msg, 1, TrailStart - 1),
            parse(CommandAndArgs, Prefix, Trail)
    end.

% Given a prefix and a trail, parse the command and the arguments.
parse(CommandAndArgs, Prefix, Trail) ->
    case string:tokens(CommandAndArgs, " ") of
        [Command|Args] ->
            {Prefix, Command, Args, Trail};
        _ ->
            error
    end.

% Convert an IRC parsing back into a string.
unparse({P, C, As, T}) ->
    PaddedP = pad_prefix(P),
    PaddedAs = pad_args(As),
    PaddedT = pad_trail(T),
    PaddedP ++ C ++ PaddedAs ++ PaddedT.

pad_prefix("") ->
    "";
pad_prefix(P) ->
    P ++ " ".
pad_args([]) ->
    "";
pad_args(Args) ->
    " " ++ string:join(Args, " ").
pad_trail("") ->
    "";
pad_trail(T) ->
    " :" ++ T.
