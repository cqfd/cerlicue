-module(cerlicue_test).
-export([run/0]).

run() ->
    application:start(cerlicue),
    {ok, Darius} = client:start_link(),
    client:nick(Darius, darius),
    {ok, Alan} = client:start_link(),
    client:nick(Alan, alan),
    client:privmsg(Alan, darius, "hi darius!"),
    client:privmsg(Darius, alan, "hi alan!"),
    cerlicue_router:join(Alan, "#hackerschool"),
    io:format("#hackerschool: ~p~n", [cerlicue_channel:names("#hackerschool")]),
    cerlicue_router:join(Darius, "#hackerschool"),
    io:format("#hackerschool: ~p~n", [cerlicue_channel:names("#hackerschool")]),
    client:privmsg(Alan, "#hackerschool", "hi everyone!"),
    application:stop(cerlicue).
