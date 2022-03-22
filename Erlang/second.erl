-module(second).
-export([bar/0, inc/1]).

bar() ->
    ok.

inc([Head|Tail]) ->
    % X = X+1
    [first:foo(Head) | inc(Tail)];
inc([]) ->
    [].