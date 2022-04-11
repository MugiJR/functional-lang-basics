-module(ring).
-export([run/1, worker/1]).

run(N) ->
    Start = self(),
    io:format("Start Process = ~p~n", [Start]),
    Last = lists:foldl(fun(_, Acc) -> spawn(ring, worker, [Acc]) end, Start, lists:seq(1, N)),
    io:format("Last Process = ~p~n", [Last]),
    Last ! ok,
    receive 
        ok -> finished
    end.


worker(Next) ->
    receive 
        ok -> Next ! ok
    end.




% Others

% >  dbg:tracer().
% >  dbg:p(new_processes, m).