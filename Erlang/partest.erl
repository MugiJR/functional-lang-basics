-module(partest).
-export([smap/2, pmap/2, fib/1, ord_pmap/2]).


smap(F, List) ->
    [F(E) || E <- List].


pmap(F, List) ->
    MainPid = self(),
    [spawn(fun() -> MainPid ! F(E) end) || E <- List],
    [receive 
        Value -> Value
     end || _ <- List].

ord_pmap(F, List) ->
    MainPid = self(),
    Pids = [spawn(fun() -> MainPid ! {self(), F(E)} end) || E <- List],
    [receive 
        {Pid, Value} -> Value
    end || Pid <- Pids].


fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).


% Additional Tips:

%   1. Calculate the total time taken by a function (Using Timer moduler)
%       > timer:tc(partest, smap, [fun(X) -> X + 1  end, lists:seq(1,10000)]).



% Others

% >  partest:smap(fun partest:fib/1, [35])