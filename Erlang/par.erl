-module(par).
-export([smap/2, pmap/2]).

smap(F, List) ->
    [F(E) || E <- List].


pmap(F, List) ->
    MainPid = self(),
    [spawn(fun() -> MainPid ! F(E) end) || E <- List],
    [receive 
        Value -> Value
     end || _ <- List].


% Examples 

%  par:smap(fun(X) -> X + 1 end, [1,2,3,4,5]).   Returns [2,3,4,5,6]

% par:pmap(fun(X) -> X + 1 end, [1,2,3,4,5]).    Returns [2,3,4,5,6]

