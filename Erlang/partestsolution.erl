


-module(exam).
-export([applyAllPar/2, speculativeEval/2, fib/1, zip/2, apply_fun/2]).

%% Task 1: Parallel apply all

applyAllPar(FS, LS) ->
    MainPid = self(),
    Pids = [spawn(fun() -> MainPid ! {self(), F(L)} end) || F <- FS, L <- LS],
    [
        receive 
            {Pid, Value} -> Value
        end
    || Pid <- Pids 
    ].



fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

zip([], []) -> [];
zip([], _) -> [];
zip(_, []) -> [];
zip([H1|T1], [H2|T2]) ->
    [{H1, H2} | zip(T1, T2)].

apply_fun(F, Arg) ->
    try
        F(Arg)
    catch
        error:_ -> ok
    end.    

speculativeEval([], []) ->
    no_proper_result;
speculativeEval([], _) ->
    no_proper_result;
speculativeEval(_, []) ->
    no_proper_result;
speculativeEval(FS, LS) ->
    MainPid = self(),
    Pids = [spawn(fun() -> MainPid ! apply_fun(F, L) end) || {F, L} <- zip(FS, LS)],
    Result = receive Value when is_number(Value) -> Value end,
    [
        receive 
            A -> A 
        end
    || _ <- lists:seq(1, length(Pids) - 1)
    ],
    Result.
    
    



