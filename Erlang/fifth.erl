-module(fifth).
-export([apply_twice/2, eval/3]).

apply_twice(F, Arg) ->
    X = F(Arg),
    F(X).



eval(M, F, Arg) ->
    try
        M:F(Arg)
    of
        Value -> {return_value, Value}
    catch
        error:function_clause -> "The function has no matching definition";
        _:undef ->  "The function is not defined";
        _:_:_Stack -> "Unexpected Error occured"
    end.