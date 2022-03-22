-module(se).
-export([figure/1,min/2, upperLower/1, minimum/1, zip/2, tails/1, bin_to_decimal/1, game/1]).


figure({A, B, C, D}) ->
    if (A == B) and (A == C) and (A == D) ->
        "square";
    true -> ok
    end;
figure({A, B, C}) ->
    if ((A == B) and (B == C) and (A == C)) ->
        {"equilateral, not_rectangle"};
    true ->
        if (not (A == B) and not (B == C) and not (A == C)) ->
            {"not_a_proper_triangle"};
        true -> {"isosceles, not_rectangle"}
        end
    end.



min(X, Y) ->
    case X =< Y of 
        true -> X;
        false -> Y
    end.



upperLower(X) ->
    Y = string:to_lower(X),
    if (X == Y) ->
        string:to_upper(X);
    true -> string:to_lower(X)
    end.


minimum([H|T]) ->
    minimum(H,T);
minimum([]) -> erlang:error("Minumum: Empty List").

minimum(E, [H|T]) ->
    if E < H ->
        Min = E,
        minimum(Min, T);
    true -> 
        Min = H,
        minimum(Min, T)
    end;
minimum(E, []) ->
    E.


zip([H|T], [X|Y]) ->
    [{H,[X]} | zip(T,Y)];
zip(_, _) ->
    [].



tails([H|T]) ->
    [[H] ++ T] ++ tails(T);
tails([])->
    [[]].



bin_to_decimal(L) ->
    bin_to_decimal(L, lists:flatlength(L) - 1, 0).

bin_to_decimal([H|T], X, Result) ->
    Count = Result + (H * trunc(math:pow(2, X))),
    bin_to_decimal(T, X - 1, Count);

bin_to_decimal([], _, Result) ->
    Result.



% function takes an argument and returns anonymous function
game(N) ->
    fun(X) ->
        Y = rand:uniform(N),
        case X == round(Y) of
            true -> "congragulations";
            _    -> "try again"
        end
    end.



