-module(third).
-export([search/2, search_prim/2, count/2, count_prim/2, freq2/1]).

search(E, [H | _]) when E == H ->
    true;
search(E, [ _ | T]) ->
    search(E, T);
search(_, []) ->
    false.


search_prim(E, [H | T]) ->
    (E == H) or search_prim(E, T);
search_prim(_, [])->
    false.

count(E, L) ->
    count(E, L, 0).

%count(E, [H | T], Count) when E == H -> 
%Replaced by pattern matching, so we don't need to use the guard condition
count(E, [E | T], Count) ->
    count(E, T, Count + 1);
count(E, [_ | T], Count) ->
    count(E, T, Count);
count(_, [], Count) ->
    Count.


count_prim(E, [H | T]) when E == H ->
    1 + count_prim(E, T);
count_prim(E, [_ | T]) ->
    count_prim(E, T);
count_prim(_, []) ->
    0.


count_del(E, [E|T]) ->
    {Count, Rem} = count_del(E, T),
    {Count+1, Rem};
count_del(E, [H|T]) ->
    {Count, Rem} = count_del(E, T),
    {Count, [H|Rem]};
count_del(_, []) ->
    {0,[]}.


freq2([H|T]) ->
    {Count, Rem} = count_del(H, T),
    [{Count + 1, H} | freq2(Rem)];
freq2([]) ->
    [].
