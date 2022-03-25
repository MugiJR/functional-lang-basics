-module(exam1).
-export([differences/2, getPositions/2, applyAll/2, riffleShuffle/1]).

%% Submitted by Mugeshh %%

% Task 1: Difference of lists

% Compare two lists (position-by-position), and return the differences! 
% The result should include the elements of the first list that differ from the elements of the second. 
% The function should check all the elements of the first list.

% Sample Input and Output

% exam:differences("","") == [].
% exam:differences("","apple") == [].
% exam:differences("apple", "") == "apple".
% exam:differences("apple", "apple") == [].
% exam:differences("apple", "peach") == "apple".
% exam:differences("apple", "apfel") == "ple".
% exam:differences([1,2,3], [3,2,1]) == [1,3].

differences([],[]) ->
    [];
differences([], _) ->
    [];
differences(L, []) ->
    L;
differences([H|T], [H1|T1]) ->
    if H /= H1 ->
        [H | differences(T,T1)];
    true -> differences(T,T1)
    end.
    

% Task 2 & 5 : Apply all and Error handling

% Define a function that takes a list of functions and a list of some elements as arguments.  
% The function applies each function to all the elements of the list. The order of the evaluation matters. 
% First, the first given function should be evaluated on all the elements of the list, then the second, etc.

% Sample Input and Output

% exam:applyAll([fun(A) -> A+1 end, fun(A) -> A*2 end], [1,2,3,4]) == [2,3,4,5,2,4,6,8].
% exam:applyAll([fun(A) -> A+2 end], []) == [].
% exam:applyAll([], [apple, pear]) == [].
% exam:applyAll([fun erlang:is_list/1], [apple, pear]) == [false,false].
% exam:applyAll([fun erlang:is_list/1], [apple, pear, []]) == [false,false,true].


% Modify the definition of the applyAll/2  function to handle the runtime errors occurring 
% during the evaluations of the function arguments on the elements.
% Put the atom bad_fun_argument into the returning list when a runtime error occurs.

% Test cases

% exam:applyAll([fun(A) -> A+2 end], [1,apple]) == [3,bad_fun_argument].
% exam:applyAll([fun erlang:atom_to_list/1, fun(A) -> A*2 end], [1,apple,3, '12']) 
%     == [bad_fun_argument, "apple", bad_fun_argument, "12", 2,  bad_fun_argument, 6, bad_fun_argument].
% exam:applyAll([fun(A) -> A+1 end, fun(A) -> A*2 end], [1,2,3,4]) == [2,3,4,5,2,4,6,8].


applyAll(F, Arg) ->
    % take the first element of the functions list and apply it to the list. with list comprehensions

    [ 
        try 
            A1(E1) 
        catch
            error:_-> "bad_fun_argument"
        end

        || A1<-F, E1<-Arg
    ].


% Task 3: Positions

% Define a function that returns the positions of a given element in a list! The indexing starts with `1`

% Sample Input and Output

% exam:getPositions($e, "apple") == [5].
% exam:getPositions($p, "apple") == [2,3].
% exam:getPositions(1, []) == [].
% exam:getPositions(1, [1,3,2,1,2,34,21,1,1,4]) == [1,4,8,9].


getPositions(E, L) ->
    getPositions(E, L, 1).


getPositions(E, [E|T], L) ->
    [L | getPositions(E, T, L + 1)];
getPositions(E, [_|T], L) ->
    getPositions(E, T, L + 1);
getPositions(_, [], _) ->
    [].



% Riffle shuffle

% Define a riffle shuffle algorithm on lists! The function takes a list as an argument, it splits the list into
% two "equal" parts (if the  number of elements  is odd, then the first part should be shorteer), then merges the 
% two sublists into a new list alternately (one from the second repeatedly)

% Example:

% [1,2,3,4,5]]
% [1,2] [3,4,5]
% [1,3,2,4,5]

% riffleShuffle([1,2,3,4]) = [1,3,2,4]
% riffleShuffle([1,2,3,4,5]) = [1,3,2,4,5]

riffleShuffle(L) ->
    % case (length(L) rem 2 == 0) of 
    %     true ->  riffleShuffle(lists:sublist(L,1, trunc(length(L) / 2)), lists:sublist(L,trunc(length(L) / 2)+1 ,trunc(length(L)) ));
    %     false -> riffleShuffle(lists:sublist(L,1, trunc(length(L) / 2)), lists:sublist(L,trunc(length(L) / 2)+1 ,trunc(length(L)) ))
    % end.
    {L1, L2} = lists:split(trunc(length(L)/2), L),
    riffleShuffle(L1,L2).

riffleShuffle([], L) ->
    L;
riffleShuffle(L, []) ->
    L;
riffleShuffle([], []) ->
    [];
riffleShuffle([H1|T1], [H2|T2]) ->
    [H1] ++ [H2] ++ riffleShuffle(T1, T2).


