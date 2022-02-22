-module(first).
-export([foo/0, foo/1]).

foo()->
    done.

foo(X)->
    X+1.
