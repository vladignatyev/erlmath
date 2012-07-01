-module(gorner).
-export([gorner_tail/2]).

gorner_tail([H|T], X)->
    H + X*gorner_tail(T,X);
gorner_tail([],X)->
    0.

