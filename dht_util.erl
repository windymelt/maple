-define(WorldSize, 1461501637330902918203684832716283019655932542975).

mod(0, _)                                        -> 0;
mod(X, Y) when X < 0                             -> (X + Y) rem Y;
mod(X, Y) when X > 0                             -> X rem Y.

cwDistanceFromTo(F, T)                           ->
    mod(T - F, ?WorldSize).

find_IdCorrespondsToMe(Id, SelfId, Pred)         ->
    cwDistanceFromTo(Pred, Id) =< cwDistanceFromTo(Pred, SelfId).

find_IdCorrespondsToSucc(Id, SelfId, Successors) ->
    [ {HeadSuccId, _} | _ ] = Successors,
    cwDistanceFromTo(SelfId, Id) =< cwDistanceFromTo(SelfId, HeadSuccId).
    
nearestNode(Id, Lis) when Lis /= []              ->
    cwDistanceFromId = fun({X, I}) -> {cwDistanceFromTo(Id, X), I} end,
    [ Nearest | _ ] = lists:sort(lists:map(cwDistanceFromId, Lis)),
    Nearest.
		    
pred_IdIsMoreReasonable(Id, Self, {PredId, _}) ->
    cwDistanceFromTo(Id, Self) < cwDistanceFromTo(PredId, Self).
