-module(misc).
-export([call/2]).

call(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        Return ->
            Return
    end.
