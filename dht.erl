-module(dht).
-export([node_loop/1]).

-include("dht_util.erl").

-define(StabilizationTime, 10).

-compile( export_all ).

start() ->
    io:fwrite("Maple Engine\n"),
    random_seed_start(),
    create().

node_loop(StatePid) ->
    io:fwrite("Waiting for message...\n"),
    RandTime_Stabilization = random:uniform(1000 * ?StabilizationTime),
    receive
	{Pid, info} ->
	    Pid ! info,
	    node_loop(StatePid);

	{Pid, returnSuccessor} ->
	    StatePid ! {self(), {work, fun(S) -> {SelfId, [SH|_], Fingers, Pred, Database} = S, Pid ! SH end}},
	    node_loop(StatePid);

	{Pid, returnPredecessor} ->
	    StatePid ! {self(), {work, fun(S) -> {_, _, _, Pred, _} = S, Pid ! Pred end}},
	    node_loop(StatePid);

	{Pid, {findSuccessor, Id}} ->
	    StatePid ! {work,
			fun(S) -> 
				{SelfId, Successors, Fingers, Pred, Database} = S,
				spawn(fun() ->
					      nodeFinder(Pid, Id, {SelfId, self()}, Pred, Successors ++ Fingers)
				      end)
			end},
	    node_loop(StatePid);

	{_, {notify, Node}} ->
	    StatePid ! {self(), {send, fun(S) -> {SelfId, _, _, Pred, _} = S,
						 case pred_IdIsMoreReasonable(Node, SelfId, Pred) of
						     true -> setelement(4, S, Node);
						     false -> S
						 end
				       end}},
	    node_loop(StatePid);

	%% {Pid, {getData, DataKey}} when haveCorrespondingData(DataKey) ->
	%%     %% write more suitably.
	%%     %% write 
	%%     Pid ! {found, lists:keyfind(DataKey, 2, Database)},
	%%     node_loop(StatePid);

	%% {Pid, {getData, DataKey}} ->
	%%     Pid ! {notFound, DataKey},
	%%     node_loop(StatePid);

	%% {Pid, {putData, Key, Data}} when keyCorresponds(Key, Data, SelfId) ->
	%%     Pid ! {accepted, Key},
	%%     StatePid ! {send, fun(S) -> {_, _, _, _, DB} = S, setelement(5, S, [{Key, Data} | DB]) end},
	%%     node_loop(StatePid);
	
	{_, {join, Address}} ->
	    StatePid ! {work,
			fun(S) ->
				{SelfId, _, _, _, _} = S,
				spawn(fun() -> joiner(Address, StatePid, SelfId) end)
			end},
	    node_loop(StatePid)
	    
		after RandTime_Stabilization ->
			io:format("Node ~p Running Stabilization after ~p msec ~n", [self(), RandTime_Stabilization]),
			StatePid ! {self(), get},
			receive
			    {_, [{_, SuccPid} | _]} ->
				stabilize(SuccPid, StatePid)
			end,
			node_loop(StatePid)
    end.

stateMemory(S) ->
    {SelfId, Successors, Fingers, Pred, Database} = S,
    receive
	{_, {send, F}} ->
	    stateMemory(F(S));
	{_, {work, F}} ->
	    F(S),
	    stateMemory(S);
	{_, {alter, S2}} ->
	    stateMemory(S2);
	{Pid, get} ->
	    Pid ! S,
	    stateMemory(S)
    end.

joiner(NodePid, StatePid, JoiningId) ->
    NodePid ! {findSuccessor, JoiningId},
    receive
	{found, NewSucc} ->
	    StatePid ! {send, fun(S) -> setelement(2, S, [NewSucc]) end},
	    ok;
	Others ->
	    undefined_message
    end.

nodeFinder(QueryPublisherAddress, QueryId, {SelfId, SelfAddress}, {PredId, PredAddress}, NodeList) ->
    Nearest = nearestNode(QueryId, NodeList),
    case find_IdCorrespondsToMe(QueryId, SelfId, PredId) of
	true ->
	    QueryPublisherAddress ! {found, {SelfId, SelfAddress}},
	    ok;
	false ->
	case find_IdCorrespondsToSucc(QueryId, SelfId, Nearest) of
	    true -> QueryPublisherAddress ! {found, Nearest}, ok;
	    false -> 
		Nearest = nearestNode(QueryId, NodeList),
		Nearest ! {QueryPublisherAddress, {findnode, QueryId}},
		ok
	end
    end.
	    
userInterface(DhtPid) ->
    receive
	{Pid, init} ->
	    %% some initialize task.
	    Pid ! ok,
	    userInterface(DhtPid);	   
	{Pid, {getData, Key}} ->
	    %% do gather
	    Pid ! ok,
	    userInterface(DhtPid)
	end.

chunkGatherer({Meta, Chunks}) ->
    receive
	{Pid, Chunk} -> ok
    end.
	    
random_seed_start() ->
    case random:seed(now()) of
	undefined ->
	    random_seed_start();
	_Else -> ok
    end.

create() ->
    %% Creating new node.
    NodeId = random:uniform(?WorldSize) - 1, % 0 until 2^160 - 1
    StatePid = spawn(fun() -> stateMemory({NodeId, [], [], [], []}) end),
    NodePid = spawn(fun() -> node_loop(StatePid) end),
    StatePid ! {send, fun(S) -> setelement(2, S, [{NodeId, NodePid}]) end},
    NodePid.

hash(Data) ->
    crypto:sha(Data).

haveCorrespondingData(StatePid, Key) ->
    StatePid ! {self(), get},
    receive
	{_, _, _, _, Database} ->
	    lists:any(fun({DBKey, _}) ->
			      DBKey == Key end, Database)
    end.

stabilize(SuccPid, StatePid) ->
    {SelfId, _, _, _, _} = StatePid ! {self(), get},
    case is_pid(SuccPid) andalso is_process_alive(SuccPid) of
	true ->
	    Successors_Predecessor = SuccPid ! {self(), returnPredecessor},
	    case SuccPid =:= Successors_Predecessor of
		true ->
		    no_need_to_update_my_successor;
		false ->
		    self() ! {self(), {updateSuccessor, Successors_Predecessor}},
		    Successors_Predecessor ! {self(), {notify, {SelfId, self()}}}
	    end;
	false ->
	    pid_is_null
    end.
		    
