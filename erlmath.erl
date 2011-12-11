-module(erlmath).
-export([dot_product/2, spawn_multiplicators/3, reductor/3, multiplicator/1]).

%%============================================================================
%% Function: dot_product/2
%% Purpose: Get dot product of vectors A and B
%% Args: {data, A, B} - A and B are lists of components of vectors
%%       {processes, NMultiplicators} - number of working processes
%% Usage:
%%		1> dot_product({data, [1,2,3], [4,5,6]}, {processes, 3}).
%%      32
%%============================================================================
dot_product({data, A, B}, {processes, NMultiplicators})->
    ReductorPid = spawn (?MODULE, reductor, [self(), NMultiplicators, 0]),
    {workers, MultiplicatorsList} = spawn_multiplicators(ReductorPid, NMultiplicators, []),
	Zipped = lists:zip(A, B),
    map(MultiplicatorsList, Zipped),
    receive
	{result, Result} ->
	    Result
    end.

%%============================================================================
%% Function: spawn_multiplicators/3
%% Purpose: Spawn new worker threads
%%============================================================================
spawn_multiplicators(ReductorPid, Count, List) when Count > 0 ->
    spawn_multiplicators(ReductorPid, Count-1, lists:append([spawn(?MODULE, multiplicator, [ReductorPid])], List));	
spawn_multiplicators(_, Count, ResultList) when Count == 0 ->
    {workers, ResultList}.

%%============================================================================
%% Function: map/4
%% Purpose: Maps data to workers
%%============================================================================
map(Pids, PidNumber, Pairs, PairsPerPid) ->
	if
		PidNumber == 1 ->
			PairsList = lists:sublist(Pairs, 1, PairsPerPid);
		PidNumber < length(Pids) ->
			PairsList = lists:sublist(Pairs, PairsPerPid * (PidNumber - 1) + 1, PairsPerPid);
		true ->
			PairsList = lists:sublist(Pairs, PairsPerPid * (PidNumber - 1) + 1, PairsPerPid + (length(Pairs) rem length(Pids)))
	end,
	lists:nth(PidNumber, Pids) ! {task, PairsList},
    if 
		(PidNumber < length(Pids)) ->
			map(Pids, PidNumber + 1, Pairs, PairsPerPid);
		true ->
			ok
    end.

%%============================================================================
%% Function: map/2
%% Purpose: Same as map/4
%%============================================================================
map(Pids, Pairs)->
	PairsPerPid = round(length(Pairs)/length(Pids)),
    map(Pids, 1, Pairs, PairsPerPid).
	
%%============================================================================
%% Function: reductor/3
%% Purpose: Reduce workers' result to dot product result
%%============================================================================
reductor (CallbackPid, WorkersCount, Result) ->
	if 
		WorkersCount > 0 ->
			receive
			{product, Product} ->
				reductor(CallbackPid, WorkersCount-1, Result + lists:sum(Product))
			end;
		true ->
			CallbackPid ! {result, Result}
	end.
	
%%============================================================================
%% Function: multiplicator/1
%% Purpose: Multiplies each pair, provided in PairsList 
%%   and sends it to reductor process
%%============================================================================
multiplicator(ReductorPid) ->
    receive
	{task, PairsList} ->
	    Product = lists:map(fun({A,B})->A*B end, PairsList),
	    ReductorPid ! {product, Product}
    end.

