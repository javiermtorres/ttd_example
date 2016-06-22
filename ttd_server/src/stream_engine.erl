-module(stream_engine).

-export().

% TODO An ID would be needed for some boxes.
% The combiner is an example.

input_ticker(AllowedKeys, Tick, Next) when is_list(AllowedKeys) ->
    spawn_link(fun() -> ticker(Tick) end),
    input_ticker_loop(AllowedKeys, [], Next).

input_ticker_loop(AllowedKeys, StoredValues, Next) ->
    receive
        Record = {Key, Value} ->
            NewStoredValues = case lists:member(Key, AllowedKeys) of
                true ->
                    lists:keystore(Key, 1, StoredValues, Record);
                false ->
                    StoredValues
            end,
            input_ticker_loop(AllowedKeys, StoredValues, Next);
        tick ->
            Next ! {tick, StoredValues},
            input_ticker_loop(AllowedKeys, [], Next)
    end.

ticker(Tick, Pid) ->
    receive
        after Tick ->
            Pid ! tick
    end.

% Since all the inputs to this are "ticked", we wouldn't need
% to tick this. But a ticked version would be similar to the
% input_ticker_loop. I need to check if an input ticker can be combined
% with this to make a really generic box, but this seems the declarative
% stuff you'd only do in Haskell.
% TODO we need to know what signals we're consuming 
% to properly fire. 

combiner(IdList, Next) when is_list(IdList) ->
    combiner_loop(IdList, [], Next).

combiner_loop(IdList, CurrentIds, StoredVals, Next) ->
    receive
        Record = {id, Id, Value} ->
            IdsAfterDelete = lists:delete(Id, CurrentIds),
            {NewCurrentIds, NewStoredVals} = case IdsAfterDelete of
                [] ->
                    Next ! [Record|StoredVals],
                    {[], []};
                _  ->
                    {IdsAfterDelete, [Record|StoredVals]}
            end,
            combiner_loop(IdList, NewCurrentIds, NewStoredVals, Next)
    end.

% This would be a "stateful/foldp" engine. Every time a signal
% comes in, the new state is computed. We need to use a combined
% input since the new turn may depend on value interactions.
% We might think about a "first come first served" policy for
% moves, but it would not be fair.

% Check move
% Check attacks
% Segment model? Segment plus navmesh?

