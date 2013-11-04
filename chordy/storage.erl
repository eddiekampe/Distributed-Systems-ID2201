%% Copyright
-module(storage).
-author("eddkam").

%% API
-export([create/0, add/3, lookup/2, split/2, merge/2]).


% Create an empty storage
create() ->
  [].


% Add {Key, Value} to the storage
add(Key, Value, Store) ->
  lists:keystore(Key, 1, Store, {Key, Value}).


% Lookup Key in storage
lookup(Key, Store) ->

  case lists:keyfind(Key, 1, Store) of
    false ->
      not_found;
    KeyPair ->
      KeyPair
  end.


% Split the storage into two lists based on Key
split(Key, Store) ->
  SortedStore = lists:keysort(1, Store),
  lists:splitwith(
    fun(Id) ->
      Id =< Key
    end, SortedStore
  ).


% Merge two lists
merge(Store, Elements) ->
  Store ++ Elements.