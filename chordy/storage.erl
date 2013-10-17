%% Copyright
-module(storage).
-author("eddkam").

%% API
-export([create/0, add/3, lookup/2, split/2, merge/2]).


create() ->
  [].

add(Key, Value, Store) ->
  lists:keystore(Key, 1, Store, {Key, Value}).

lookup(Key, Store) ->

  case lists:keyfind(Key, 1, Store) of
    false ->
      not_found;
    KeyPair ->
      KeyPair
  end.

split(Key, Store) ->
  SortedStore = lists:keysort(1, Store),
  lists:splitwith(
    fun(Id) ->
      Id =< Key
    end, SortedStore
  ).

merge(Store, Elements) ->
  Store ++ Elements.