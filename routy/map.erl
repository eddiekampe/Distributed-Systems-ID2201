%% Copyright
-module(map).
-author("eddkam").

%% API
-export([new/0, update/3, reachable/2, all_nodes/1]).


% Create a new empty Map
new() -> [].


% Update Map with {Node, Links}
update(Node, Links, Map) -> lists:keystore(Node, 1, Map, {Node, Links}).


% Get all reachable Cities from the Node
reachable(Node, Map) -> lists:flatten([Links || {City, Links} <- Map, Node == City]).


% All Nodes in Map
all_nodes(Map) ->

  lists:foldl(
    fun({City, Links}, AccIn) ->
      AccIn ++ [Node || Node <- [City | Links], not lists:member(Node, AccIn)]
    end, [], Map
  ).