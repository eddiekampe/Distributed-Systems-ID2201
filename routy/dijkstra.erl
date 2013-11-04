%% Copyright
-module(dijkstra).
-author("eddkam").

%% API
-export([table/2, route/2, iterate/3]).


% Find a Node's Length attribute
entry(Node, SortedList) ->

  case lists:keyfind(Node, 1, SortedList) of
    false -> 0;
    {_City, Length, _Gateway} -> Length
  end.


% Replace Node with updated attributes
replace(Node, Length, Gateway, SortedList) ->

  UnsortedList = lists:keyreplace(Node, 1, SortedList, {Node, Length, Gateway}),
  lists:keysort(2, UnsortedList).


% Determine if a Node should be updated in the List
update(Node, Length, Gateway, SortedList) ->

  EntryLength = entry(Node, SortedList),
  if
    Length < EntryLength -> replace(Node, Length, Gateway, SortedList);
    true -> SortedList
  end.


% Heart of dijkstra
iterate([], _Map, Table) -> Table;
iterate([{_City, inf, _Gateway} | _Tail], _Map, Table) -> Table;
% Iterate over Entries in the SortedList
iterate([FirstEntry | SortedList], Map, Table) ->

  % Expand the first entry in the SortedList
  {City, Length, Gateway} = FirstEntry,

  case map:reachable(City, Map) of

    [] ->
      % No reachable nodes from current entry.
      % Prepend the table and continue iteration
      iterate(SortedList, Map, [{City, Gateway}] ++ Table);

    ReachableNodes ->
      % Found reachable nodes, update and put the result into UpdatedList
      UpdatedList = lists:foldl(
        fun(Node, AccIn) ->
          update(Node, Length + 1, Gateway, AccIn)
        end, SortedList, ReachableNodes
      ),
      % Prepend the table and continue iteration
      iterate(UpdatedList, Map, [{City, Gateway}] ++ Table)

  end.


% Construct a routing table given the Gateways and a Map.
table(Gateways, Map) ->

  GatewayList = [{Gateway, 0, Gateway} || Gateway <- Gateways],
  MapNodes = [{Node, inf, unknown} || Node <- map:all_nodes(Map), not lists:member(Node, Gateways)],

  iterate(GatewayList ++ MapNodes, Map, []).


% Search the routing table and return the gateway suitable to route messages to a node.
% If a gateway is found we should return {ok, Gateway} otherwise we return notfound.
route(Node, Table) ->

  case lists:keyfind(Node, 1, Table) of
    false -> notfound;
    {_Node, Gateway} -> {ok, Gateway}
  end.