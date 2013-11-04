%% Copyright
-module(test).
-author("eddkam").

-define(SLEEP, 10000).

%% API
-export([start_node1/1, start_node2/1]).


% Start N nodes connected in a circle &
% send a probe around the circle.
start_node1(N) ->

  StartNode = node1:start(1),

  lists:foreach(
    fun(Id) ->
      node1:start(Id, StartNode)
    end, lists:seq(1, N)
  ),

  timer:sleep(?SLEEP),
  StartNode ! probe.


% Start N nodes connected in a cricle.
% Each node with a data sets
start_node2(N) ->

  StartNode = node2:start(1),
  PidList = [StartNode] ++ [node2:start(Id, StartNode) || Id <- lists:seq(1, N-1)],

  timer:sleep(?SLEEP),

  client:start(1, PidList).