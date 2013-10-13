%% Copyright
-module(test).
-author("eddkam").

-define(SLEEP, 1000).

%% API
-export([start_node1/1]).


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
