%% Copyright
-module(logger).
-author("eddkam").

%% API
-export([start/1, stop/1]).


% Start logger
start(Nodes) -> spawn_link(fun() -> init(Nodes) end).


% Stop Logger
stop(Logger) ->

  io:format("~n[Logger] Closing down ~n~n", []),
  Logger ! stop.


% Initialize
init(Nodes) ->

  NodeList = [{Node, 0} || Node <- Nodes],
  io:format("~n[Logger] Logging nodes: ~w~n~n", [NodeList]),
  loop(NodeList, [], 0).


% Loop, received messages
loop(NodeList, Queue, Max) ->

  receive

    {log, Node, Time, Message} ->

      UpdatedQueue = update_queue(Queue, {Node, Time, Message}),
      UpdatedNodeList = lists:keyreplace(Node, 1, NodeList, {Node, Time}),

      % Find the time of the worker with lowest clock
      LowestClock = lists:foldl(
        fun({_Node, WorkerTime}, LowestSoFar) ->
          min(WorkerTime, LowestSoFar)
        end, inf, UpdatedNodeList
      ),

      MaxQueueLength = max(length(UpdatedQueue), Max),

      {SafeToLog, SortedQueue} = lists:splitwith(
        fun({_Node, Clock, _Message}) ->
          Clock < LowestClock
        end, UpdatedQueue
      ),

      lists:foreach(fun(LogEntry) -> log(LogEntry) end, SafeToLog),
      loop(UpdatedNodeList, SortedQueue, MaxQueueLength);

    stop -> io:format("QueueLength: ~w~n ", [Max])

  end.


% Update queue
update_queue(Queue, LogEntry) -> lists:keysort(2, Queue ++ [LogEntry]).


% Log Message
log({From, Time, Message}) -> io:format("Log: ~w ~w ~p~n", [From, Time, Message]).