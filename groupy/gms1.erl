%% Copyright
-module(gms1).
-author("eddkam").

%% API
-export([start/1, start/2]).


% Start??
start(Id) ->

  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Self) end)}.


% Initialize Leader
init(Id, Master) -> leader(Id, Master, [], [Master]).


% Start Group
start(Id, Group) ->

  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Group, Self) end)}.


% Initialize Group
init(Id, Group, Master) ->

  Self = self(),
  io:format("[GMS-~w] Asking group to join~n", [Id]),
  Group ! {join, Master, Self},
  
  receive
    {view, [Leader|Slaves], NewGroup} ->

      io:format("[GMS-~w] Got view from group ~n", [Id]),

      Master ! {view, NewGroup},
      slave(Id, Master, Leader, Slaves, NewGroup);

    Error -> io:format("[GMS-~w] Got wierd message: ~w~n", [Id, Error])
  end.


% Slave loop
slave(Id, Master, Leader, Slaves, Group) ->
  
  receive
    
    {mcast, Message} ->

      Leader ! {mcast, Message},
      slave(Id, Master, Leader, Slaves, Group);

    {join, Worker, Peer} ->

      io:format("[GMS-~w] Slave got join: ~w ~w~n", [Id, Worker, Peer]),
      Leader ! {join, Worker, Peer},
      slave(Id, Master, Leader, Slaves, Group);

    {msg, Message} ->

      Master ! Message,
      slave(Id, Master, Leader, Slaves, Group);

    {view, [Leader|UpdatedSlaves], UpdatedGroup} ->

      Master ! {view, UpdatedGroup},
      slave(Id, Master, Leader, UpdatedSlaves, UpdatedGroup);

    stop -> ok;
    Error -> io:format("[GMS-~w] Slave, received strange message: ~w~n", [Id, Error])
    
  end.


% Leader loop
leader(Id, Master, Slaves, Group) ->

  receive

    {mcast, Message} ->

      broadcast(Id, {msg, Message}, Slaves),
      Master ! Message,
      leader(Id, Master, Slaves, Group);

    {join, Worker, Peer} ->

      io:format("[GMS-~w] Leader got join: ~w ~w~n", [Id, Worker, Peer]),

      UpdatedSlaves = Slaves ++ [Peer],
      UpdatedGroup = Group ++ [Worker],

      broadcast(Id, {view, [self()|UpdatedSlaves], UpdatedGroup}, UpdatedSlaves),

      Master ! {view, UpdatedGroup},

      leader(Id, Master, UpdatedSlaves, UpdatedGroup);

    stop -> ok;
    Error -> io:format("[GMS-~w] Leader, received strange message: ~w~n", [Id, Error])

  end.


% Broadcast a message to given nodes
broadcast(_Id, Message, Nodes) ->

  %io:format("[GMS-~w] Broadcasting: ~w~n", [_Id, Message]),
  lists:foreach(fun(Node) -> Node ! Message end, Nodes).