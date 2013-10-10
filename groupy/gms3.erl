%% Copyright
-module(gms3).
-author("eddkam").

% Macros
-define(TIMEOUT, 1000).
-define(ARGH, 10000).
-define(RANDOM, 1000).

%% API
-export([start/1, start/2]).


% Start Leader
start(Id) ->

  Random = random:uniform(?RANDOM),
  Self = self(),
  {ok, spawn_link(fun() -> init_leader(Id, Random, Self) end)}.


% Initialize Leader
init_leader(Id, Random, Master) ->

  random:seed(Random, Random, Random),
  leader(Id, Master, 0, [], [Master]).


% Start Slave
start(Id, Grp) ->

  Self = self(),
  {ok, spawn_link(fun() -> init_slave(Id, Grp, Self) end)}.


% Initialize Slave
init_slave(Id, Grp, Master) ->

  io:format("[GMS-~w] Asking group to join~n", [Id]),
  Self = self(),
  Grp ! {join, Master, Self},

  receive
    {view, N, [Leader|Slaves], Group} ->

      io:format("[GMS-~w] Got view ~n", [Id]),
      erlang:monitor(process, Leader),

      Master ! {view, Group},
      slave(Id, Master, Leader, N, {view, N, [Leader|Slaves], Group}, Slaves, Group);

    Error ->
      io:format("[GMS-~w] Got weird message: ~w~n", [Id, Error])

  after ?TIMEOUT ->

    Master ! {error, "No reply from Leader"}

  end.


% Slave loop
slave(Id, Master, Leader, N, Last, Slaves, Group) ->

  receive

    {mcast, Message} ->

      Leader ! {mcast, Message},
      slave(Id, Master, Leader, N, Last, Slaves, Group);

    {join, Worker, Peer} ->

      io:format("[GMS-~w] Slave got join: ~w ~w~n", [Id, Worker, Peer]),
      Leader ! {join, Worker, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);


    {msg, I, _Message} when I < N ->
      slave(Id, Master, Leader, N, Last, Slaves, Group);

    {msg, I, Message} ->

      Master ! Message,
      slave(Id, Master, Leader, I+1, {msg, I, Message}, Slaves, Group);

    {view, I, [Leader|UpdatedSlaves], UpdatedGroup} ->

      Master ! {view, UpdatedGroup},
      slave(Id, Master, Leader, I, {view, I, [Leader|UpdatedSlaves], UpdatedGroup}, UpdatedSlaves, UpdatedGroup);

    {'DOWN', _Ref, process,  Leader, _Reason} ->
      election(Id, Master, N, Last, Slaves, Group);

    stop -> ok;
    Error -> io:format("[GMS-~w] Slave, received strange message: ~w~n", [Id, Error])

  end.


% Leader loop
leader(Id, Master, N, Slaves, Group) ->

  receive

    {mcast, Message} ->

      broadcast(Id, {msg, N, Message}, Slaves),
      Master ! Message,
      leader(Id, Master, N+1, Slaves, Group);

    {join, Worker, Peer} ->

      io:format("[GMS-~w] Leader got join: ~w ~w~n", [Id, Worker, Peer]),

      UpdatedSlaves = Slaves ++ [Peer],
      UpdatedGroup = Group ++ [Worker],

      broadcast(Id, {view, N, [self()|UpdatedSlaves], UpdatedGroup}, UpdatedSlaves),

      Master ! {view, UpdatedGroup},

      leader(Id, Master, N+1, UpdatedSlaves, UpdatedGroup);

    stop -> ok;
    Error -> io:format("[GMS-~w] Leader, received strange message: ~w~n", [Id, Error])

  end.


% Broadcast a message to given nodes
broadcast(Id, Message, Nodes) ->
  lists:foreach(fun(Node) -> Node ! Message, crash(Id) end, Nodes).


% Add a small chance to crash
crash(Id) ->

  case random:uniform(?ARGH) of

    ?ARGH->
      io:format("[GMS-~w] Leader crashed ~n", [Id]),
      exit(no_luck);
    _ ->
      ok

  end.


% Elect a new leader
election(Id, Master, N, Last, Slaves, [_PreviousLeader|Group]) ->

  io:format("[GMS-~w] Electing the new leader~n", [Id]),
  Self = self(),

  case Slaves of

    [Self|Rest] ->

      io:format("[GMS-~w] I am now leader~n", [Id]),
      io:format("[GMS-~w] Sending Last message: ~w~n", [Id, Last]),
      timer:sleep(300),
      broadcast(Id, Last, Rest),
      broadcast(Id, {view, N, Slaves, Group}, Rest),

      Master ! {view, Group},
      leader(Id, Master, N+1, Rest, Group);

    [Leader|Rest] ->

      io:format("[GMS-~w] New leader: ~w~n", [Id, Leader]),
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N, Last, Rest, Group)

  end.
