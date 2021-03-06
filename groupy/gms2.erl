%% Copyright
-module(gms2).
-author("eddkam").

% Macros
-define(TIMEOUT, 20).
-define(ARGH, 100).
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
  leader(Id, Master, [], [Master]).


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
    {view, [Leader|Slaves], Group} ->

      io:format("[GMS-~w] Got view ~n", [Id]),
      erlang:monitor(process, Leader),

      Master ! {view, Group},
      slave(Id, Master, Leader, Slaves, Group);

    Error ->
      io:format("[GMS-~w] Got weird message: ~w~n", [Id, Error])

  after ?TIMEOUT ->

    Master ! {error, "No reply from Leader"}

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

    {'DOWN', _Ref, process,  Leader, _Reason} ->
      election(Id, Master, Slaves, Group);

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
election(Id, Master, Slaves, [_PreviousLeader|Group]) ->

  io:format("[GMS-~w] Electing the new leader~n", [Id]),
  Self = self(),

  case Slaves of

    [Self|Rest] ->
      io:format("[GMS-~w] I am now leader~n", [Id]),
      broadcast(Id, {view, Slaves, Group}, Rest),
      Master ! {view, Group},
      leader(Id, Master, Rest, Group);

    [Leader|Rest] ->
      io:format("[GMS-~w] New leader: ~w~n", [Id, Leader]),
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, Rest, Group)

  end.
