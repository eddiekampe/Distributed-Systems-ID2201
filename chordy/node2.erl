%% Copyright
-module(node2).
-author("eddkam").

-define(STABILIZE, 100).
-define(TIMEOUT, 10000).

%% API
-export([start/1, start/2]).


% Start first node
start(Id) -> start(Id, nil).
% Start node connecting to Peer
start(Id, Peer) ->

  timer:start(),
  spawn(fun() -> init(Id, Peer) end).


% Initialize
init(Id, Peer) ->

  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, storage:create()).


% Connect to Peer
connect(Id, nil) -> {ok, {Id, self()}};
connect(Id, Peer) ->

  Qref = make_ref(),
  Peer ! {key, Qref, self()},

  receive
    {Qref, Skey} -> {ok, {Skey, Peer}}
  after ?TIMEOUT ->
    io:format("[Node-~w] Time out - no response ~n", [Id])
  end.


% Main loop
node(Id, Predecessor, Successor, Store) ->

  receive

    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);

    {notify, New} ->
      {UpdatedPredecessor, UpdatedStore} = notify(New, Id, Predecessor, Store),
      node(Id, UpdatedPredecessor, Successor, UpdatedStore);

    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);

    {status, Pred} ->
      UpdatedSuccessor = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, UpdatedSuccessor, Store);

    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store);

    % Probe functionality
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store);

    {probe, Id, Nodes, T} ->
      remove_probe(Id, T, Nodes),
      node(Id, Predecessor, Successor, Store);

    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store);
    % /Probe

    Add={add, Key, Value, Qref, Client} ->
      io:format("[Node-~w] Add: ~w~n", [Id, Add]),
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);

    Lookup={lookup, Key, Qref, Client} ->
      io:format("[Node-~w] Lookup: ~w~n", [Id, Lookup]),
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);

    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged);

    Error -> io:format("[Node-~w] Received strange message: ~w~n", [Id, Error])

  end.


% Stabilize the ring
stabilize(Predecessor, Id, Successor) ->

  {Skey, Spid} = Successor,

  case Predecessor of

    nil ->
      % Inform Successor about our existence
      Spid ! {notify, {Id, self()}},
      Successor;

    {Id, _Spid} ->
      % It is pointing to us, don't do anything
      Successor;

    {Skey, _Spid} ->
      % Pointing to itself. Inform about our existence
      Spid ! {notify, {Id, self()}},
      Successor;

    {Xkey, Xpid} ->

      case key:between(Xkey, Id, Skey) of
        true ->
          Xpid ! {request, self()},
          {Xkey, Xpid};
        false ->
          Spid ! {notify, {Id, self()}},
          Successor
      end

  end.


% Send request to successor
stabilize({_Skey, Spid}) -> Spid ! {request, self()}.


% Schedule message passing
schedule_stabilize() -> timer:send_interval(?STABILIZE, self(), stabilize).


% Notify
notify({Nkey, Npid}, Id, Predecessor, Store) ->

  case Predecessor of

    nil ->
      % Return new predecessor
      Keep = handover(Store, Nkey, Npid),
      {{Nkey, Npid}, Keep};

    {Pkey, _Ppid} ->

      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Store, Nkey, Npid),
          {{Nkey, Npid}, Keep};
        false ->
          {Predecessor, Store}
      end

  end.


% Respond to request
request(Peer, Predecessor) ->

  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.


% Send a probe around the ring
create_probe(Id, {_Skey, Spid}) ->

  Time = erlang:now(),
  Spid ! {probe, Id, [Id], Time},
  io:format("~n[Node-~w] Probe started~n", [Id]).


% Full circle achieved, measure time
remove_probe(Id, Time, Nodes) ->

  TimeDiff = timer:now_diff(erlang:now(), Time),
  io:format("[Node-~w] Removing probe after ~w. Nodes visited: ~w~n~n", [Id, TimeDiff, Nodes]).


% Forward the probe to our successor
forward_probe(Ref, Time, Nodes, Id, {Skey, Spid}) ->

  Spid ! {probe, Ref, Nodes ++ [Id], Time},
  io:format("[Node-~w] Forwarding probe to Node~w~n", [Id, Skey]).


% Add {Key, Value} to storage if we are responsible, otherwise delegate the job
add(Key, Value, Qref, Client, Id, {Pkey, _Ppid}, {_Skey, Spid}, Store) ->

  case key:between(Key, Pkey, Id) of
    true ->
      io:format("[Node-~w] Adding {~w, ~w} to store~n", [Id, Key, Value]),
      Client ! {Qref, ok},
      storage:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.


% Lookup Key in storage if we are responsible, otherwise delegate the job
lookup(Key, Qref, Client, Id, {Pkey, _Ppid}, Successor, Store) ->

  case key:between(Key, Pkey, Id) of
    true ->
      io:format("[Node-~w] Looking up key: ~w~n", [Id, Key]),
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      {_Skey, Spid} = Successor,
      Spid ! {lookup, Key, Qref, Client}
  end.


% Handover part of the storage to new Node
handover(Store, Nkey, Npid) ->

  {Keep, Leave} = storage:split(Nkey, Store),
  Npid ! {handover, Leave},
  Keep.