%% Copyright
-module(node1).
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
  node(Id, Predecessor, Successor).


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
node(Id, Predecessor, Successor) ->

  receive

    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);

    {notify, New} ->
      UpdatedPredecessor = notify(New, Id, Predecessor),
      node(Id, UpdatedPredecessor, Successor);

    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);

    {status, Pred} ->
      UpdatedSuccessor = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, UpdatedSuccessor);

    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor);

    % Probe functionality
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor);

    {probe, Id, Nodes, T} ->
      remove_probe(Id, T, Nodes),
      node(Id, Predecessor, Successor);

    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor);
    % /Probe

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
notify({Nkey, Npid}, Id, Predecessor) ->

  case Predecessor of

    nil ->
      % Return new predecessor
      {Nkey, Npid};

    {Pkey, _Ppid} ->

      case key:between(Nkey, Pkey, Id) of
        true ->
          {Nkey, Npid};
        false ->
          Predecessor
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


create_probe(Id, {_Skey, Spid}) ->

  Time = erlang:now(),
  Spid ! {probe, Id, [Id], Time},
  io:format("~n[Node-~w] Probe started~n", [Id]).

remove_probe(Id, Time, Nodes) ->

  TimeDiff = timer:now_diff(erlang:now(), Time),
  io:format("[Node-~w] Removing probe after ~w. Nodes visited: ~w~n~n", [Id, TimeDiff, Nodes]).

forward_probe(Ref, Time, Nodes, Id, {Skey, Spid}) ->

  Spid ! {probe, Ref, Nodes ++ [Id], Time},
  io:format("[Node-~w] Forwarding probe to Node~w~n", [Id, Skey]).