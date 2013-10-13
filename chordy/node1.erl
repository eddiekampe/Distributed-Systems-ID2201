%% Copyright
-module(node1).
-author("eddkam").

-define(STABILIZE, 1000).

%% API
-export([node/3, schedule_stabilize/0]).


% Node loop
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

    Error ->
      io:format("[Node-~w] Received strange message: ~w~n", [Id, Error])

  end.


% Stabilize the ring
stabilize(Predecessor, Id, Successor) ->

  {Skey, Spid} = Successor,

  case Predecessor of

    nil ->
      % Inform Successor about our existence
      Spid ! {notify, {Id, self()}},
      Successor;

    {Id, _} ->
      % It is pointing to us, don't do anything
      Successor;

    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      Successor;

    {Xkey, Xpid} ->

      case key:between(Xkey, Id, Skey) of

        true ->
          self() ! stabilize, %% Check whats going on!
          {Xkey, Xpid};
        false ->
          Spid ! {notify, {Id, self()}},
          Successor

      end

  end.


% Send request to successor
stabilize({_, Spid}) ->
  Spid ! {request, self()}.


% Schedule message passing
schedule_stabilize() ->
  timer:send_interval(?STABILIZE, self(), stabilize).


notify({Nkey, Npid}, Id, Predecessor) ->

  case Predecessor of

    nil ->
      % Return new Predecessor
      {Nkey, Npid};

    {Pkey, _} ->

      case key:between(Nkey, Pkey, Id) of

        true -> {Nkey, Npid};
        false -> Predecessor

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


