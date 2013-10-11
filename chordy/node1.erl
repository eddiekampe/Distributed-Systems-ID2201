%% Copyright
-module(node1).
-author("eddkam").

%% API
-export([node/3]).





%% The messages we need to maintain the ring are:
%% • {key, Qref, Peer} : a peer needs to know our key
%% • {notify, New} : a new node informs us of its existence
%% • {request, Peer} : a predecessor needs to know our predecessor
%% • {status, Pred} : our successor informs us about its predecessor

% Node loop
node(Id, Predecessor, Successor) ->

  receive

    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);

    {notify, New} ->
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);

    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);

    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ);

    Error ->
      io:format("[Node-~w] Received strange message: ~w~n", [Id, Error])

  end.


stabilize(Predecessor, Id, Successor) ->

  {Skey, Spid} = Successor,

  case Predecessor of

    nil ->
      % Inform Predecessor about our existence
      ok;

    {Id, _} ->
      % It is pointing to us, don't do anything
      ok;

    {Xkey, Xpid} ->

      case key:between(Xkey, Id, Skey) of
        true ->
          ok;
        false ->
          ok
      end

  end.


%% The periodic stabilize procedure will consist of a node sending a {request,
%% self()} message to its successor and then expecting a {status, Pred} in
%% return. When it knows the predecessor of its successor it can check if the ring
%% is stable or if the successor needs to be notifies about its existence through
%% a {notify, {Id, self()} message.
%% Below is a skeleton for the stabilize/3 procedure. The Pred argument
%% is ours successors current predecessor. If this i nil we should of course
%% inform it about our existence. If it is pointing back to us we don’t have to
%% do anything. If it is pointing to itself we should of course notify it about
%% our existence.
%% If it’s pointing to another node we need to be careful. The question is
%% if we are to slide in between the two nodes or if we should place ourselves
%% behind the predecessor. If the key of the predecessor of our successor (Xkey)
%% is between us and our successor we should of course adopt this node as our
%% successor and run stabilization again. If we should be in between the nodes
%% we inform our successor of our existence.