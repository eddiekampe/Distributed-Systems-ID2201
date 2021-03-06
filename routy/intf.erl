%% Copyright
-module(intf).
-author("eddkam").

%% API
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).


% Return an empty set of interfaces.
new() -> [].


% Add a new entry to the set and return the new set of interfaces.
add(Name, Ref, Pid, Intf) -> lists:keystore(Name, 1, Intf, {Name, Ref, Pid}).


% Remove an entry given a name of an interface, return a new set of interfaces.
remove(Name, Intf) -> lists:keydelete(Name, 1, Intf).


% Find the process identifier given a name, return {ok, Pid} if found otherwise notfound.
lookup(Name, Intf) ->

  case lists:keyfind(Name, 1, Intf) of
    false -> notfound;
    {_Name, _Ref, Pid} -> {ok, Pid}
  end.


% Find the reference given a name and return {ok, Ref} or notfound.
ref(Name, Intf) ->

  case lists:keyfind(Name, 1, Intf) of
    false -> notfound;
    {_Name, Ref, _Pid} -> {ok, Ref}
  end.


% Find the name of an entry given a reference and return {ok, Name} or notfound.
name(Ref, Intf) ->

  case lists:keyfind(Ref, 2, Intf) of
    false -> notfound;
    {Name, _Ref, _Pid} -> {ok, Name}
  end.


% Return a list with all names.
list(Intf) -> [Name || {Name, _Ref, _Pid} <- Intf].


% Send the message to all interface processes.
broadcast(Message, Intf) ->
  io:format("Broadcasting~n", []),
  lists:foreach(
    fun({_Node, _Ref, Pid}) ->
      % io:format("[Intf] Sending message ~w to: ~w~n ", [Message, Pid]),
      Pid ! Message
    end, Intf
  ).