%% Copyright
-module(routy).
-author("eddkam").

%% API
-export([start/1, stop/1, init/1]).


% Start Routy
start(Name) ->
  register(Name, spawn(fun() -> init(Name) end)).


% Stop
stop(Node) ->

  Node ! stop,
  unregister(Node).


% Initialize
init(Name) ->

  Intf = intf:new(),
  Map = map:new(),
  Table = dijkstra:table(Intf, Map),
  History = [hist:new(Name)],
  router(Name, 0, History, Intf, Table, Map).


% Main loop
router(Name, N, History, Intf, Table, Map) ->
  receive

    {add, Node, Pid} ->
      Ref = erlang:monitor(process,Pid),
      Intf1 = intf:add(Node, Ref, Pid, Intf),
      router(Name, N, History, Intf1, Table, Map);

    {remove, Node} ->
      {ok, Ref} = intf:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = intf:remove(Node, Intf),
      router(Name, N, History, Intf1, Table, Map);

    {'DOWN', Ref, process, _, _}  ->
      {ok, Down} = intf:name(Ref, Intf),
      io:format("~w: Got exit from ~w~n", [Name, Down]),
      Intf1 = intf:remove(Down, Intf),
      router(Name, N, History, Intf1, Table, Map);

    {status, From} ->
      From ! {status, {Name, N, History, Intf, Table, Map}},
      router(Name, N, History, Intf, Table, Map);

    {links, Node, R, Links} ->
      case hist:update(Node, R, History) of
        {new, Hist1} ->
          intf:broadcast({links, Node, R, Links}, Intf),
          UpdatedMap = map:update(Node, Links, Map),
          router(Name, N, Hist1, Intf, Table, UpdatedMap);
        old ->
          router(Name, N, History, Intf, Table, Map)
      end;

    update ->
      Table1 = dijkstra:table(intf:list(Intf), Map),
      router(Name, N, History, Intf, Table1, Map);

    broadcast ->
      Message = {links, Name, N, intf:list(Intf)},
      intf:broadcast(Message, Intf),
      router(Name, N + 1, History, Intf, Table, Map);

    {route, Name, From, Message} ->
      io:format("~w: Got message from ~w:~s ~n", [Name, From, Message]),
      router(Name, N, History, Intf, Table, Map);

    {route, To, From, Message} ->
      io:format("~w: Routing message (~s)~n", [Name, Message]),
      case dijkstra:route(To, Table) of
        {ok, Gateway} ->
          case intf:lookup(Gateway, Intf) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};
            notfound ->
              ok
          end;
        notfound ->
          ok
      end,
      router(Name, N, History, Intf, Table, Map);

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, History, Intf, Table, Map);

    stop -> ok
  end.