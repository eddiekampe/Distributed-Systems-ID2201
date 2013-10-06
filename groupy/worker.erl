%% Copyright
-module(worker).
-author("eddkam").

%% Macros
-define(CHANGE, 20).
-define(COLOR, {0, 0, 0}).

%% API
-export([start/4, start/5]).


% Start/4
start(Id, Module, Random, Sleep) ->
  spawn(fun() -> init(Id, Module, Random, Sleep) end).


% Init/4
init(Id, Module, Random, Sleep) ->

  {ok, Cast} = apply(Module, start, [Id]),
  init_cont(Id, Random, Cast, ?COLOR, Sleep).


% Start/5
start(Id, Module, Random, Peer, Sleep) ->
  spawn(fun() -> init(Id, Module, Random, Peer, Sleep) end).


% Init/5
init(Id, Module, Random, Peer, Sleep) ->

  {ok, Cast} = apply(Module, start, [Id, Peer]),
  {ok, Color} = join(Id, Cast),
  init_cont(Id, Random, Cast, Color, Sleep).


% Join broadcast
join(Id, Cast) ->

  receive
    {view, _} ->

      Ref = make_ref(),
      Cast ! {mcast, {state_request, Ref}},
      state(Id, Ref);

    {error, Reason} -> {error, Reason}
  end.


% State
state(Id, Ref) ->

  receive
    {state_request, Ref} ->
      receive
        {state, Ref, Color} -> {ok, Color}
      end;

    _Ignore -> state(Id, Ref)
  end.


% Init cont
init_cont(Id, Random, Cast, Color, Sleep) ->

  random:seed(Random, Random, Random),
  Title = "Worker: " ++ integer_to_list(Id),
  GUI = gui:start(Title, self()),
  GUI ! {color, Color},
  worker(Id, Cast, Color, GUI, Sleep),
  Cast ! stop,
  GUI ! stop.


% Worker
worker(Id, Cast, Color, GUI, Sleep) ->

  Wait = if
    Sleep == 0 -> 0;
    true -> random:uniform(Sleep)
  end,

  receive

    {change, N} ->

      NewColor = change_color(N, Color),
      GUI ! {color, NewColor},
      worker(Id, Cast, NewColor, GUI, Sleep);

    {state_request, Ref} ->

      Cast ! {mcast, {state, Ref, Color}},
      worker(Id, Cast, Color, GUI, Sleep);

    {state, _,  _} -> worker(Id, Cast, Color, GUI, Sleep);

    {join, Peer, Gms} ->

      io:format("[Worker-~w] Worker got join: ~w ~w~n", [Id, Peer, Gms]),
      Cast ! {join, Peer, Gms},
      worker(Id, Cast, Color, GUI, Sleep);

    {view, _} -> worker(Id, Cast, Color, GUI, Sleep);

    stop -> ok;

    Error ->

      io:format("[Worker-~w] Received strange message: ~w~n", [Id, Error]),
      worker(Id, Cast, Color, GUI, Sleep)

  after Wait ->

    Cast ! {mcast, {change, random:uniform(?CHANGE)}},
    worker(Id, Cast, Color, GUI, Sleep)

  end.


% Change color
change_color(N, {R, G, B}) -> {G, B, ((R+N rem 256))}.