%% Copyright
-module(client).
-author("eddkam").

-define(SLEEP, 500).

%% API
-export([start/2]).


start(Id, PidList) ->
  spawn(fun() -> init(Id, PidList) end).


init(Id, PidList) ->

  lists:foreach(
    fun(Pid) ->

      Pid ! {add, random:uniform(length(PidList)),
                  random:uniform(500),
                  make_ref(),
                  self()}

    end, PidList
  ),

  loop(Id, PidList).


loop(Id, PidList) ->

  timer:sleep(?SLEEP),

  Random = random:uniform(length(PidList)),
  Pid = lists:nth(Random, PidList),

  Qref = make_ref(),
  Pid ! {lookup, random:uniform(Random), Qref, self()},

  receive

    {Qref, Result} ->
      io:format("[Client-~w] Lookup: ~w~n", [Id, Result]),
      loop(Id, PidList);

     Error ->
       io:format("[Client-~w] Other message: ~w~n", [Id, Error]),
       loop(Id, PidList)

  end.
