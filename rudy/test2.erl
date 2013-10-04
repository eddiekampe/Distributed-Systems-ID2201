-module(test2).
-author("eddkam").

%% API
-export([request/2, bench/4]).

%
%% This benchmark uses asynchronous requests from N processes.
%% It spawns a given number of workers and sends a Number of requests from each process.
%


% Benchmark Host running @ Port with Amount workers, each doing N requests
bench(Host, Port, Amount, N) ->

  Start = now(),
  % Start workers and wait for callback
  register(benchmark, spawn(fun() -> wait_for_callback(Amount, Start) end)),

  % Spawn workers
  spawn_workers(Amount, Host, Port, supervisor, N).


% Spawn workers handling requests
spawn_workers(0, _Host, _Port, _Supervisor, _Number) -> ok;
spawn_workers(WorkersLeft, Host, Port, Supervisor, Number) ->

  spawn(fun() ->
    run(Number, Host, Port),
    Supervisor ! finished
  end),

  spawn_workers(WorkersLeft-1, Host, Port, Supervisor, Number).


% Wait until all workers report back, then we know that we are done
wait_for_callback(0, Start) ->

  TimeDiff = timer:now_diff(now(), Start),
  io:format("Time difference: ~p~n", [TimeDiff]);

wait_for_callback(Amount, Start) ->

  receive
    finnished -> wait_for_callback(Amount-1, Start)
  end.


% Make N requests to Host @ Port
run(N, Host, Port) ->

  if
    N == 0 ->
      ok;
    true ->
      request(Host,Port),
      run(N-1,Host,Port)
  end.


% Send request to Host @ Port
request(Host,Port) ->

  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:connect(Host,Port,Opt) of

    {ok, Server} ->
      gen_tcp:send(Server,http:get("foo")),
      Recv = gen_tcp:recv(Server,0),

      case Recv of
        {ok, _} -> ok;
        {error,Error} -> io:format("[Test] Error: ~w~n", [Error])
      end;

    {error, Reason} ->
      io:format("[Test] Error: ~w~n", [Reason])

  end.

