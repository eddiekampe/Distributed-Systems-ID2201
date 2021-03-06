-module(test).
-author("eddkam").

%% API
-export([bench/3, request/2]).

%
%% This benchmark uses synchronous requests from one process
%

% Benchmark Host running @ Port with NumberOfRequests
bench(Host, Port, NumberOfRequests) ->

  Start = now(),
  run(NumberOfRequests, Host, Port),
  Finish = now(),
  timer:now_diff(Finish, Start).


% Make N requests to Host @ Port
run(N, Host, Port) ->

  if
    N == 0 ->
      ok;
    true ->
      request(Host, Port),
      run(N-1, Host, Port)
  end.


% Send request to Host @ Port
request(Host, Port) ->

  Opt = [list, {active, false}, {reuseaddr, true}],
  {ok, Server} = gen_tcp:connect(Host, Port, Opt),
  gen_tcp:send(Server, http:get("foo")),

  Recv = gen_tcp:recv(Server, 0),

  case Recv of
    {ok, _} -> ok;
    {error, _Error} -> error
  end.