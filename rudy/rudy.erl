-module(rudy).
-author("eddkam").

%% API
-export([init/2, handler/1, request/1, reply/1, start/2, stop/0]).


% Start Rudy; specify Port and Number of workers in the server pool
start(Number, Port) ->

  Pid = spawn(?MODULE, init, [Number, Port]),
  register(rudy, Pid).


% Stop Rudy
stop() -> rudy ! {stop}.


% Initialize. Setup server pool with Number workers listening to Port
init(Number, Port) ->

  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->

      % Spawn linked workers
      PidList = lists:map(
        fun(_) ->
          spawn_link(?MODULE, handler, [Listen])
        end, lists:seq(1, Number)
      ),

      io:format("[Rudy] Successfully set up ~w process(es). Server pool listening @ ~w ~n", [length(PidList), Port]),

      % Wait for stop
      receive
        {stop} -> ok
      end,

      gen_tcp:close(Listen);

    {error, Error} ->
      io:format("[Rudy] Failed listening because of ~w ~n", [Error])
  end.


% Listen to socket for incoming requests
handler(Listen) ->

  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      rudy:request(Client),
      rudy:handler(Listen);
    {error, _} ->
      error
  end.


% Accept request, parse and reply
request(Client) ->

  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response);
    {error, Error} ->
      io:format("[~w] Error: ~w~n", [self(), Error])
  end,
  gen_tcp:close(Client).


% Construct HTTP 200 OK
reply({{get, URI, _}, _, _}) ->

  % Simulate file handling, add small delay
  timer:sleep(40),
  http:ok(URI).