%% Copyright
-module(worker).
-author("eddkam").

%% API
-export([start/5, stop/1, peers/2]).


% Start worker
start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).


% Stop Worker
stop(Worker) -> Worker ! stop.


% Initialize
init(Name, Log, Seed, Sleep, Jitter) ->

  random:seed(Seed, Seed, Seed),
  receive
    {peers, Peers} -> loop(Name, Log, Peers, Sleep, Jitter, 0);
    stop -> ok
  end.


% Send Peers to Worker
peers(Worker, Peers) -> Worker ! {peers, Peers}.


% Loop
loop(Name, Log, Peers, Sleep, Jitter, WorkerTime) ->

  Wait = random:uniform(Sleep),

  receive

    {msg, ReceivedTime, ReceivedMessage} ->

      Time = max(WorkerTime, ReceivedTime) + 1,
      Log ! {log, Name, Time, {received, ReceivedMessage}},
      loop(Name, Log, Peers, Sleep, Jitter, Time);

    stop -> ok;
    Error -> Log ! {log, Name, time, {error, Error}}

  after Wait ->

    Time = WorkerTime + 1,
    Peer = select_random(Peers),
    Message = {hello, random:uniform(100)},
    Peer ! {msg, Time, Message},
    jitter(Jitter),
    Log ! {log, Name, Time, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter, Time)

  end.


% Select random peer
select_random(Peers) -> lists:nth(random:uniform(length(Peers)), Peers).


% Jitter
jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).