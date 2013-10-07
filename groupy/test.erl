%% Copyright
-module(test).
-author("eddkam").

%% API
-export([start/1, start_same/2]).


start(Gms) ->

  Leader = worker:start(1, Gms, 500, 500),
  worker:start(2, Gms, 1000, Leader, 1000),
  worker:start(3, Gms, 700, Leader, 700),
  worker:start(4, Gms, 400, Leader, 400),

  timer:sleep(10000),

  worker:start(5, Gms, 900, Leader, 900),

  Leader.


start_same(Gms, Time) ->

  Leader = worker:start(1, Gms, 500, Time),
  worker:start(2, Gms, 500, Leader, Time),
  worker:start(3, Gms, 500, Leader, Time),
  worker:start(4, Gms, 500, Leader, Time),
  worker:start(5, Gms, 500, Leader, Time),
  worker:start(6, Gms, 500, Leader, Time),

  Leader.
