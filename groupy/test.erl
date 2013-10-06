%% Copyright
-module(test).
-author("eddkam").

%% API
-export([start/1]).


start(Gms) ->

  Leader = worker:start(1, Gms, 500, 500),
  worker:start(2, Gms, 1000, Leader, 1000),
  worker:start(3, Gms, 700, Leader, 700),
  worker:start(4, Gms, 300, Leader, 300),


  Leader.