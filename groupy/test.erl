%% Copyright
-module(test).
-author("eddkam").

%% API
-export([start/2, start_same/3]).


start(Gms, Limit) ->

  Leader = worker:start(1, Gms, 500, 500),

  lists:foreach(
    fun(Id) ->
      worker:start(Id, Gms, 100*Id, Leader, 100*Id)
    end, lists:seq(0, Limit)
  ),

  Leader.


start_same(Gms, Limit, Time) ->

  Leader = worker:start(1, Gms, 500, Time),

  lists:foreach(
    fun(Id) ->
      worker:start(Id, Gms, 500, Leader, Time)
    end, lists:seq(0, Limit)
  ),

  Leader.