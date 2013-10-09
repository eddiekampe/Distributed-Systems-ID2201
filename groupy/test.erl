%% Copyright
-module(test).
-author("eddkam").

%% API
-export([start/1, start_same/2, kristian_connect/2, kristian_init/2]).


start(Gms) ->

  Leader = worker:start(1, Gms, 500, 500),
  worker:start(2, Gms, 1000, Leader, 1000),
  worker:start(3, Gms, 700, Leader, 700),
  worker:start(4, Gms, 400, Leader, 400),
  worker:start(5, Gms, 900, Leader, 900),

  Leader.


start_same(Gms, Time) ->

  Leader = worker:start(1, Gms, 500, Time),
  worker:start(2, Gms, 500, Leader, Time),
  worker:start(3, Gms, 500, Leader, Time),
  worker:start(4, Gms, 500, Leader, Time),
  worker:start(5, Gms, 500, Leader, Time),
  worker:start(6, Gms, 500, Leader, Time),
  worker:start(7, Gms, 500, Leader, Time),
  worker:start(8, Gms, 500, Leader, Time),
  worker:start(9, Gms, 500, Leader, Time),
  worker:start(10, Gms, 500, Leader, Time),
  worker:start(11, Gms, 500, Leader, Time),
  worker:start(12, Gms, 500, Leader, Time),
  worker:start(13, Gms, 500, Leader, Time),
  worker:start(14, Gms, 500, Leader, Time),
  worker:start(15, Gms, 500, Leader, Time),
  worker:start(16, Gms, 500, Leader, Time),
  worker:start(17, Gms, 500, Leader, Time),
  worker:start(18, Gms, 500, Leader, Time),

  Leader.


kristian_init(Gms, Time) ->

  Leader = worker:start(1, Gms, 500, Time),
  register(orebro, Leader),

  Leader.


kristian_connect(Gms, Time) ->

  net_adm:ping('denmark@130.229.169.212'),
  worker:start(2, Gms, 500, {copenhagen, 'denmark@130.229.169.212'}, Time).