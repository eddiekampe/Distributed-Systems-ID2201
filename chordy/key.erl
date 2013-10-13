%% Copyright
-module(key).
-author("eddkam").

-define(RANDOM, 1000000000). % 30-bits

%% API
-export([generate/0, between/3]).


% Generate identifier
generate() ->
  Time = now(),
  random:seed(Time, Time, Time),
  random:uniform(?RANDOM).


% Check whether Key is within the interval: (From, To]
between(Key, From, To) ->

  if
    From == To -> true;
    From < To -> (From < Key) and (Key =< To);
    From > To -> (From < Key) or (Key =< To)
  end.