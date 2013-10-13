%% Copyright
-module(key).
-author("eddkam").

-define(RANDOM, 1000000000).

%% API
-export([generate/0, between/3]).


% Generate a random Id, ensure unique seed
generate() ->
  Time = now(),
  random:seed(Time, Time, Time),
  random:uniform(?RANDOM).


% (Key, From, To). Check if From == To
between(_Key, Value, Value) -> true;
% Key within (From, To] ?
between(Key, From, To) ->

  if
    From < To -> (From < Key) and (Key =< To);
    From > To -> (From < Key) or (Key =< To)
  end.