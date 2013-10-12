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

  case Key of

    Key > From >= To ->
      true;

    To > From ->
      false

  end.


%% The between/3 function will check if a Key is between From and To or
%% equal to To, this is called a partly closed interval and is denoted (F rom, T o].
%% Remember that the we’re dealing with a ring so it could be that From
%% is larger than To. What does that mean and how do you handle it? Also,
%% From could be equal to To and we will interpret this as the full circle i.e.
%% anything is in between.