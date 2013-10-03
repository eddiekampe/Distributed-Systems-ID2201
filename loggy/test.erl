%% Copyright
-module(test).
-author("eddkam").

%% API
-export([run/2]).


% Run test
run(Sleep, Jitter) ->

  Log = logger:start([john, paul, ringo, george]),

  A = worker:start(john, Log, 13, Sleep, Jitter),
  B = worker:start(paul, Log, 23, Sleep, Jitter),
  C = worker:start(ringo, Log, 36, Sleep, Jitter),
  D = worker:start(george, Log, 49, Sleep, Jitter),

  worker:peers(A, [B, C, D]),
  worker:peers(B, [A, C, D]),
  worker:peers(C, [A, B, D]),
  worker:peers(D, [A, B, C]),

  timer:sleep(5000),

  logger:stop(Log),
  worker:stop(A),
  worker:stop(B),
  worker:stop(C),
  worker:stop(D).


% Tests
%
% How do you know that they are printed in the wrong order?
% Increase/decrease Jitter, what happens to number of wrong entries?
% Can we eliminate the wrong entries?
%
% I did not manage to get all messages correct. Printout of received did happen before printout of send.


%% 2> test:run(5000, 5).
%%
%% Log: ringo na {received,{hello,57}}
%% Log: john na {sending,{hello,57}}

%% Log: john na {received,{hello,77}}
%% Log: ringo na {sending,{hello,77}}

%% Log: ringo na {received,{hello,68}}
%% Log: paul na {sending,{hello,68}}

%% Log: john na {received,{hello,20}}
%% Log: paul na {sending,{hello,20}}
%% Log: george na {received,{hello,20}}
%% Log: ringo na {sending,{hello,20}}

%% Log: george na {received,{hello,84}}
%% Log: john na {sending,{hello,84}}

%% Log: george na {received,{hello,16}}
%% Log: paul na {sending,{hello,16}}



%% 3> test:run(5000, 3).

%% Log: ringo na {received,{hello,57}}
%% Log: john na {sending,{hello,57}}

%% Log: john na {received,{hello,77}}
%% Log: ringo na {sending,{hello,77}}

%% Log: ringo na {received,{hello,68}}
%% Log: paul na {sending,{hello,68}}

%% Log: john na {received,{hello,20}}
%% Log: paul na {sending,{hello,20}}

%% Log: george na {received,{hello,20}}
%% Log: ringo na {sending,{hello,20}}

%% Log: george na {received,{hello,84}}
%% Log: john na {sending,{hello,84}}

%% Log: george na {received,{hello,16}}
%% Log: paul na {sending,{hello,16}}



%% 4> test:run(5000, 1).

%% Log: ringo na {received,{hello,57}}
%% Log: john na {sending,{hello,57}}

%% Log: john na {received,{hello,77}}
%% Log: ringo na {sending,{hello,77}}

%% Log: ringo na {received,{hello,68}}
%% Log: paul na {sending,{hello,68}}

%% Log: john na {received,{hello,20}}
%% Log: paul na {sending,{hello,20}}

%% Log: george na {received,{hello,20}}
%% Log: ringo na {sending,{hello,20}}

%% Log: george na {received,{hello,84}}
%% Log: john na {sending,{hello,84}}

%% Log: george na {received,{hello,16}}
%% Log: paul na {sending,{hello,16}}


% What is always true and what is sometimes true?

% Messages received happens always in correct order.
% Messages send can vary because of the jitter.