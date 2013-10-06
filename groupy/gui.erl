%% Copyright
-module(gui).
-author("eddkam").

%% Macros
-define(WIDTH, 200).
-define(HEIGHT, 200).

%% API
-export([start/2]).

%% Include
-include_lib("wx/include/wx.hrl").


% Spawn a new GUI
start(Title, Master) -> spawn_link(fun() -> init(Title, Master) end).


% Initialize Window
init(Title, Master) ->

  Window = make_window(Title),
  loop(Window, Master).


% Create the actual Window
make_window(Title) ->

  Server = wx:new(),
  Frame = wxFrame:new(Server, -1, Title, [{size, {?WIDTH, ?HEIGHT}}]),
  wxFrame:setBackgroundColour(Frame, ?wxBLACK),
  wxFrame:show(Frame),

  Window = wxWindow:new(Frame, ?wxID_ANY),
  wxWindow:setBackgroundColour(Window, ?wxBLACK),
  wxWindow:show(Window),

  wxFrame:connect(Frame, close_window),
  Window.


% Loop
loop(Window, Master) ->

  receive

    #wx{event=#wxClose{}} ->

      wxWindow:destroy(Window),
      Master ! stop,
      ok;

    {color, Color} ->
      color(Window, Color),
      loop(Window, Master);

    stop -> ok;

    Error ->
      io:format("[GUI] Strange message ~w~n", [Error]),
      loop(Window, Master)

  end.


% Set background color
color(Window, Color) ->
  wxWindow:setBackgroundColour(Window, Color),
  wxWindow:refresh(Window).