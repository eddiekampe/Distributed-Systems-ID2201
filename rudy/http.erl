-module(http).
-author("eddkam").

%% Macros
-define(CR, 13).
-define(LF, 10).
-define(SPACE, 32).

%% API
-export([parse_request/1, ok/1, get/1]).


% Extract URI
request_uri([?SPACE | R0]) -> {[], R0};
request_uri([C | R0]) ->

  {Rest, R1} = request_uri(R0),
  {[C | Rest], R1}.


% Extract HTTP version
http_version([$H, $T, $T, $P, $/, $1, $., $1 | RO]) -> {v11, RO};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | RO]) -> {v10, RO}.


% Accept only GET requests for now
request_line([$G, $E, $T, ?SPACE | R0]) ->

  {URI, R1} = request_uri(R0),
  {Ver, R2} = http_version(R1),
  [?CR, ?LF | R3] = R2,
  {{get, URI, Ver}, R3}.


% Extract header
header([?CR, ?LF | R0]) -> {[], R0};
header([C | R0]) ->

  {Rest, R1} = header(R0),
  {[C | Rest], R1}.


% Parse headers
headers([?CR, ?LF | R0]) -> {[], R0};
headers(R0) ->

  {Header, R1} = header(R0),
  {Rest, R2} = headers(R1),
  {[Header|Rest], R2}.


% Parse body
message_body(R) -> {R, []}.


% Parse request
parse_request(R0) ->

  {Request, R1} = request_line(R0),
  {Headers, R2} = headers(R1),
  {Body, _} = message_body(R2),
  {Request, Headers, Body}.


% Helper functions
ok(Body) -> "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.
get(URI) -> "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".