-module(http_client).
-include("tood_test.hrl").

-export([connect/2,
         disconnect/1,
         get/2]).

-export([send_4_reply/3,
         send_4_redirection/3]).

connect(Url, Port) ->
    {ok, Conn} = gun:open(Url, Port),

    {ok, Conn}.

disconnect(Conn) ->
    ok = gun:close(Conn).

get(Conn, Path) ->
    Ref = gun:get(Conn, Path),
    {ok, Reply} = gun:await_body(Conn, Ref),
    Reply.

send_4_reply(Conn, Path, Msg) ->
    Ref = do_send(Conn, Path, Msg),
    {ok, Reply} = gun:await_body(Conn, Ref),
    jiffy:decode(Reply, [return_maps]).

send_4_redirection(Conn, Path, Msg) ->
    {Ref, StatusCode, Headers} = send_4_status_code(Conn, Path, Msg),
    ?assert((StatusCode >= 300) and (308 >= StatusCode)),
    {ok, Reply} = gun:await_body(Conn, Ref),
    URL = proplists:get_value(<<"location">>, Headers),
    {StatusCode, URL, tood_test:decode(Reply)}.

send_4_status_code(Conn, Path, Msg) ->
    Ref = do_send(Conn, Path, Msg),
    {response, nofin, StatusCode, Headers} = gun:await(Conn, Ref),

    {Ref, StatusCode, Headers}.

do_send(Conn, Path, Msg) ->
    PackedMsg = jiffy:encode(Msg),
    gun:post(Conn, Path,
             [{<<"accept">>, "application/json"},
              {<<"content-type">>, "application/json"}],
             PackedMsg).
