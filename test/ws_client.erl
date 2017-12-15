-module(ws_client).

-export([connect/3,
         disconnect/1,
         send/2,
         send_for_reply/2]).

connect(Url, Port, WSPath) ->
    {ok, Conn} = gun:open(Url, Port),

    _Ref = gun:ws_upgrade(Conn, WSPath),
    ok =
    receive
        {gun_ws_upgrade, Conn, ok, _Headers} -> ok
    after
        1000 -> throw(missing_successful_upgrading)
    end,
    {ok, http} = gun:await_up(Conn),
    {ok, Conn}.

disconnect(Conn) ->
    ok = gun:ws_send(Conn, close),

    ok.

send(Conn, Msg) ->
    gun:ws_send(Conn, {binary, jiffy:encode(Msg)}).

send_for_reply(Conn, Msg) ->
    ok = send(Conn, Msg),
    get_response(Conn).

get_response(Conn) ->
    receive
        {gun_ws, Conn, {_, Data}} ->
            tood_test:decode(Data)
    after
        1000 -> throw({missing_message, Conn})
    end.
