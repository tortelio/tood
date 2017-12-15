-module(tood_SUITE).
-include("tood_test.hrl").

-compile(export_all).

all() -> [register_user,
          try_to_log_in_an_unregistered_user,
          log_in_a_registered_user,
          get_ics
         ].

%%------------------------------------------------------------------------------
%% SUITE init/end
%%------------------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%------------------------------------------------------------------------------
%% TESTCASE init/end
%%------------------------------------------------------------------------------

init_per_testcase(log_in_a_registered_user, Config0) ->
    Config = init_per_testcase(common, Config0),

    ok = tood_db_user:save(tood_fixtures:user(example_user)),

    Config;

init_per_testcase(get_ics, Config0) ->
    Config = init_per_testcase(log_in_a_registered_user, Config0),

    ok = tood_db_item:save(tood_fixtures:item(for_example_user)),

    Config;

init_per_testcase(common, Config) ->
    {ok, _} = application:ensure_all_started(tood),
    {ok, _} = application:ensure_all_started(gun),

    ok = tood_db:bootstrap(i_know_what_i_am_doing),
    Config;

init_per_testcase(_, Config) ->
    init_per_testcase(common, Config).

end_per_testcase(_, _Config) ->
    ok = application:stop(gun),
    ok = application:stop(tood),
    ok.

register_user(_Config) ->
    {ok, Conn} = http_client:connect("localhost", 8080),

    ?assertEqual(#{<<"state">> => <<"approved">>},
                http_client:send_4_reply(Conn, "/sign-up",
                                         #{<<"username">> => ?USERNAME,
                                           <<"password">> => ?PASSWORD})),

    ?assertEqual(#{<<"state">> => <<"refused">>,
                   <<"reason">> => <<"already_registered">>},
                 http_client:send_4_reply(Conn, "/sign-up",
                                          #{<<"username">> => ?USERNAME,
                                            <<"password">> => ?PASSWORD})),

    ok = http_client:disconnect(Conn),
    ok.

try_to_log_in_an_unregistered_user(_Config) ->
    {ok, Conn} = http_client:connect("localhost", 8080),

    ?assertEqual(#{<<"state">> => <<"refused">>,
                   <<"reason">> => <<"not_registered">>},
                 http_client:send_4_reply(Conn, "/log-in",
                                          #{<<"username">> => ?USERNAME,
                                            <<"password">> => ?PASSWORD})),

    ok = http_client:disconnect(Conn),
    ok.

log_in_a_registered_user(_Config) ->
    {ok, HttpConn} = http_client:connect("localhost", 8080),

    Response = http_client:send_4_reply(HttpConn, "/log-in",
                                        #{<<"username">> => ?USERNAME,
                                          <<"password">> => ?PASSWORD}),

    ?assertEqual(maps:get(<<"state">>, Response), <<"approved">>),
    ?assertEqual(maps:get(<<"user-id">>, Response), ?USER_ID),
    ?assertEqual(maps:get(<<"username">>, Response), ?USERNAME),

    #{<<"token">> := Token} = Response,

    ok = http_client:disconnect(HttpConn),

    {ok, WSConn} = ws_client:connect("localhost", 8080, "/websocket/" ++ Token),

    ?assertEqual(#{<<"type">> => <<"get-items">>,
                   <<"items">> => []},
                 ws_client:send_for_reply(WSConn, #{<<"type">> => <<"get-items">>})),

    Item = #{
      <<"alarm">>         => undefined,
      <<"created_at">>    => 1513072191666,
      <<"sequence">>      => 1,
      <<"id">>            => <<"user@example.com">>,
      <<"due">>           => 1513472191666,
      <<"status">>        => <<"needs_action">>,
      <<"summary">>       => <<"Submit Income Taxes">>,
      <<"x">>             => #{<<"TOOD-ID">> => ?ITEM_ID}
     },

    ok = ws_client:send(WSConn, #{<<"type">> => <<"create-item">>,
                                  <<"item">> => Item}),

    Item2 = Item#{<<"summary">> => <<"Submit Income more Taxes">>,
                  <<"sequence">> => 2},

    ok = ws_client:send(WSConn, #{<<"type">> => <<"update-item">>,
                                  <<"item">> => Item2}),

    ?assertEqual(#{<<"type">> => <<"get-items">>,
                   <<"items">> => [#{<<"alarm">> => <<"undefined">>,
                                     <<"created_at">> => 1513072191,
                                     <<"due">> => 1513472191,
                                     <<"id">> => <<"user@example.com">>,
                                     <<"sequence">> => 2,
                                     <<"status">> => <<"needs_action">>,
                                     <<"summary">> => <<"Submit Income more Taxes">>,
                                     <<"x">> => #{<<"TOOD-ID">> => ?ITEM_ID}}]},
                 ws_client:send_for_reply(WSConn, #{<<"type">> => <<"get-items">>})),

    ok = ws_client:send(WSConn, #{<<"type">> => <<"delete-item">>,
                                  <<"item">> => Item2}),

    ?assertEqual(#{<<"type">> => <<"get-items">>,
                   <<"items">> => []},
                 ws_client:send_for_reply(WSConn, #{<<"type">> => <<"get-items">>})),

    ok = ws_client:disconnect(WSConn),
    ok.

get_ics(Config) ->
    {ok, Conn} = http_client:connect("localhost", 8080),

    ?assertEqual(?READ_FILE("items.ics", Config),
                 http_client:get(Conn, "/todos/" ++ binary_to_list(?USER_ID))),

    ok = http_client:disconnect(Conn),
    ok.

