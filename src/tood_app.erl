%%%-------------------------------------------------------------------------------------------------
%% @doc tood public API
%% @end
%%%-------------------------------------------------------------------------------------------------

-module(tood_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(APPLICATION, tood).
-define(LISTENER, tood_tcp_listener).

%%==================================================================================================
%% API
%%==================================================================================================

start(_StartType, _StartArgs) ->
    ets:new(active_users, [set, named_table, public]),
    Dispatch = cowboy_router:compile([{'_', [{"/",                  cowboy_static,      {priv_file, ?APPLICATION, "static/index.html"}},
                                             {"/assets/[...]",      cowboy_static,      {priv_dir,  ?APPLICATION, "static/assets"}},
                                             {"/sign-up",           sign_up_handler,    []},
                                             {"/log-in",            log_in_handler,     []},
                                             {"/websocket/:token",  ws_handler,         []},
                                             {"/todos/:user_id",    calendar_handler,   []}
                                            ]}
                                     ]),
    Port = application:get_env(?APPLICATION, port, 8080),

    {ok, _} = cowboy:start_clear(?LISTENER,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    ok = tood_db:start(),
    tood_sup:start_link().

%%--------------------------------------------------------------------------------------------------
stop(_State) ->
    ok = tood_db:stop(),
    cowboy:stop_listener(?LISTENER).

%%==================================================================================================
%% Internal functions
%%==================================================================================================
