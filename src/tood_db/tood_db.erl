-module(tood_db).

%% Includes
-include("tood_db.hrl").

%% API
-export([start/0,
         stop/0,

         bootstrap/1]).

%%==============================================================================
%% Implementation
%%==============================================================================
%%------------------------------------------------------------------------------
%% Public functions
%%------------------------------------------------------------------------------
start() ->
    case mnesia:wait_for_tables(?TABLES, ?TIMEOUT) of
        ok ->
            ok;
        {timeout, _Tables} ->
            ok;
        {error, Results} ->
            {error, Results}
    end.

% TODO
stop() ->
    ok.

bootstrap(i_know_what_i_am_doing) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    lists:foreach(fun(Model) -> Model:migrate(up, [node()]) end, ?MODELS);

% TODO remove
bootstrap(i_should_know_what_i_am_doing) ->
    bootstrap(i_know_what_i_am_doing),

    tood_fixtures:fill_db().

