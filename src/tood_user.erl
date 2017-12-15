-module(tood_user).
-include("tood.hrl").

%% API
-export([sign_up/2,
         log_in/2,
         get_calendar/1]).

-export([create/2]).

%% Getters
-export([get_by_id/1,
         get_by_name/1]).

%%==================================================================================================
%% Implementation
%%==================================================================================================

%% Create ------------------------------------------------------------------------------------------

create(Name, Password) ->
    User = defaults(#tood_user{name = Name, password = erlpass:hash(Password)}),
    ok = tood_db_user:save(User),
    {ok, User}.

%% Sign up -----------------------------------------------------------------------------------------

sign_up(Name, Password) ->
    case get_by_name(Name) of
        undefined ->
            {ok, _} = create(Name, Password),
            ok;
        _User ->
            {error, already_registered}
    end.

%% Log in ------------------------------------------------------------------------------------------

log_in(Name, Password) ->
    case tood_db_user:find_by_name(Name) of
        [User] when is_record(User, tood_user)->
            case erlpass:match(Password, User#tood_user.password) of
                true ->
                    {ok, User#tood_user.id};
                _ ->
                    {error, bad_password}
            end;
        [] ->
            {error, not_registered};
        Else ->
            throw({impossible, {tood_user, log_in, Else}})
    end.

get_calendar(UserId) ->
    case get_by_id(UserId) of
        undefined -> undefined;
        _User ->
            Items = tood_db_item:find_by_user_id(UserId),
            #calendar{version = <<"2.0">>,
                      production_id = <<"-//Tood//Beta//EN">>,
                      todos = [Todo || #tood_item{todo = Todo} <- Items]}
    end.

%% Getters -----------------------------------------------------------------------------------------

get_by_id(UserId) ->
    case tood_db_user:find_by_id(UserId) of
        [] -> undefined;
        [User] -> User;
        _Else -> throw({impossible, {tood_user, get_by_id}})
    end.

get_by_name(Username) ->
    case tood_db_user:find_by_name(Username) of
        [] -> undefined;
        [User] -> User;
        _Else -> throw({impossible, {tood_user, get_by_name}})
    end.


%%==================================================================================================
%% @private
%%==================================================================================================

%% Fill default values -----------------------------------------------------------------------------

defaults(#tood_user{id = undefined} = User) ->
    defaults(User#tood_user{id = ?GENERATE_TOKEN});
defaults(User) when is_record(User, tood_user) ->
    User.

%%==================================================================================================
%% Test
%%==================================================================================================

-ifdef(EUNIT).
%% TODO
-endif.
