-module(tood_db_user).
-include("tood_db.hrl").

-export([save/1]).
-export([find_by_id/1, find_by_name/1,
         collect_all/0]). %% TODO remove

-export([migrate/2]).


save(User) when is_record(User, tood_user) ->
    ?TRANSACTION(mnesia:write(tood_users, User, write)).

find_by_id(UserId) ->
    ?TRANSACTION(mnesia:match_object(tood_users, ?FILTER__USER__ID(UserId), read)).

find_by_name(UserName) ->
    ?TRANSACTION(mnesia:match_object(tood_users, ?FILTER__USER__NAME(UserName), read)).

collect_all() ->
    ?TRANSACTION(mnesia:match_object(tood_users, ?FILTER_ANY_USER, read)).

migrate(up, Nodes) ->
    {atomic, ok} = mnesia:create_table(tood_users,
                                       [{attributes, record_info(fields, tood_user)},
                                        {record_name, tood_user},
                                        {index, [#tood_user.name]},
                                        {disc_copies, Nodes}]),
    ok.
