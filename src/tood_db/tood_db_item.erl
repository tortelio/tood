-module(tood_db_item).
-include("tood_db.hrl").

-export([save/1,
         find_by_id/1,
         find_by_user_id/1,
         delete_by_id/1,
         migrate/2]).

-export([collect_all/0]).

save(Item) when is_record(Item, tood_item) ->
    ?TRANSACTION(mnesia:write(tood_items, Item, write)).

find_by_id(ItemId) ->
    case ?TRANSACTION(mnesia:match_object(tood_items, ?FILTER__ITEM__ID(ItemId), read)) of
        [] -> undefined;
        [Item] -> Item
    end.

find_by_user_id(UserId) ->
    ?TRANSACTION(mnesia:match_object(tood_items, ?FILTER__ITEM__USER_ID(UserId), read)).

delete_by_id(ItemId) ->
    ?TRANSACTION(mnesia:delete(tood_items, ItemId, write)).

collect_all() ->
    ?TRANSACTION(mnesia:match_object(tood_items, ?FILTER_ANY_ITEM, read)).

migrate(up, Nodes) ->
    {atomic, ok} = mnesia:create_table(tood_items,
                                       [{attributes, record_info(fields, tood_item)},
                                        {record_name, tood_item},
                                        {index, []},
                                        {disc_copies, Nodes}]),
    ok.
