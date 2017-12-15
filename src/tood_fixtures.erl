-module(tood_fixtures).
-include("tood.hrl").

-export([fill_db/0]).
-export([user/1,
         item/1]).

fill_db() ->
    tood_db_user:save(user(a)),
    tood_db_item:save(item(for_a)).

user(a) ->
    #tood_user{id = <<"0822d5da-6475-520c-bb52-597babc2982a">>,
               name = <<"a">>,
               password = erlpass:hash(<<"a">>)};
user(example_user) ->
    #tood_user{id = <<"f6e3b61a-65cc-413a-8105-905eeec3358a">>,
               name = <<"user@example.com">>,
               password = erlpass:hash(<<"s3cr3t">>)}.


item(for_a) ->
    ItemId = <<"7c63bb33-ba8b-5044-b4ef-b2904d6a283c">>,
    #tood_item{id = ItemId,
               user_id = <<"0822d5da-6475-520c-bb52-597babc2982a">>,
               todo = #todo{
                         created_at = {{2017, 12, 10}, {13, 45, 00}},
                         sequence = 2,
                         id = <<"a">>,
                         due = {{2018, 3, 10}, {23, 59, 59}},
                         status = in_process,
                         summary = <<"Buy milk">>,
                         x = #{<<"TOOD-ID">> => ItemId}
                        }};

item(for_example_user) ->
    ItemId = <<"bfb644d2-5c4a-583e-9e70-6a6715f2d88f">>,
    #tood_item{id = ItemId,
               user_id = <<"f6e3b61a-65cc-413a-8105-905eeec3358a">>,
               todo = #todo{
                         created_at = {{1998, 1, 30}, {13, 45, 00}},
                         sequence = 1,
                         id = <<"user@example.com">>,
                         due = {{1998, 4, 15}, {23, 59, 59}},
                         status = needs_action,
                         summary = <<"Submit Income Taxes">>,
                         x = #{<<"TOOD-ID">> => ItemId}
                        }}.
