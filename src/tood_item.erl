-module(tood_item).
-include("tood.hrl").

-export([create/2,
         update/1,
         delete/1,
         find_by_user_id/1]).

-export([prepare/1]).

-export([todo/1,
         todo_to_map/1,
         map_to_todo/1]).

create(Username, ICSTodo) when is_binary(ICSTodo) ->
    create(Username, eics:decode(todo, ICSTodo));

create(Username, Todo) when is_map(Todo) ->
    Item = default(#tood_item{user_id = Username, todo = map_to_todo(Todo)}),
    ok = tood_db_item:save(Item),
    ok.

prepare(#{<<"created_at">> := CreatedAt,
          <<"due">> := Due} = Todo) ->
    Todo#{<<"created_at">> := qdate:to_date(trunc(CreatedAt/1000)),
          <<"due">> := qdate:to_date(trunc(Due/1000))}.

update(ICSTodo) when is_binary(ICSTodo) ->
    Todo = eics:decode(todo, ICSTodo),
    update(Todo);

update(Todo0) when is_map(Todo0) ->
    Todo = map_to_todo(Todo0),

    #{<<"TOOD-ID">> := ItemId} = eics:x(Todo),

    case tood_db_item:find_by_id(ItemId) of
        undefined -> {error, missing_item};
        OldItem ->
            NewItem = update(OldItem, Todo),
            tood_db_item:save(NewItem)
    end.

update(OldItem, Todo) ->
    OldItem#tood_item{todo = Todo}.

delete(Todo) when is_map(Todo) ->
    #{<<"x">> := #{<<"TOOD-ID">> := ItemId}} = Todo,

    io:format("DELETE: ~p~n", [ItemId]),
    tood_db_item:delete_by_id(ItemId).

find_by_user_id(UserId) -> tood_db_item:find_by_user_id(UserId).

default(#tood_item{id = undefined, todo = Todo} = Item) ->
    #{<<"TOOD-ID">> := ItemId} = eics:x(Todo),

    % TODO
    true = uuid:is_valid(binary_to_list(ItemId)),

    default(Item#tood_item{id = ItemId});

default(#tood_item{id = Id,
                   user_id = UserId,
                   todo = Todo} = Item)
  when Id =/= undefined andalso
       UserId =/= undefined andalso
       Todo =/= undefined ->
    Item.

todo(#tood_item{todo = Todo}) -> Todo.

todo_to_map(#tood_item{
               todo = #todo{
                         created_at = CreatedAt,
                         sequence   = Sequence,
                         id         = Id,
                         due        = Due,
                         status     = Status,
                         summary    = Summary,
                         alarm      = Alarm,
                         x          = X
                        }
              } = Item) ->
    Item#tood_item{
      todo = #{created_at   => qdate:to_unixtime(CreatedAt),
               sequence     => Sequence,
               id           => Id,
               due          => qdate:to_unixtime(Due),
               status       => Status,
               summary      => Summary,
               alarm        => Alarm,
               x            => X
              }
     }.

map_to_todo(Map) ->
    io:format("MAP: ~p~n", [Map]),
    #todo{
       created_at = maps:get(<<"created_at">>,   Map, undefined),
       sequence   = maps:get(<<"sequence">>,     Map, undefined),
       id         = maps:get(<<"id">>,           Map, undefined),
       due        = maps:get(<<"due">>,          Map, undefined),
       status     = encode_status(maps:get(<<"status">>, Map)),
       summary    = maps:get(<<"summary">>,      Map, undefined),
       alarm      = maps:get(<<"alarm">>,        Map, undefined),
       x          = maps:get(<<"x">>,            Map, #{})
      }.

encode_status(<<"needs_action">>) ->
    needs_action;
encode_status(<<"completed">>) ->
    complete;
encode_status(<<"in_process">>) ->
    in_process;
encode_status(<<"cancelled">>) ->
    cancelled;
encode_status(Status) ->
    throw({unknown_status, Status}).
