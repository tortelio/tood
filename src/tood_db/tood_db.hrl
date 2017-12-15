%% Database depended header
-ifndef(TOOD_DB_HRL).
-define(TOOD_DB_HRL, true).

-include("../tood.hrl").

-define(TIMEOUT, 5000).

-define(MODELS, [tood_db_user, tood_db_item]).
-define(TABLES, [tood_users, tood_items]).

-define(FILTER_ANY_USER, #tood_user{_ = '_'}).
-define(FILTER_ANY_USER(Part), #tood_user{_ = '_', Part}).
-define(FILTER__USER__ID(Id), ?FILTER_ANY_USER(id = Id)).
-define(FILTER__USER__NAME(Name), ?FILTER_ANY_USER(name = Name)).

-define(FILTER_ANY_ITEM, #tood_item{_ = '_'}).
-define(FILTER_ANY_ITEM(Part), #tood_item{_ = '_', Part}).
-define(FILTER__ITEM__ID(Id), ?FILTER_ANY_ITEM(id = Id)).
-define(FILTER__ITEM__USER_ID(UserId), ?FILTER_ANY_ITEM(user_id = UserId)).

-define(TRANSACTION(Exprs), mnesia:activity(transaction, fun() -> Exprs end)).

-endif.
