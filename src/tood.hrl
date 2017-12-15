-ifndef(TOOD_HRL).
-define(TOOD_HRL, true).

-define(APPLICATION, tood).

-include_lib("eics/include/eics.hrl").

-record tood_user, {
          id        :: uuid:uuid(),
          name      :: binary(),
          password  :: binary()
         }.

-type tood_user() :: #tood_user{}.

-record tood_item, {
          id        :: uuid:uuid(),
          user_id   :: uuid:uuid(),
          todo      :: eics:todo()
         }.

-type tood_item() :: #tood_item{}.

-define(WS_OPTS, #{idle_timeout => 3600000}).

-define(GENERATE_TOKEN, list_to_binary(uuid:to_string(uuid:uuid5(uuid:uuid4(), "t00d")))).

-define(PACK(__Msg), jiffy:encode(__Msg)).
-define(UNPACK(__Msg), jiffy:decode(__Msg, [return_maps])).

-define(B2A(Binary), erlang:binary_to_atom(Binary, utf8)).
-define(A2B(Atom), erlang:atom_to_binary(Atom, utf8)).

-define(APPROVED, #{<<"state">> => <<"approved">>}).
-define(REFUSED, #{<<"state">> => <<"refused">>}).
-define(REFUSE(Reason, State), ?REPLY(?REFUSED#{<<"reason">> => Reason}, State)).
-define(REPLY(Reply, State), {reply, {text, ?PACK(Reply)}, State}).
-define(OK(State), {ok, State}).
-define(STOP(State), {stop, State}).

-endif.
