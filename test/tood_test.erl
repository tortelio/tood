-module(tood_test).

-export([sign_up/3,
         log_in/3,
         get_items/1]).

-export([decode/1]).

sign_up(Ref, Username, Password) ->
    <<"ok">> = wsc:send_for_reply(Ref, #{<<"type">> => <<"sign-up">>,
                                         <<"username">> => Username,
                                         <<"password">> => Password}),
    ok.

log_in(Ref, Username, Password) ->
    #{<<"state">> := <<"approved">>,
      <<"user_id">> := UserId} = wsc:send_for_reply(Ref, #{<<"type">> => <<"log-in">>,
                                                            <<"username">> => Username,
                                                            <<"password">> => Password}),
    {ok, UserId}.

get_items(Ref) ->
    Items = wsc:send_for_reply(Ref, #{<<"type">> => <<"get-items">>}),
    {ok, Items}.

decode(Data) -> jiffy:decode(Data, [return_maps]).
