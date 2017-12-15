-module(calendar_handler).

-export([init/2]).

-define(HEADERS, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}).

init(Req0, Opts) ->
    UserId = cowboy_req:binding(user_id, Req0),

    Req =
    case tood_user:get_calendar(UserId) of
        undefined ->
            io:format("NO CALENDAR: ~p~n", [UserId]),
            cowboy_req:reply(404, ?HEADERS, Req0);
        Calendar ->
            io:format("CALENDAR: ~p~n", [UserId]),
            cowboy_req:reply(200, ?HEADERS, eics:encode(Calendar), Req0)
    end,

    {ok, Req, Opts}.
