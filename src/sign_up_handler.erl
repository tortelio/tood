-module(sign_up_handler).
-include("tood.hrl").

-export([init/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2]).

-export([to_json/2]).

init(Req0, Opts) ->
    {cowboy_rest, Req0, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),

    #{<<"username">> := Username,
      <<"password">> := Password} = ?UNPACK(Body),

    Response =
    case tood_user:sign_up(Username, Password) of
        ok ->
            ?APPROVED;
        {error, Reason} ->
            ?REFUSED#{<<"reason">> => Reason}
    end,

    {true, cowboy_req:reply(200, #{}, ?PACK(Response), Req1), State}.
