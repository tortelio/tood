-module(ws_handler).
-include("tood.hrl").

-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-record state, {token   :: uuid:uuid() | undefined,
                user_id :: uuid:uuid() | undefined}.

init(Req, []) ->
    {cowboy_websocket, Req, #state{token = cowboy_req:binding(token, Req)}, ?WS_OPTS}.

websocket_init(#state{token = Token} = State) ->
    case ets:lookup(active_users, Token) of
        [{Token, UserId}] ->
            io:format("FOUND ACTIVE USER: TOKEN: ~p USER_ID: ~p~n", [Token, UserId]),
            ?OK(State#state{user_id = UserId});
        [] ->
            io:format("NOT FOUND ACTIVE USER~n", []),
            ?STOP(State)
    end.

websocket_handle({_, Binary}, State) ->
    try ?UNPACK(Binary) of
        Message ->
            handle_message(Message, State)
    catch
        Class:Reason ->
            io:format("EXCEPTION RASIED: ~p REASON: ~p~n", [Class, Reason]),
            ?STOP(State)
    end.

websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _PartialReq, State) ->
    io:format("TERMINATE: ~p STATE: ~p~n", [Reason, State]),
    ok.

% Get items
handle_message(#{<<"type">> := <<"get-items">>}, #state{user_id = UserId} = State) ->
    io:format("GET ITEMS: USER_ID: ~p~n", [UserId]),

    Items = [begin
                 Item2 = tood_item:todo_to_map(Item),
                 Item2#tood_item.todo
             end || Item <- tood_item:find_by_user_id(UserId)],

    ?REPLY(#{<<"type">> => <<"get-items">>, <<"items">> => Items}, State);

% Create item
handle_message(#{<<"type">> := <<"create-item">>, <<"item">> := Todo0},
               #state{user_id = Username} = State) when is_map(Todo0) ->
    io:format("CREATE TODO: ~p~n", [Todo0]),

    Todo = tood_item:prepare(Todo0),
    ok = tood_item:create(Username, Todo),

    ?OK(State);

% Update item
handle_message(#{<<"type">> := <<"update-item">>, <<"item">> := Todo0}, State) when is_map(Todo0) ->
    io:format("UPDATE TODO: ~p~n", [Todo0]),

    Todo = tood_item:prepare(Todo0),
    ok = tood_item:update(Todo),

    ?OK(State);

% Delete item
handle_message(#{<<"type">> := <<"delete-item">>, <<"item">> := Todo0}, State) when is_map(Todo0) ->
    io:format("DELETE TODO: ~p~n", [Todo0]),

    Todo = tood_item:prepare(Todo0),
    case tood_item:delete(Todo) of
        ok ->
            ?OK(State);
        {error, Reason} ->
            ?REFUSE(Reason, State)
    end;

% Unhandled messages
handle_message(Msg, State) ->
    io:format("UNKNOWN MESSAGE: ~p STATE: ~p~n", [Msg, State]),
    ?STOP(State).
