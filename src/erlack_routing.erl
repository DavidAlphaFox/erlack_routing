-module(erlack_routing).

-export([middleware/3]).

middleware(Handler, Module, NotFound) ->
    case Module:url_dispatch(get(<<"REQUEST_URI">>)) of
        error ->
            ecgi:apply_handler(NotFound);
        {Endpoint, Args, Query} ->
            put(erlack_endpoint, Endpoint),
            put(erlack_args, Args),
            put(erlack_query, Query),
            ecgi:apply_handler(Handler)
    end.
