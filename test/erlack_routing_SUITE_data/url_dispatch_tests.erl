-module(url_dispatch_tests).

-compile({parse_transform, erlack_url_dispatch}).

-export([url_dispatch/1]).

-pattern(
   { integer,
     "[0-9]+",
     {erlang, binary_to_integer},
     {erlang, integer_to_binary}
   }).

-dispatch({root, "posts/", {dispatch, post}}).
-dispatch({post, "", {endpoint, index}}).
-dispatch({post, "{id:integer}", {endpoint, post}}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

routing_middleware_test_() ->
    Handler =
        fun () ->
                {get(erlack_endpoint),
                 get(erlack_args),
                 get(erlack_query)}
        end,
    NotFound = fun() -> not_found end,

    Test =
        fun(URI) ->
                put(<<"REQUEST_URI">>, URI),
                erlack_routing:middleware(Handler, ?MODULE, NotFound)
        end,

    [?_assertEqual(not_found, Test(<<"/posts">>)),
     ?_assertEqual({post,#{id => 10}, <<>>}, Test(<<"/posts/10">>))].
    

url_reverse_test_() ->
    [?_assertEqual([<<"/">>,<<"posts/">>], url_reverse(index, #{})),
     ?_assertEqual([<<"/">>,<<"posts/">>,<<"10">>], url_reverse(post, #{id => 10}))].

url_dispatch_test_() ->
    [?_assertEqual({index,#{},<<>>}, url_dispatch(<<"/posts/">>)),
     ?_assertEqual({index,#{},<<"a">>}, url_dispatch(<<"/posts/?a">>)),
     ?_assertEqual({post,#{id => 10},<<>>}, url_dispatch(<<"/posts/10">>)),
     ?_assertEqual({post,#{id => 10},<<>>}, url_dispatch(<<"/posts/1%30">>)),
     ?_assertEqual(error, url_dispatch(<<"/posts/10/">>)),
     ?_assertEqual(error, url_dispatch(<<"/posts/1%3">>))].

-endif.
