-module(peg_tests).

-compile({parse_transform, erlack_pgen}).

-rule(digit/1).

digit([H|T])
  when H >= $0, H =< $9 ->
    {ok, H - $0, T}.

int(S) -> int(0,S).

-rule(int/2).
int(Acc, S) ->
    {ok, N, S1} = digit(S),
    int(Acc*10+N, S1);
int(Acc, S) ->
    {ok, N, S1} = digit(S),
    {ok, Acc*10+N, S1}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

int_test_() ->
    [?_assertEqual({ok, 1, ""}, int("1")),
     ?_assertEqual({ok, 12, ""}, int("12")),
     ?_assertEqual({ok, 12, "a"}, int("12a")),
     ?_assertEqual(error, int("a")),
     ?_assertEqual(error, int(""))].

-endif.
