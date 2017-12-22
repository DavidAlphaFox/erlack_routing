-module(erlack_bitset).

-export(
   [ from_range_list/1,
     to_range_list/1,
     union/2,
     intersection/2,
     subtract/2 ]).


from_range_list(L) ->
    lists:foldl(
      fun (X, A) ->
              A bor (1 bsl X)
      end,
      0,
      [I || {X,Y} <- L,
            I <- lists:seq(X,Y)]).


to_range_list(X) ->
    erlack_range_list:from_list(to_range_list(X, 0)).

to_range_list(0, _) ->
    [];
to_range_list(X, N) ->
    case X band 1 of
        0 ->
            to_range_list(X bsr 1, N+1);
        1 ->
            [{N, N}|to_range_list(X bsr 1, N+1)]
    end.


union(X, Y) ->
    X bor Y.

intersection(X, Y) ->
    X band Y.

subtract(X, Y) ->
    X bxor (X band Y).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

from_range_list_test() ->
    ?assertEqual(2#10110, from_range_list([{1,2},{4,4}])).

to_range_list_test() ->
    ?assertEqual([{1,2},{4,4}], to_range_list(2#10110)).

union_test() ->
    ?assertEqual(2#11, union(2#01, 2#10)).

intersection_test() ->
    ?assertEqual(2#010, intersection(2#011, 2#110)).

subtract_test() ->
    ?assertEqual(2#001, subtract(2#011, 2#110)).

-endif.
