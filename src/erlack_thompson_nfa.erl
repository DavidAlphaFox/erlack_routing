-module(erlack_thompson_nfa).

-export([from_re/1]).

from_re(RE) ->
    transform(RE).

transform(RE) ->
    Seqs = [append([transform_elem(E) || E <- Seq]) || Seq <- RE],
    States = [N || {N, _} <- Seqs],
    E = 2 + lists:sum(States) - 2 * length(States),
    {Trans, Next} =
        lists:mapfoldl(
          fun({N, Trans}, M) ->
                  %%  1  2        N-1 N
                  %%  +--+- - - - -+--+
                  %%  1 2+M      N-1+M E
                  %%               1  2
                  %%               +--+- - -
                  %%               1 N+M
                  {erlack_nfa:renum(Trans, M, 1, N, E), M+N-2}
          end,
          0,
          Seqs),
    {Next+2, lists:append(Trans)}.

append(List) ->
    {Trans, Next} =
        lists:mapfoldl(
          fun ({N, E}, M) ->
                  {erlack_nfa:renum(E, M, N), N+M-1}
          end,
          0,
          List),
    {Next+1, lists:append(Trans)}.


transform_elem({repeat, 0, infinity, E}) ->
    {N1, E1} = transform_elem(E),
    E2 = erlack_nfa:renum(E1, 0, N1, N1, 1),
    {N1, [{1, e, N1}|E2]};
transform_elem({repeat, X, infinity, E}) ->
    E1 = transform_elem(E),
    {N2, E2} = append(lists:duplicate(X-1, E1)),
    {N3, E3} = E1,
    E4 = erlack_nfa:renum(E3, N2-1, N3),
    {N2+N3-1, lists:append([E2, E4, [{N2+N3-1, e, N2}]])};
transform_elem({repeat, X, X, E}) ->
    append(lists:duplicate(X, transform_elem(E)));
transform_elem({repeat, X, Y, E}) ->
    E1 = transform_elem(E),
    {N2, E2} = append(lists:duplicate(X, E1)),
    {N3, E3} = append(lists:duplicate(Y-X, E1)),
    %%  1        N2
    %%  +- - - - +
    %%           1        N3
    %%           +- - - - +
    %%           N2       N2+N3-1
    E4 = erlack_nfa:renum(E3, N2-1, N3),
    {N2+N3-1, lists:append([E2, E4, [{N2, e, N2+N3-1}]])};
transform_elem({group, E}) ->
    transform(E);
transform_elem({set, Set}) ->
    {2, [{1, Set, 2}]};
transform_elem(C) when is_integer(C) ->
    {2, [{1, [{C,C}], 2}]}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

transform_elem_test_() ->
    [?_assertEqual({2, [{1,[{0,0}],2}]}, transform_elem(0)),
     ?_assertEqual({2, [{1,[{0,3}], 2}]}, transform_elem({set, [{0,3}]}))].

-endif.
