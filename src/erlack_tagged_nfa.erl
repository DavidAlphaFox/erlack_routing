-module(erlack_tagged_nfa).

-export([to_tagged_dfa/3]).

-import(erlack_nfa_utils, [traverse/4, dict_fetch/3]).


to_tagged_dfa(NFA, TaggedTrans, EndStates) ->
    NFA1 =
        dict:map(
          fun(_, V) ->
                  merge_moves([{A, ordsets:from_list([To])} || {A, To} <- V])
          end,
          erlack_nfa:to_dict(NFA)),

    DFA =
      traverse(
        [ordsets:from_list([1])],
        dict:new(),
        fun dict:is_key/2,
        fun (States, Result) ->
                Moves =
                    erlack_group_map:from_list(
                      [ {A, ordsets:from_list([{To, {From, Tags}}])}
                        || {S, Tags, From} <- tagged_closure(States, TaggedTrans),
                           {A, L} <- dict_fetch(S, NFA1, []),
                           To <- L]),

                Moves1 =
                    erlack_group_map:compact(
                      [{A, normalize_tagged_states(To)} || {A, To} <- Moves]),

                {[[S || {S,_} <- To] || {_,To} <- Moves1], dict:store(States, Moves1, Result)}
        end),

    {DFANumberList, _} =
        lists:mapfoldl(
          fun ([1], N) ->
                  {{[1], 1}, N};
              (S, N) ->
                  {{S, N}, N+1}
          end,
          2,
          dict:fetch_keys(DFA)),

    DFANumbers = dict:from_list(DFANumberList),

    { [{N, length(L)} || {L,N} <- DFANumberList],
      [ {N,hd(L)}
        || {N,L} <-
               [ {N,
                  [{index_of(From, L), Tags, S}
                   || {S, Tags, From} <- tagged_closure(L, TaggedTrans),
                      lists:member(S, EndStates) ]
                 }
                 || {L, N} <- DFANumberList],
           case L of
               [_] ->
                   true;
               [] ->
                   false
           end
      ],

      [{dict:fetch(From, DFANumbers),
        [ {A,
           {dict:fetch([S || {S,_} <- To], DFANumbers),
            [{index_of(F,From), Tags} || {_, {F,Tags}} <- To]}
          }
          || {A, To} <- M]}
       || {From, M} <- dict:to_list(DFA)]}.


index_of(X, L) ->
    index_of(X, 1, L).

index_of(X, N, [X|_]) ->
    N;
index_of(X, N, [_|T]) ->
    index_of(X, N+1, T).


tagged_closure(States, TaggedTrans) ->
    sets:to_list(
      traverse(
        [{S, ordsets:new(), S} || S <- States],
        sets:new(),
        fun sets:is_element/2,
        fun ({State, Tags, From}, Result) ->
                {[ {To, ordsets:add_element(Tag, Tags), From}
                   || {Tag, To} <- dict_fetch(State, TaggedTrans, [])],
                 sets:add_element({State, Tags, From}, Result)
                }
        end)).


normalize_tagged_states(List) ->
    lists:keysort(
      1,
      dict:to_list(
        dict:map(
          fun (_, [H|T]) ->
                  lists:foldl(
                    fun(X = {_, T1}, Acc = {_, T2}) ->
                            case {ordsets:is_subset(T1,T2), ordsets:is_subset(T2,T1)} of
                                {true, false} ->
                                    Acc;
                                {false, true} ->
                                    X;
                                _ ->
                                    throw(inconsistent)
                            end
                    end,
                    H,
                    T)
          end,
          lists:foldl(
            fun({To, {From, Tags}}, D) ->
                    dict:append(To, {From, Tags}, D)
            end,
            dict:new(),
            List)))).


merge_moves(List) ->
    erlack_group_map:compact(
      erlack_group_map:from_list(List)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

tagged_closure_test_() ->
    [?_assertEqual(
       [{2,[1],1},{4,[2,3],1},{3,[2],1},{1,[],1}],
        tagged_closure([1], dict:from_list([{1, [{1, 2}, {2, 3}]}, {3, [{3, 4}]}])))].

to_tagged_dfa_test_() ->
    [?_assertEqual(
        {[{2,2},{1,1}],
         [{2,{2,[],3}}],
         [{2,[{[{$a,$a}],{2,[{1,[]},{1,[1]}]}}]},
          {1,[{[{$a,$a}],{2,[{1,[]},{1,[1]}]}}]}]},
        to_tagged_dfa(
          [{1, [{$a, $a}], 1},
           {2, [{$a, $a}], 3},
           {3, [{$a, $a}], 3}
          ],
          dict:from_list([{1, [{1, 2}]}]),
          [3])),

     ?_assertEqual(
        {[{2,1},{1,1}],
         [{2,{1,[],5}}],
         [{2,[]},{1,[{[{$a,$a}],{2,[{1,[1,2]}]}}]}]},
        to_tagged_dfa(
          [{2, [{$a, $a}], 5},
           {4, [{$a, $a}], 5}],
          dict:from_list([{1,[{1,2},{1,3}]}, {3, [{2,4}]}]),
          [5])),

     ?_assertThrow(
        inconsistent,
        to_tagged_dfa(
          [{2, [{$a, $a}], 4},
           {3, [{$a, $a}], 4}],
          dict:from_list([{1,[{1,2},{2,3}]}]),
          [4]))
].

-endif.
