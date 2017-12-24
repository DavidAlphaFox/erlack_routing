-module(erlack_utf8_regex).

-export([encode/1]).


encode(RE) ->
    erlack_regex:simplify([ [ encode_elem(E) || E <- Seq ] || Seq <- RE]).

encode_elem({repeat, X, Y, E}) ->
    {repeat, X, Y, encode_elem(E)};
encode_elem({group, E}) ->
    {group, encode(E)};
encode_elem({set, Set}) ->
    {group, encode_ranges(Set)};
encode_elem(C)
  when 0 =< C, C < 16#80 ->
    C;
encode_elem(C)
  when 16#80 =< C, C < 16#800 ->
    {group,
     [[(C bsr 6) bor 16#C0,
       (C band 16#3F) bor 16#80 ]
     ]};
encode_elem(C)
  when 16#800 =< C, C < 16#10000 ->
    {group,
     [[(C bsr 12) bor 16#E0,
       ((C bsr 6) band 16#3F) bor 16#80,
       (C band 16#3F) bor 16#80]
     ]};
encode_elem(C)
  when 16#10000 =< C, C < 16#110000 ->
    {group,
     [[(C bsr 18) bor 16#F0,
       ((C bsr 12) band 16#3F) bor 16#80,
       ((C bsr 6) band 16#3F) bor 16#80,
       (C band 16#3F) bor 16#80]
     ]}.


encode_ranges(Set) ->
    merge_tail(lists:append([encode_range(X,Y) || {X, Y} <- Set])).

encode_range(X, Y) ->
    lists:append(
      [encode_range(max(X, L), min(Y, H), [], Desc)
       || {L, H, Desc}
              <- [{16#0,    16#7F,    [                                                      {0,16#0, 0}]},
                  {16#80,   16#7FF,   [                                      {6,16#C0,16#3F},{0,16#80,0}]},
                  {16#800,  16#FFFF,  [                    {12,16#E0,16#FFF},{6,16#80,16#3F},{0,16#80,0}]},
                  {16#10000,16#10FFFF,[{18,16#F0,16#3FFFF},{12,16#80,16#FFF},{6,16#80,16#3F},{0,16#80,0}]}
                 ],
          L =< Y, X =< H
      ]).


encode_range(X, X, Head, [{0, 0, 0}]) ->
    [Head ++ [X]];
encode_range(X, Y, _, [{0, 0, 0}])
  when X > Y ->
    [];
encode_range(X, Y, Head, [{0, 0, 0}]) ->
    [Head ++ [{set, [{X, Y}]}]];
encode_range(X, Y, Head, [{Bits, Prefix, Mask}|Rest]) ->
    %% [X,Y], [L,H)
    L = ((X - 1) bsr Bits) + 1,
    H = (Y + 1) bsr Bits,

    if L =< H ->
            if L ==  H - 1 ->
                    [Head ++ [L bor Prefix] ++ full_range(Mask, Rest)];
               L < H ->
                    [Head ++ [{set, [{L bor Prefix, H-1 bor Prefix}]}] ++ full_range(Mask, Rest)];
               true ->
                    []
            end ++
                if X < (L bsl Bits) ->
                        encode_range(X band Mask, Mask, Head ++ [(X bsr Bits) bor Prefix], Rest);
                   true ->
                        []
                end ++
                if Y >= (H bsl Bits) ->
                        encode_range(0, Y band Mask, Head ++ [(Y bsr Bits) bor Prefix], Rest);
                   true ->
                        []
                end;
       true ->
            true = X =< Y,
            encode_range(X band Mask, Y band Mask, Head ++ [(X bsr Bits) bor Prefix], Rest)
    end.

full_range(0, []) ->
    [];
full_range(M, [{Bits, Prefix, Mask}|Rest]) ->
    [{set, [{0 bor Prefix, (M bsr Bits) bor Prefix}]}
     |full_range(Mask, Rest)].


merge_tail(RE) ->
    Tails =
        dict:to_list(
          lists:foldl(
            fun ([Z], D) ->
                    dict:append(Z, [], D);
                ([Y,Z], D) ->
                    dict:append(Z, [Y], D);
                ([X,Y,Z], D) ->
                    dict:append(Z, [X,Y], D);
                ([W,X,Y,Z], D) ->
                    dict:append(Z, [W,X,Y], D)
            end,
            dict:new(),
            RE)),

    [case R of
         [X] ->
             X ++ [T];
         _ ->
             [{group, merge_tail(R)}, T]
     end
     || {T,R} <- Tails].



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_range_test_() ->
    [?_assertEqual([], encode_range(1, 0)),
     ?_assertEqual([[0]], encode_range(0, 0)),
     ?_assertEqual([[{set, [{0,1}]}]], encode_range(0, 1)),
     ?_assertEqual([[{set, [{0,16#7F}]}], [16#C2,{set,[{16#80,16#BF}]}]], encode_range(0, 16#BF)),

     %% | X  Y |
     ?_assertEqual([[16#C2,{set,[{16#81,16#BE}]}]], encode_range(16#81, 16#BE)),

     %% X | Y
     ?_assertEqual(
        [[16#C2, {set, [{16#81, 16#BF}]}],
         [16#C3, {set, [{16#80, 16#82}]}]
        ], encode_range(16#81, 16#C2)),

     %% X | | Y
     ?_assertEqual(
        [[{set, [{16#C3, 16#C5}]}, {set, [{16#80, 16#BF}]}],
         [16#C2, {set, [{16#81, 16#BF}]}],
         [16#C6, {set, [{16#80, 16#82}]}]
        ], encode_range(16#81, 16#182)),

     %%   |X  Y|
     ?_assertEqual(
        [[{set, [{16#C3, 16#C5}]}, {set, [{16#80, 16#BF}]}]],
        encode_range(16#C0, 16#17F)),

     %%   |X  Y
     ?_assertEqual(
        [[16#C3, {set, [{16#80, 16#BE}]}]], encode_range(16#C0, 16#FE)),

     %%   |X  | Y
     ?_assertEqual(
        [[{set, [{16#C3, 16#C5}]}, {set, [{16#80, 16#BF}]}],
         [16#C6, {set, [{16#80, 16#82}]}]
        ], encode_range(16#C0, 16#182)),

     %%   X  Y|
     ?_assertEqual(
        [[16#C3, {set, [{16#82, 16#BF}]}]], encode_range(16#C2, 16#FF)),

     %%   X |  Y|
     ?_assertEqual(
        [[{set, [{16#C3, 16#C5}]}, {set, [{16#80, 16#BF}]}],
         [16#C2, {set, [{16#82, 16#BF}]}]
        ], encode_range(16#82, 16#17F))
    ].

encode_ranges_test_() ->
    [?_assertEqual(
        [[{group,
           [[16#E0, {set, [{16#A0, 16#BF}]}],
            [{set, [{16#E1, 16#EF}]}, {set, [{16#80, 16#BF}]}],
            [{set, [{16#C2, 16#DF}]}]
           ]},
          {set, [{16#80, 16#BF}]}],
         [{set, [{0,16#7F}]}]
        ],
        encode_ranges([{0, 16#FFFF}])),
     ?_assertEqual(
        [[{group,
           [[16#E0, {set, [{16#A0, 16#BF}]}],
            [{group,
          [[16#F4, {set, [{16#80, 16#8F}]}],
           [16#F0, {set, [{16#90, 16#BF}]}],
           [{set, [{16#F1, 16#F3}]}, {set, [{16#80, 16#BF}]}],
           [{set, [{16#E1, 16#EF}]}]
          ]},
             {set, [{16#80, 16#BF}]}],
            [{set, [{16#C2, 16#DF}]}]
           ]},
          {set, [{16#80, 16#BF}]}],
         [{set, [{0,16#7F}]}]
        ],
        encode_ranges([{0, 16#10FFFF}]))].


encode_test_() ->
    [?_assertEqual([[{repeat, 1, 2, 16#7f}]], encode([[{repeat, 1, 2, 16#7f}]])),
     ?_assertEqual([[16#7f]], encode([[{group, [[16#7f]]}]])),
     ?_assertEqual([[16#7f]], encode([[16#7f]])),
     ?_assertEqual([[16#C2, 16#80]], encode([[16#80]])),
     ?_assertEqual([[16#E0, 16#A0, 16#80]], encode([[16#800]])),
     ?_assertEqual([[16#F0, 16#90, 16#80, 16#80]], encode([[16#10000]])),
     ?_assertEqual([], encode([[{set, [{16#7f, 16#00}]}]])),
     ?_assertEqual([[16#7f]], encode([[{set, [{16#7f, 16#7f}]}]])),
     ?_assertEqual([[16#C2, 16#80]], encode([[{set, [{16#80,16#80}]}]])),
     ?_assertEqual([[16#E0, 16#A0, 16#80]], encode([[{set, [{16#800,16#800}]}]])),
     ?_assertEqual([[16#F0, 16#90, 16#80, 16#80]], encode([[{set, [{16#10000,16#10000}]}]]))].

-endif.
