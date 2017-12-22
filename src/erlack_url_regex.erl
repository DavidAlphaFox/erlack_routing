-module(erlack_url_regex).

-export([encode/1]).

encode(RE) ->
    erlack_regex:simplify([ [encode_elem(E) || E <- Seq] || Seq <- RE]).

encode_elem({repeat, X, Y, E}) ->
    {repeat, X, Y, encode_elem(E)};
encode_elem({group, E}) ->
    {group, encode(E)};
encode_elem({set, Set}) ->
    {group, encode_ranges(Set)};
encode_elem($/) ->
    $/;
encode_elem(C)
  when $a =< C, C =< $z;
       $A =< C, C =< $Z;
       $0 =< C, C =< $9;
       $& =< C, C < $/;
       C =:= $$; C =:= $!; C =:= $_ ; C =:= $~ ;
       C =:= $=; C =:= $:; C =:= $; ; C =:= $@ ->
    {group, [[C]|encode_char(C)]};
encode_elem(C)
  when is_integer(C) ->
    {group, encode_char(C)}.

encode_char(C) ->
    [[$%,H,L]
     || H <- hex_chars(C bsr 4),
        L <- hex_chars(C band 15)].

hex_chars(X) when 0 =< X, X =< 9 ->
    [$0 + X];
hex_chars(X) when 10 =< X, X =< 15 ->
    [$A + X - 10, $a + X - 10].


encode_ranges(Set) ->
    lists:append(
      [[[C] || C <- safe_chars(), X =< C, C =< Y] ++
           if X =< $/, $/ =< Y ->
                   if X < $/ ->
                           encode_range(X, $/-1);
                      true ->
                           []
                   end ++
                       if $/ < Y ->
                               encode_range($/+1, Y);
                          true ->
                               []
                       end;
             true ->
                   encode_range(X,Y)
           end
       || {X,Y} <- Set]).


safe_chars() ->
    lists:seq($a, $z)
        ++ lists:seq($A, $Z)
        ++ lists:seq($0, $9)
        ++ lists:seq($&, $/)
        ++ [$$, $!, $_, $~, $=, $:, $;, $@].


encode_range(X, Y) ->
    L = ((X - 1) bsr 4) + 1,
    H = (Y + 1) bsr 4,

    if L =< H ->
            if L < H ->
                    [[$%,
                      half_byte_group(L, H-1),
                      half_byte_group()]];
               true ->
                    []
            end ++
                if X < (L bsl 4) ->
                        [[$%,
                          half_byte_group(X bsr 4),
                          half_byte_group(X band 15, 15)]];
                   true ->
                        []
                end ++
                if Y >= (H bsl 4) ->
                        [[$%,
                          half_byte_group(Y bsr 4),
                          half_byte_group(0, Y band 15)]];
                   true ->
                        []
                end;
       true ->
            if X =< Y ->
                    [[$%,
                      half_byte_group(X bsr 4),
                      half_byte_group(X band 15, Y band 15)]];
               true ->
                    []
            end
    end.

half_byte_group(X, Y) ->
    { group,
      [ [C]
        || I <- lists:seq(X, Y),
           C <- hex_chars(I)
      ]}.

half_byte_group(X) ->
    half_byte_group(X, X).

half_byte_group() ->
    half_byte_group(0, 15).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

half_byte_groups(Groups) ->
    [[$%|[half_byte_group(L,H) || {L,H} <- G]]
     || G <- Groups].

half_byte_group_test_() ->
    [?_assertEqual({group, []}, half_byte_group(1, 0)),
     ?_assertEqual({group, [[$1]]}, half_byte_group(1, 1)),
     ?_assertEqual({group, [[$1],[$2],[$3]]}, half_byte_group(1, 3)),
     ?_assertEqual({group, [[$A],[$a],[$B],[$b]]}, half_byte_group(16#A, 16#B))].

encode_range_test_() ->
    Test =
        fun(X, Y, Groups) ->
                ?_assertEqual(half_byte_groups(Groups), encode_range(X,Y))
        end,

    [?_assertEqual([], encode_range(1,0)),
     Test(0, 0, [[{0,0},{0,0}]]),
     %% | X Y |
     Test(16#11, 16#1E, [[{1,1},{1,16#E}]]),
     %% X | Y
     Test(
       16#11, 16#2E,
       [[{1,1},{1,16#F}],
        [{2,2},{0,16#E}]]),
     %% X | | Y
     Test(
       16#11, 16#4E,
       [[{2,3},{0,16#F}],
        [{1,1},{1,16#F}],
        [{4,4},{0,16#E}]]),
     %% |X  Y| 
     Test(16#10, 16#4F, [[{1,4},{0,16#F}]]),
     %% |X  Y
     Test(16#10, 16#1E, [[{1,1},{0,16#E}]]),
     %% |X  | Y
     Test(
       16#10, 16#2E,
       [[{1,1},{0,16#F}],
        [{2,2},{0,16#E}]]),
    %% X  Y|
    Test(16#11, 16#1F, [[{1,1},{1,16#F}]]),
    %% X | Y|
    Test(16#11, 16#2F,
         [[{2,2},{0,16#F}],
          [{1,1},{1,16#F}]])
    ].

encode_test_() ->
    [?_assertEqual([[$/]], encode([[$/]])),
     ?_assertEqual([[$a], [$%, $6, $1]], encode([[$a]])),
     ?_assertEqual([[$%, $8, $0]], encode([[16#80]])),
     ?_assertEqual([[$/]], encode([[{set, [{$/, $/}]}]])),
     ?_assertEqual([[$a], [$%, $6, $1]], encode([[{set, [{$a, $a}]}]])),
     ?_assertEqual([[$%, $8, $0]], encode([[{set, [{16#80, 16#80}]}]]))].

-endif.
