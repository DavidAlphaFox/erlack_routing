-module(erlack_url_rule).

-compile({parse_transform, erlack_pgen}).

-export([parse/1]).

parse(S) ->
    case rule(S) of
        {ok, Rule, []} ->
            {ok, normalize(Rule)};
        _ ->
            error
    end.

normalize([]) ->
    [];
normalize([{_,_}=H|T]) ->
    [H|normalize(T)];
normalize([H|T]) ->
    case normalize(T) of
        [L|Rest] when is_list(L) ->
            [[H|L]|Rest];
        List ->
            [[H]|List]
    end.

-rule(rule/1).

rule([]) ->
    {ok, [], []};
rule(S) ->
    {ok, H, S1} = seg(S),
    {ok, T, S2} = rule(S1),
    {ok, [H|T], S2}.

-rule(seg/1).

seg([$\\, H|S]) when H =:= ${; H =:= $}; H =:= $\\ ->
    {ok, H, S};
seg([${|S]) ->
    {ok, K, [$:|S1]} = name(S),
    {ok, V, [$}|S2]} = name(S1),
    {ok, {K,V}, S2};
seg([H|S]) when H =/= ${, H =/= $}, H =/= $\\ ->
    {ok, H, S}.

-rule(name/1).

name([H|S])
  when $a =< H, H =< $z ->
    {ok, T, S1} = name_tail(S),
    {ok, list_to_atom([H|T]), S1}.

-rule(name_tail/1).

name_tail([H|S])
  when $a =< H, H =< $z; $0 =< H, H =< $9; H =:= $_ ->
    {ok, T, S1} = name_tail(S),
    {ok, [H|T], S1};
name_tail(S) ->
    {ok, [], S}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    [?_assertEqual({ok, []}, parse("")),
     ?_assertEqual({ok, ["/index.html"]}, parse("/index.html")),
     ?_assertEqual({ok, ["/posts/", {id,integer}, "/"]}, parse("/posts/{id:integer}/")),
     ?_assertEqual({ok,["\\"]}, parse("\\\\")),
     ?_assertEqual(error, parse("{"))].

-endif.
