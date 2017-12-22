-ifndef(ERLACK_URL_HRL_).
-define(ERLACK_URL_HRL_, 1).

-pattern(
   { integer,
     "[0-9]+",
     {erlang, binary_to_integer},
     {erlang, integer_to_binary}
   }).

-endif.
