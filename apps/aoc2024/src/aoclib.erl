-module(aoclib).

-compile(export_all).

-define(YEAR, 2024).

get_data(Day) ->
    dagedda:get(?YEAR, Day, #{cache_dir => code:priv_dir(aoc2024)}).

lines(Bin) ->
    binary:split(Bin, <<"\n">>, [global, trim_all]).

words(Bin) ->
    binary:split(Bin, <<" ">>, [global, trim_all]).

numbers(Bin) ->
    [binary_to_integer(B) || B <- words(Bin)].
