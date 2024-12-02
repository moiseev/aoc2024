-module(aoclib).

-compile(export_all).

lines(Bin) ->
    binary:split(Bin, <<"\n">>, [global, trim_all]).

words(Bin) ->
    binary:split(Bin, <<" ">>, [global, trim_all]).

numbers(Bin) ->
    [binary_to_integer(B) || B <- words(Bin)].
