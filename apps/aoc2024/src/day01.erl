-module(day01).

-export([part_1/0, part_2/0]).

part_1() ->
    {ok, Data} = dagedda:get(2024, 1, #{cache_dir => code:priv_dir(aoc2024)}),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    Pairs = [
        begin
            BinPair = binary:split(L, <<" ">>, [global, trim_all]),
            list_to_tuple([binary_to_integer(B) || B <- BinPair])
        end
     || L <- Lines
    ],
    {L1, L2} = lists:unzip(Pairs),
    Sorted = lists:zip(lists:sort(L1), lists:sort(L2)),
    Distances = [erlang:abs(A - B) || {A, B} <- Sorted],
    lists:sum(Distances).

part_2() ->
    {ok, Data} = dagedda:get(2024, 1, #{cache_dir => code:priv_dir(aoc2024)}),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    Pairs = [
        begin
            BinPair = binary:split(L, <<" ">>, [global, trim_all]),
            list_to_tuple([binary_to_integer(B) || B <- BinPair])
        end
     || L <- Lines
    ],
    {L1, L2} = lists:unzip(Pairs),
    Hist = hist(L2),

    Scores = [N * maps:get(N, Hist, 0) || N <- L1],
    lists:sum(Scores).

hist(Ns) ->
    lists:foldl(
        fun(N, Acc) ->
            maps:update_with(N, fun(V) -> V + 1 end, 1, Acc)
        end,
        #{},
        Ns
    ).
