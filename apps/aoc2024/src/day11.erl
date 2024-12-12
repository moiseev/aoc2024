-module(day11).

-export([part_1/0, part_2/0]).
-export([step/2, steps/2]).

-define(DAY, 11).

part_1() ->
    {ok, Data} = aoclib:get_data(?DAY),
    %Data = <<"125 17\n">>,
    [Line] = aoclib:lines(Data),
    Ns = aoclib:numbers(Line),
    length(steps(Ns, 25)).

part_2() ->
    ok.

steps(Xs, 0) ->
    Xs;
steps(Xs, N) ->
    steps(step(Xs, []), N - 1).

step([], Acc) ->
    lists:reverse(Acc);
step([H | T], Acc) ->
    Acc1 =
        case H of
            0 ->
                [1 | Acc];
            N ->
                case floor(math:log10(N) + 1) of
                    Size when Size rem 2 == 0 ->
                        {Left, Right} = split(N, Size),
                        [Right, Left | Acc];
                    _Other ->
                        [N * 2024 | Acc]
                end
        end,
    step(T, Acc1).

split(Bin, Size) when is_binary(Bin) ->
    Half = Size div 2,
    Left = binary:part(Bin, 0, Half),
    Right = binary:part(Bin, Half, Half),
    {Left, Right};
split(N, Size) when is_number(N) ->
    Bin = integer_to_binary(N),
    {Left, Right} = split(Bin, Size),
    {binary_to_integer(Left), binary_to_integer(Right)}.
