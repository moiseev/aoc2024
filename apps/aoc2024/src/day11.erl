-module(day11).

-export([part_1/0, part_2/0]).

-define(DAY, 11).

part_1() ->
    {ok, Data} = aoclib:get_data(?DAY),
    %Data = <<"125 17\n">>,
    [Line] = aoclib:lines(Data),
    Ns = aoclib:numbers(Line),
    solve(Ns, 25).

part_2() ->
    {ok, Data} = aoclib:get_data(?DAY),
    %Data = <<"125 17\n">>,
    [Line] = aoclib:lines(Data),
    Ns = aoclib:numbers(Line),
    solve(Ns, 75).

blink(N) ->
    case N of
        0 ->
            [1];
        _ ->
            case floor(math:log10(N) + 1) of
                Size when Size rem 2 == 0 ->
                    {Left, Right} = split(N, Size),
                    [Left, Right];
                _ ->
                    [N * 2024]
            end
    end.

all_steps_of_one(_, 0) ->
    1;
all_steps_of_one(N, Depth) ->
    case ets:lookup(cache, {N, Depth}) of
        [] ->
            Res = lists:sum([all_steps_of_one(N1, Depth - 1) || N1 <- blink(N)]),
            ets:insert(cache, {{N, Depth}, Res}),
            Res;
        [{_Key, Val}] ->
            Val
    end.

solve(Ns, Depth) ->
    _T = ets:new(cache, [named_table]),
    Res = lists:sum([all_steps_of_one(N, Depth) || N <- Ns]),
    ets:delete(cache),
    Res.

split(Bin, Size) when is_binary(Bin) ->
    Half = Size div 2,
    Left = binary:part(Bin, 0, Half),
    Right = binary:part(Bin, Half, Half),
    {Left, Right};
split(N, Size) when is_number(N) ->
    Bin = integer_to_binary(N),
    {Left, Right} = split(Bin, Size),
    {binary_to_integer(Left), binary_to_integer(Right)}.
