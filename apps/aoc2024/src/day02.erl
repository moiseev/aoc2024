-module(day02).

-export([part_1/0, part_2/0]).

-define(DAY, 2).

part_1() ->
    {ok, Data} = aoclib:get_data(?DAY),
    Lines = aoclib:lines(Data),
    lists:foldl(
        fun(Line, Acc) ->
            Ns = to_numbers(Line),
            case is_safe(Ns) of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        Lines
    ).

part_2() ->
    {ok, Data} = aoclib:get_data(?DAY),
    Lines = aoclib:lines(Data),
    lists:foldl(
        fun(Line, Acc) ->
            Ns = to_numbers(Line),
            case lists:any(fun is_safe/1, candidates(Ns)) of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        Lines
    ).

to_numbers(Bin) ->
    [binary_to_integer(B) || B <- aoclib:words(Bin)].

is_safe(Ns) ->
    [H | T] = Ns,
    {_, Deltas} = lists:foldl(
        fun(N, {Prev, Ds}) ->
            {N, [N - Prev | Ds]}
        end,
        {H, []},
        T
    ),
    lists:all(fun(N) -> (N > 0) andalso (N < 4) end, Deltas) orelse
        lists:all(fun(N) -> (N < 0) andalso (N > -4) end, Deltas).

candidates(L) ->
    Skips = [without_nth(N, L) || N <- lists:seq(1, length(L))],
    [L | Skips].

without_nth(N, L) ->
    lists:sublist(L, N - 1) ++ lists:nthtail(N, L).
