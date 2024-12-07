-module(aoclib).

-compile(export_all).

-define(YEAR, 2024).

get_data(Day) ->
    dagedda:get(?YEAR, Day, #{cache_dir => code:priv_dir(aoc2024)}).

lines(Bin) ->
    split_by(Bin, <<"\n">>).

words(Bin) ->
    split_by(Bin, <<" ">>).

split_by(Bin, Sep) ->
    binary:split(Bin, Sep, [global, trim_all]).

numbers(Bin) ->
    [binary_to_integer(B) || B <- words(Bin)].

grid(Bin) ->
    Lines = lines(Bin),
    Height = erlang:length(Lines),
    [H | _] = Lines,
    Width = erlang:byte_size(H),
    Grid = maps:from_list(
        [
            {{X, Y}, Ch}
         || {X, L} <- lists:enumerate(Lines),
            {Y, Ch} <- lists:enumerate(binary_to_list(L))
        ]
    ),
    {Grid, {Height, Width}}.

neighbors({X, Y}, {MaxX, MaxY}) ->
    [
        {X1, Y1}
     || DX <- [-1, 0, 1],
        DY <- [-1, 0, 1],
        X1 <- [X + DX],
        Y1 <- [Y + DY],
        X1 > 0,
        X1 =< MaxX,
        Y1 > 0,
        Y1 =< MaxY,
        {X1, Y1} /= {X, Y}
    ].

% shoot a ray from XY using direction DX,DY of length Len within grid
% 0,0xMaxX,MaxY
ray(_Start, {0, 0}, _Bounds, _Len, _Acc) ->
    [];
ray(_Start, _Direction, _Bounds, _Len = 0, Acc) ->
    lists:reverse(Acc);
ray({X, Y}, _Direction, {MaxX, MaxY}, _LenIsNotZero, _Acc) when
    X < 1 orelse X > MaxX orelse Y < 1 orelse Y > MaxY
->
    [];
ray({X, Y}, Direction = {DX, DY}, Bounds, Len, Acc) ->
    ray({X + DX, Y + DY}, Direction, Bounds, Len - 1, [{X, Y} | Acc]).

rays(Start, Bounds, Len) ->
    [
        Ray
     || DX <- [-1, 0, 1],
        DY <- [-1, 0, 1],
        Ray <- [ray(Start, {DX, DY}, Bounds, Len, [])],
        Ray /= []
    ].

rev_index(Grid) when is_map(Grid) ->
    KeyFun = fun({_K, V}) -> V end,
    ValueFun = fun({K, _V}) -> K end,
    maps:groups_from_list(KeyFun, ValueFun, maps:to_list(Grid)).

combinations(_, 0) ->
    [[]];
combinations(Xs, Len) ->
    [[H | T] || H <- Xs, T <- combinations(Xs, Len - 1)].
