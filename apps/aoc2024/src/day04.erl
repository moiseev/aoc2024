-module(day04).

-export([part_1/0, part_2/0]).

-define(DAY, 4).

part_1() ->
    {ok, Data} = aoclib:get_data(?DAY),
    %Data =
    %<<
    %"MMMSXXMASM\n"
    %"MSAMXMSMSA\n"
    %"AMXSXMAAMM\n"
    %"MSAMASMSMX\n"
    %"XMASAMXAMM\n"
    %"XXAMMXXAMA\n"
    %"SMSMSASXSS\n"
    %"SAXAMASAAA\n"
    %"MAMMMXMMMM\n"
    %"MXMXAXMASX"
    %>>,
    {Grid, Dimensions} = aoclib:grid(Data),
    RevIndex = aoclib:rev_index(Grid),
    AllXMAS = [
        Ray
     || XLoc <- maps:get($X, RevIndex, undefined),
        Ray <- aoclib:rays(XLoc, Dimensions, 4),
        ray_to_word(Grid, Ray) == "XMAS"
    ],
    length(lists:uniq(AllXMAS)).

part_2() ->
    {ok, Data} = aoclib:get_data(?DAY),
    %Data =
    %<<
    %"MMMSXXMASM\n"
    %"MSAMXMSMSA\n"
    %"AMXSXMAAMM\n"
    %"MSAMASMSMX\n"
    %"XMASAMXAMM\n"
    %"XXAMMXXAMA\n"
    %"SMSMSASXSS\n"
    %"SAXAMASAAA\n"
    %"MAMMMXMMMM\n"
    %"MXMXAXMASX"
    %>>,
    {Grid, Dimensions} = aoclib:grid(Data),
    RevIndex = aoclib:rev_index(Grid),
    length([
        ALoc
     || ALoc <- maps:get($A, RevIndex, undefined),
        XRays <- [x_at(ALoc, Dimensions)],
        XRays /= [],
        lists:all(fun(Ray) -> is_mas(Grid, Ray) end, XRays)
    ]).

ray_to_word(Grid, Ray) ->
    [maps:get(Loc, Grid, undefined) || Loc <- Ray].

x_at({X, Y}, {MaxX, MaxY}) when
    X == 1 orelse X == MaxX orelse Y == 1 orelse Y == MaxY
->
    [];
x_at({X, Y}, _Bounds) ->
    [
        [{X - 1, Y - 1}, {X, Y}, {X + 1, Y + 1}],
        [{X - 1, Y + 1}, {X, Y}, {X + 1, Y - 1}]
    ].

is_mas("SAM") -> true;
is_mas("MAS") -> true;
is_mas(_) -> false.

is_mas(Grid, Ray) ->
    is_mas(ray_to_word(Grid, Ray)).
