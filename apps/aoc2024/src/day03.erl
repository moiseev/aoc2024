-module(day03).

-export([part_1/0, part_2/0]).

-define(DAY, 3).

part_1() ->
    {ok, Data} = aoclib:get_data(?DAY),
    lists:sum([
        binary_to_integer(A) * binary_to_integer(B)
     || [<<"mul", _Rest/binary>>, A, B] <- parts(Data)
    ]).

part_2() ->
    {ok, Data} = aoclib:get_data(?DAY),
    Pairs = filter(parts(Data)),
    lists:sum([A * B || {A, B} <- Pairs]).

parts(Bin) ->
    {match, Triples} = re:run(
        Bin, <<"do\\(\\)|don't\\(\\)|mul\\(([0-9]{1,3}),([0-9]{1,3})\\)">>, [
            global, {capture, [0, 1, 2], binary}
        ]
    ),
    Triples.

filter(Triples) ->
    {_Enabled, Res} = lists:foldl(
        fun([Op, A, B], {Enabled, Res}) ->
            case Op of
                <<"do()">> ->
                    {true, Res};
                <<"don't()">> ->
                    {false, Res};
                _Must_be_mul ->
                    case Enabled of
                        true ->
                            {Enabled, [
                                {binary_to_integer(A), binary_to_integer(B)}
                                | Res
                            ]};
                        false ->
                            {Enabled, Res}
                    end
            end
        end,
        {true, []},
        Triples
    ),
    lists:reverse(Res).
