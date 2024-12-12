-module(day07).

-export([part_1/0, part_2/0]).
-export([eval/2]).

-define(DAY, 7).

part_1() ->
    {ok, Data} = aoclib:get_data(?DAY),
    %Data = <<
    %"190: 10 19\n"
    %"3267: 81 40 27\n"
    %"83: 17 5\n"
    %"156: 15 6\n"
    %"7290: 6 8 6 15\n"
    %"161011: 16 10 13\n"
    %"192: 17 8 14\n"
    %"21037: 9 7 18 13\n"
    %"292: 11 6 16 20\n"
    %>>,
    Lines = aoclib:lines(Data),
    ValidTotals = [
        begin
            [TotalS, NsS] = aoclib:split_by(Line, <<":">>),
            Total = binary_to_integer(TotalS),
            Ns = aoclib:numbers(NsS),
            CanBeTrue = lists:member(Total, all_evals(Ns, [add, mul])),
            case CanBeTrue of
                true -> Total;
                false -> 0
            end
        end
     || Line <- Lines
    ],
    lists:sum(ValidTotals).

part_2() ->
    {ok, Data} = aoclib:get_data(?DAY),
    %Data = <<
    %"190: 10 19\n"
    %"3267: 81 40 27\n"
    %"83: 17 5\n"
    %"156: 15 6\n"
    %"7290: 6 8 6 15\n"
    %"161011: 16 10 13\n"
    %"192: 17 8 14\n"
    %"21037: 9 7 18 13\n"
    %"292: 11 6 16 20\n"
    %>>,
    Lines = aoclib:lines(Data),
    ValidTotals = [
        begin
            [TotalS, NsS] = aoclib:split_by(Line, <<":">>),
            Total = binary_to_integer(TotalS),
            Ns = aoclib:numbers(NsS),
            CanBeTrue = lists:member(Total, all_evals(Ns, [add, mul, concat])),
            case CanBeTrue of
                true -> Total;
                false -> 0
            end
        end
     || Line <- Lines
    ],
    lists:sum(ValidTotals).

all_evals(Ns, OpsSet) ->
    NOps = length(Ns) - 1,
    AllOps = aoclib:combinations(OpsSet, NOps),
    [eval(Ns, Ops) || Ops <- AllOps].

eval([N0 | Ns], Ops) ->
    eval(Ns, Ops, N0).

eval([], [], Acc) ->
    Acc;
eval([N | Ns], [Op | Ops], Acc) ->
    Acc1 =
        case Op of
            add ->
                Acc + N;
            mul ->
                Acc * N;
            concat ->
                binary_to_integer(
                    iolist_to_binary(io_lib:format("~w~w", [Acc, N]))
                )
        end,
    eval(Ns, Ops, Acc1).
