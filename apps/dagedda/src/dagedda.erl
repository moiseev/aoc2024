-module(dagedda).

-export([get/3]).

-define(BASE_URL, <<"https://adventofcode.com">>).

get(Year, Day, Options) ->
    ConfigPath = filename:join([os:getenv("HOME"), ".config", "aoc.config"]),
    {ok, [{session_cookie, SessionCookie}]} = file:consult(ConfigPath),
    Fname =
        case maps:get(cache_dir, Options, not_set) of
            not_set ->
                undefined;
            Dir ->
                lists:flatten(io_lib:format("~s/day/~2..0w.txt", [Dir, Day]))
        end,
    case filelib:is_regular(Fname) of
        false ->
            %io:format("Doing the request"),
            Url0 = io_lib:format("~s/~p/day/~p/input", [?BASE_URL, Year, Day]),
            Url = iolist_to_binary(Url0),
            Headers = [{"Cookie", SessionCookie}],
            RequestOptions = [{full_result, false}, {body_format, binary}],
            {ok, {200, Data}} = httpc:request(
                get, {Url, Headers}, [], RequestOptions
            ),
            Fname /= undefined andalso
                begin
                    %io:format("~s~n", [Fname]),
                    ok = filelib:ensure_dir(Fname),
                    ok = file:write_file(Fname, Data)
                end,
            {ok, Data};
        true ->
            %io:format("Cache baby~n"),
            file:read_file(Fname)
    end.
