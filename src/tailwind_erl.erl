-module(tailwind_erl).

-export([change_file_permissions/2]).

to_result(Result) ->
    case Result of
        ok ->
            {ok, nil};
        {ok, Value} ->
            {ok, Value}
    end.

change_file_permissions(Path, Permission) ->
    to_result(file:change_mode(Path, list_to_integer(string:trim(integer_to_list(Permission), leading, "0"), 8))).
