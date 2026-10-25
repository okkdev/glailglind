-module(tailwind_erl).

-export([ os_arch/0, os_platform/0, sha256_file/1]).

os_arch() ->
    Arch = erlang:system_info(system_architecture),
    list_to_binary(hd(string:tokens(Arch, "-"))).

os_platform() ->
    Platform = case os:type() of
        {win32, nt} ->
            "win32";
        {unix, linux} ->
            "linux";
        {unix, darwin} ->
            "darwin";
        {unix, freebsd} ->
            "freebsd";
        {_, _} ->
            "unknown"
    end,
    list_to_binary(Platform).

sha256_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    string:lowercase(binary:encode_hex(crypto:hash(sha256, Bin))).
