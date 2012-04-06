-module(radius_dict_file).

-export([load/1]).

-include("radius.hrl").

load(File) ->
    case file:open(dictionary_path(File), [read]) of
        {ok, Fd} ->
            lists:flatten(read_line(Fd));
        {error, Reason} ->
            error_logger:error_msg(
                "** Can't load RADIUS dictionary file ~s~n"
                "   for the reason ~p: ~s~n",
                [File, Reason, file:format_error(Reason)])
    end.

%%
%% Internal functions
%%
dictionary_path(File) ->
    PrivDir = case code:priv_dir(radius) of
        {error, bad_name} ->
            "./priv";
        D -> D
    end,
    filename:join([PrivDir, File]).

read_line(Fd) ->
    read_line(Fd, []).
read_line(Fd, Acc) ->
    case io:get_line(Fd, "") of
        eof ->
            file:close(Fd),
            lists:reverse(Acc);
        Line ->
            L = strip_comments(Line),
            case parse_line(string:tokens(L, "\t\n\s")) of
                {ok, Result} ->
                    read_line(Fd, [Result | Acc]);
                _ ->
                    read_line(Fd, Acc)
            end
    end.

strip_comments(Line) ->
    case string:chr(Line, $#) of
        0 ->
            Line;
        I ->
            L = string:sub_string(Line, 1, I - 1),
            string:strip(L)
    end.

parse_line(["$INCLUDE", File]) ->
    {ok, load(File)};

parse_line(["ATTRIBUTE", Name, Code, Type]) ->
    case get(vendor) of
        undefined ->
            {ok, #attribute{name = Name, code = list_to_integer(Code), type = list_to_atom(Type)}};
        {_VendorName, VendorID} ->
            C = {VendorID, list_to_integer(Code)},
            {ok, #attribute{name = Name, code = C, type = list_to_atom(Type)}}
    end;

parse_line(["ATTRIBUTE", Name, Code, Type, Extra]) ->
    case get(vendor) of
        undefined -> % ATTRIBUTE name number type OPTIONS
            Opts = [parse_option(string:tokens(I, "=")) || I <- string:tokens(Extra, ",")],
            A = #attribute{name = Name, code = list_to_integer(Code), type = list_to_atom(Type)},
            {ok, A#attribute{opts = Opts}};
        {Extra, VendorID} -> % ATTRIBUTE name number type VENDOR-NAME
            C = {VendorID, list_to_integer(Code)},
            A = #attribute{name = Name, code = C, type = list_to_atom(Type)},
            {ok, A};
        {_VendorName, VendorID} -> % ATTRIBUTE name number type VENDOR-OPTIONS
            Opts = [parse_option(string:tokens(I, "=")) || I <- string:tokens(Extra, ",")],
            C = {VendorID, list_to_integer(Code)},
            A = #attribute{name = Name, code = C, type = list_to_atom(Type)},
            {ok, A#attribute{opts = Opts}}
    end;

parse_line(["VALUE", A, Name, Value]) ->
    V = #value{aname = A, vname = Name, value = list_to_integer(Value)},
    {ok, V};
parse_line(["VENDOR", Name, Code]) ->
    put(vendor, {Name, list_to_integer(Code)});
parse_line(["END-VENDOR", _]) ->
    erase(vendor);
parse_line(_) ->
    ok.

parse_option(["has_tag"]) ->
    has_tag;
parse_option(["encrypt", Value]) ->
    {encrypt, list_to_integer(Value)}.
