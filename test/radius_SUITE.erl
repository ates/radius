-module(radius_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("radius.hrl").

%% CT callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% tests
-export([attribute_value/1]).
-export([attribute_lookup/1]).
-export([access_request/1]).

all() ->
    [{group, codec}, {group, radius}].

groups() ->
    CodecTests = [
        attribute_value,
        attribute_lookup
    ],
    RadiusTests = [
        access_request
    ],
    [
        {codec, [parallel], CodecTests},
        {radius, [parallel], RadiusTests}
    ].

init_per_suite(Config) ->
    {ok, _Started} = application:ensure_all_started(radius),
    lists:foreach(fun radius_dict:add/1, radius_dict_file:load("dictionary")),
    server:start(),
    Config.

end_per_suite(Config) ->
    ok = application:stop(radius),
    Config.

init_per_group(_Group, Config) -> Config.

end_per_group(_Group, Config) -> Config.

init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, Config) -> Config.

attribute_value(_Config) ->
    Packet = #radius_packet{
        code = 1,
        ident = <<1>>,
        auth = <<150,97,28,26,189,109,225,40,196,115,4,231,91,164,40,86>>,
        attrs = [
            {"Message-Authenticator",<<235,48,33,2,199,70,80,39,54,79,139,53,86,198,222,114>>},
            {"NAS-Port",1},
            {"NAS-IP-Address",{192,168,1,123}},
            {"Password",[173,212,78,171,80,245,131,197,211,29,50,200,173,144,73,55]},
            {"User-Name","joel"}
        ]
    },
    1 = radius_codec:attribute_value("NAS-Port", Packet),
    {192,168,1,123} = radius_codec:attribute_value("NAS-IP-Address", Packet),
    undefined = radius_codec:attribute_value("Framed-IP-Address", Packet).

attribute_lookup(_Config) ->
    Attr1 = #attribute{
        code = 1, type = string, name = "User-Name", opts = undefined
    },
    Attr2 = #attribute{
        code = {10415, 1}, type = string, name = "3GPP-IMSI", opts = undefined
    },
    not_found = radius_dict:lookup_attribute("Fake-Attribute"),
    Attr1 = radius_dict:lookup_attribute("User-Name"),
    Attr2 = radius_dict:lookup_attribute({10415, 1}).

access_request(_Config) ->
    {ok, Pid} = radius_client:start_link({127, 0, 0, 1}, 1812, "testing123"),
    {ok, Response} = radius_client:send(Pid, ?ACCESS_REQUEST, [{"User-Name", "joel"}]),
    {10, 10, 0, 1} = radius_codec:attribute_value("Framed-IP-Address", Response),
    radius_client:stop(Pid).
