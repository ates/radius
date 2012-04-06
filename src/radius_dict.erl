-module(radius_dict).

%% API
-export([add/1, lookup_attribute/1, lookup_value/2, to_list/1]).

-include("radius.hrl").

-define(ATTRS_TABLE, radius_dict_attrs).
-define(VALUES_TABLE, radius_dict_values).

%% @doc Adds RADIUS attribute of value to internal storage
%% @spec add(radius_attribute() | radius_value()) -> true
-spec add(radius_attribute() | radius_value()) -> true.
add(Attribute) when is_record(Attribute, attribute) ->
    ets:insert(?ATTRS_TABLE, Attribute);
add(Value) when is_record(Value, value) ->
    Key = {Value#value.aname, Value#value.value},
    ets:insert(?VALUES_TABLE, {Key, Value}).

%% @doc Looking for the specified RADIUS attribute
%% @spec lookup_attribute(string() | non_neg_integer() | tuple()) ->
%%  not_found | radius_attribute()
-spec lookup_attribute(string() | non_neg_integer() | tuple()) ->
    not_found | radius_attribute().
lookup_attribute(Name) when is_list(Name) ->
    Pattern = {attribute, '_', '_', Name, '_'},
    case ets:match_object(?ATTRS_TABLE, Pattern, 1) of
        {[Attribute], _} ->
            Attribute;
        '$end_of_table' ->
            not_found
    end;
lookup_attribute(Code) ->
    case ets:lookup(?ATTRS_TABLE, Code) of
        [Attribute] ->
            Attribute;
        [] ->
            not_found
    end.

%% @doc Looking for the specified RADIUS value
%% @spec lookup_value(string(), string()) -> not_found | term().
-spec lookup_value(string(), string()) -> not_found | term().
lookup_value(A, V) ->
    case ets:lookup(?VALUES_TABLE, {A, V}) of
        [{_Key, Value}] ->
            Value;
        [] ->
            not_found
    end.

%% @doc Returns the list of registered attributes or values.
%% @spec to_list(attrs | values) -> [] | [radius_attribute() | radius_value()]
-spec to_list(attrs | values) -> [] | [radius_attribute() | radius_value()].
to_list(attrs) ->
    ets:tab2list(?ATTRS_TABLE);
to_list(values) ->
    ets:tab2list(?VALUES_TABLE).
