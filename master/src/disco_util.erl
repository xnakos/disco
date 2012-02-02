-module(disco_util).
-export([groupby/2, join/2, read_config/1, write_config/2]).

% Define locally since this is not exported from stdlib/timer.erl or erts/erlang.erl.
-type timestamp() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-export_type([timestamp/0]).

groupby(N, TupleList) ->
    groupby(N, TupleList, []).

groupby(_N, [], Groups) -> lists:reverse(Groups);
groupby(N, [H|_] = List, Groups) ->
    Key = element(N, H),
    {Group, Rest} = lists:splitwith(fun(X) -> Key =:= element(N, X) end, List),
    groupby(N, Rest, [Group|Groups]).

join([], _Separator) -> [];
join([_] = List, _Separator) -> List;
join([F|List], Separator) ->
    lists:flatten([F, [[Separator, E] || E <- List]]).

read_config(EnvVar) -> file:read_file(disco:get_setting(EnvVar)).

write_config(EnvVar, Data) -> file:write_file(disco:get_setting(EnvVar), Data).
