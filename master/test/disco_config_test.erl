-module(disco_config_test).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

-export([prop_test/0]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
	 next_state/3]).

-record(state, {}).

-define(SERVER, disco_config).
-define(FS, fs).

all_raw_hosts() -> [[<<"node1:4">>,<<"2">>], [<<"node42:45">>,<<"4">>], [<<"jedi1:8">>,<<"8">>], [<<"barkiller">>,<<"16">>], [<<"foofighter">>,<<"8">>], [<<"sith1:8">>,<<"2">>], [<<"just42">>,<<"42">>], [<<"c3p0">>,<<"1">>], [<<"r2d2">>,<<"1">>]].

raw_hosts() ->
    lists:sublist(shuffle(all_raw_hosts()), random:uniform(length(all_raw_hosts()))).

shuffle(List) ->
    randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
    randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) ->
                    randomize(Acc)
                end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
    D = lists:map(fun(A) ->
                      {random:uniform(), A}
                  end, List),
    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.

is_sublist([], _) -> true;
is_sublist([H|T], L) ->
    lists:member(H, L) andalso is_sublist(T, L -- [H]).

host_name() ->
    elements(disco_config:get_expanded_hosts(all_raw_hosts()) ++ ["nonexistent", "unreal"]).

initial_state() ->
    #state{}.

command(_S) ->
    oneof([{call, ?SERVER, get_config_table, []},
           {call, ?SERVER, blacklist, [host_name()]},
           {call, ?SERVER, whitelist, [host_name()]},
           {call, ?SERVER, gc_blacklist, [host_name()]},
           {call, ?SERVER, gc_whitelist, [host_name()]},
           {call, ?SERVER, save_config_table, [raw_hosts()]}]).

precondition(_, _) -> true.

postcondition(_S, {call, _M, get_config_table, []}, R) ->
    {ok, RH} = R, disco_config:get_expanded_hosts(RH) =:= the_hostlist();
postcondition(_S, {call, _M, blacklist, [HostName]}, _R) ->
    not lists:member(HostName, the_hostlist()) or lists:member(HostName, the_blacklist());
postcondition(_S, {call, _M, whitelist, [HostName]}, _R) ->
    not lists:member(HostName, the_blacklist());
postcondition(_S, {call, _M, gc_blacklist, [HostName]}, _R) ->
    not lists:member(HostName, the_hostlist()) or lists:member(HostName, the_gc_blacklist());
postcondition(_S, {call, _M, gc_whitelist, [HostName]}, _R) ->
    not lists:member(HostName, the_gc_blacklist());
postcondition(_S, {call, _M, save_config_table, [RawHosts]}, _R) ->
    disco_config:get_expanded_hosts(RawHosts) =:= the_hostlist() andalso is_sublist(the_blacklist(), the_hostlist()) andalso is_sublist(the_gc_blacklist(), the_hostlist());
postcondition(_, _, _) ->
    true.

next_state(_, _, _) ->
    #state{}.

the_blacklist() ->
    {_H, B, _G} = config(), [binary_to_list(Element) || Element <- B].

the_gc_blacklist() ->
    {_H, _B, G} = config(), [binary_to_list(Element) || Element <- G].

the_hostlist() ->
    {H, _B, _G} = config(), disco_config:get_expanded_hosts(H).

config() ->
    {ok, Json} = disco_util:read_config("DISCO_MASTER_CONFIG"),
    C = case mochijson2:decode(Json) of
            {struct, Body} ->
                case proplists:is_defined(<<"gc_blacklist">>, Body) of
                    true -> Body;
                    false -> [{<<"gc_blacklist">>, []}] ++ Body
                end;
            L when is_list(L) -> [{<<"hosts">>, L},
                                  {<<"blacklist">>, []},
                                  {<<"gc_blacklist">>, []}]
        end,
    {proplists:get_value(<<"hosts">>, C),
     proplists:get_value(<<"blacklist">>, C),
     proplists:get_value(<<"gc_blacklist">>, C)}.

prologue() ->
    ets:new(?FS, [named_table, public]),
    ets:insert(?FS, {"DISCO_MASTER_CONFIG", "{\"hosts\":[[\"node1:4\",\"2\"]], \"blacklist\":[\"node1\"], \"gc_blacklist\":[\"node1\"]}"}),
    meck:new(disco_util, [unstick, passthrough]),
    meck:expect(disco_util, read_config, fun(Var) -> [{Var, Value}] = ets:lookup(?FS, Var), {ok, Value} end),
    meck:expect(disco_util, write_config, fun(Var, Data) -> ets:insert(?FS, {Var, Data}), ok end),
    meck:new(disco_server, [unstick, passthrough]),
    meck:expect(disco_server, manual_blacklist, fun(_, _) -> ok end),
    meck:expect(disco_server, gc_blacklist, fun(_) -> ok end),
    meck:expect(disco_server, update_config_table, fun(_, _, _) -> ok end),
    meck:new(error_logger, [unstick, passthrough]),
    meck:expect(error_logger, info_report, fun(_) -> ok end),
    meck:expect(error_logger, warning_report, fun(_) -> ok end).

epilogue() ->
    ets:delete(?FS),
    meck:unload(disco_util),
    meck:unload(disco_server),
    meck:unload(error_logger).

prop_works_fine() ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
            begin
                ?SERVER:start_link(),
                {History,_State,Result} = run_commands(?MODULE, Cmds),
                %?SERVER:stop(),
                ?WHENFAIL(io:format("History: ~w\nDISCO_MASTER_CONFIG: ~w\nResult: ~w\n",
                                    [History, element(2, hd(ets:lookup(?FS, "DISCO_MASTER_CONFIG"))), Result]),
                          aggregate(command_names(Cmds), Result =:= ok))
            end)).

prop_test() ->
    prologue(), proper:quickcheck(prop_works_fine()), epilogue().
