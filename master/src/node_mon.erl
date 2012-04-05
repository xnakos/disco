-module(node_mon).
-export([start_link/1]).

-define(RESTART_DELAY, 15000).
-define(SLAVE_ARGS, "+K true -connect_all false").
-define(RPC_CALL_TIMEOUT, 30000).
-define(RPC_RETRY_TIMEOUT, 120000).

-type host() :: nonempty_string().

-spec start_link(host()) -> pid().
start_link(Host) ->
    spawn_link(fun() -> spawn_node(Host) end).

-spec spawn_node(host()) -> 'ok'.
spawn_node(Host) ->
    process_flag(trap_exit, true),
    spawn_node(Host, is_master(Host)).

-spec spawn_node(host(), boolean()) -> 'ok'.
spawn_node(Host, IsMaster) ->
    case {IsMaster, catch slave_start(Host)} of
        {true, {ok, Node}} ->
            % start a dummy ddfs_node process for the master, no get or put
            start_ddfs_node(node(), {false, false}),
            % start ddfs_node for the slave on the master node.
            % put enabled, but no get, which is handled by master
            node_monitor(Host, Node, {false, true});
        {false, {ok, Node}} ->
            % normal remote ddfs_node, both put and get enabled
            node_monitor(Host, Node, {true, true});
        {_, {error, {already_running, Node}}} ->
            node_monitor(Host, Node, {not(IsMaster), true});
        {_, {error, timeout}} ->
            error_logger:info_report({"Connection timed out to", Host}),
            disco_server:connection_status(Host, down);
        Error ->
            error_logger:warning_report(
                {"Spawning node @", Host, "failed for unknown reason", Error}),
            disco_server:connection_status(Host, down)
    end,
    timer:sleep(?RESTART_DELAY).

-spec node_monitor(host(), node(), {boolean(), boolean()}) -> 'ok'.
node_monitor(Host, Node, WebConfig) ->
    monitor_node(Node, true),
    start_ddfs_node(Node, WebConfig),
    start_temp_gc(Node),
    start_lock_server(Node),
    disco_server:connection_status(Host, up),
    wait(Node),
    disco_server:connection_status(Host, down).

-spec wait(node()) -> 'ok'.
wait(Node) ->
    receive
        {is_ready, Pid} ->
            Pid ! node_ready,
            wait(Node);
        {'EXIT', _, already_started} ->
            error_logger:info_report({"Already started", Node, self()}),
            wait(Node);
        {'EXIT', _, Reason} ->
            error_logger:info_report({"Node failed", Node, Reason});
        {nodedown, _Node} ->
            error_logger:info_report({"Node", Node, "down"});
        E ->
            error_logger:info_report({"Erroneous message (node_mon)", E})
    end.

slave_env() ->
    Home = disco:get_setting("DISCO_MASTER_HOME"),
    lists:flatten([?SLAVE_ARGS,
                   [io_lib:format(" -pa ~s/ebin/~s", [Home, Dir])
                    || Dir <- [""]],
                   [io_lib:format(" -pa ~s/deps/~s/ebin", [Home, Dir])
                    || Dir <- ["mochiweb", "lager"]],
                   [io_lib:format(" -env ~s '~s'", [S, disco:get_setting(S)])
                    || S <- disco:settings()]]).

-spec slave_start(host()) -> {'ok', node()} | {'error', _}.
slave_start(Host) ->
    error_logger:info_report({"starting node @", Host}),
    disco_dev:host_put_port(Host),
    disco_dev:host_get_port(Host),
    slave:start("localhost",
                disco_dev:slave_name(Host),
                slave_env(),
                self(),
                disco:get_setting("DISCO_ERLANG")).

-spec is_master(host()) -> boolean().
is_master(_Host) ->
    false.

-spec start_temp_gc(node()) -> pid().
start_temp_gc(Node) ->
    spawn_link(Node, temp_gc, start_link, [node(whereis(disco_server))]).

-spec start_lock_server(node()) -> pid().
start_lock_server(Node) ->
    spawn_link(Node, lock_server, start_link, []).

-spec start_ddfs_node(node(), {boolean(), boolean()}) -> pid().
start_ddfs_node(Node, {GetEnabled, PutEnabled}) ->
    DdfsRoot = disco_dev:host_ddfs_data(disco:host(Node)),
    DiscoRoot = disco:get_setting("DISCO_DATA"),
    PutMax = list_to_integer(disco:get_setting("DDFS_PUT_MAX")),
    GetMax = list_to_integer(disco:get_setting("DDFS_GET_MAX")),
    PutPort = list_to_integer(disco_dev:host_put_port(disco:host(Node))),
    GetPort = list_to_integer(disco_dev:host_get_port(disco:host(Node))),
    Args = [{nodename, disco:host(Node)},
            {ddfs_root, DdfsRoot}, {disco_root, DiscoRoot},
            {put_max, PutMax}, {get_max, GetMax},
            {put_port, PutPort}, {get_port, GetPort},
            {get_enabled, GetEnabled},
            {put_enabled, PutEnabled}],
    spawn_link(Node, ddfs_node, start_link, [Args]).
