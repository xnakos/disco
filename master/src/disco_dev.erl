-module(disco_dev).

-export([init/0, put_urls/1, host/1, host_ddfs_data/1, host_get_port/1, host_put_port/1, slave_name/1]).

-define(DLMTR, "_at_").

init() ->
    ets:new(host_put_ports, [named_table, public]),
    ets:new(host_get_ports, [named_table, public]),
    ets:new(next_host_ports, [named_table, public]),
    ets:insert(next_host_ports, {"PUT", "8992"}),
    ets:insert(next_host_ports, {"GET", "8991"}).

host(Node) ->
    MasterName = disco:master_name(),
    case string:sub_word(atom_to_list(Node), 1, $@) of
        MasterName -> Host = string:sub_word(atom_to_list(Node), 2, $@);
        _ -> {match, [Host]} = re:run(atom_to_list(Node), slave_name_prefix() ++ "(.*)@localhost", [{capture, all_but_first, list}])
    end,
    Host.

host_ddfs_data(Host) ->
    Dir = filename:join(disco:get_setting("DDFS_DATA"), Host),
    file:make_dir(Dir),
    Dir.

host_put_port(Host) ->
    case ets:lookup(host_put_ports, Host) of
        [] ->
            [{_, NextPutPort}] = ets:lookup(next_host_ports, "PUT"),
            ets:insert(host_put_ports, {Host, NextPutPort}),
            ets:insert(next_host_ports, {"PUT", integer_to_list(list_to_integer(NextPutPort) + 2)}),
            NextPutPort;
        [{_, PutPortStr}] -> PutPortStr
    end.

host_get_port(Host) ->
    case ets:lookup(host_get_ports, Host) of
        [] ->
            [{_, NextGetPort}] = ets:lookup(next_host_ports, "GET"),
            ets:insert(host_get_ports, {Host, NextGetPort}),
            ets:insert(next_host_ports, {"GET", integer_to_list(list_to_integer(NextGetPort) + 2)}),
            NextGetPort;
        [{_, GetPortStr}] -> GetPortStr
    end.

put_urls(Urls) ->
    [[A, "localhost", C, host_put_port(B), E, F] || [A, B, C, _D, E, F] <- Urls].

slave_name_prefix() ->
    disco:slave_name() ++ ?DLMTR.

slave_name(Host) ->
    slave_name_prefix() ++ Host.
