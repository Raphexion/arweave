-module(app_net_explore).
-export([graph/0, graph/1]).
-export([get_all_nodes/0, get_live_nodes/0, filter_offline_nodes/1]).
-export([get_nodes_connectivity/0,
		 generate_gephi_csv/2,
		 get_nodes_version/1,
		 filter_local_peers/1]).

%%% Tools for building a map of connected peers.
%%% Requires graphviz for visualisation.

%% The directory generated files should be saved to.
-define(OUTPUT_DIR, "net-explore-output").

%% @doc Build a snapshot graph in PNG form of the current state of the network.
graph() ->
	io:format("Getting live peers...~n"),
	graph(get_live_nodes()).
graph(Nodes) ->
	io:format("Generating connection map...~n"),
	Map = generate_map(Nodes),
	ar:d(Map),
	io:format("Generating dot file...~n"),
	Timestamp = erlang:timestamp(),
	DotFile = filename(Timestamp, "graph", "dot"),
	ok = filelib:ensure_dir(DotFile),
	PngFile = filename(Timestamp, "graph", "png"),
	ok = filelib:ensure_dir(PngFile),
	ok = generate_dot_file(DotFile, Map),
	io:format("Generating PNG image...~n"),
	os:cmd("dot -Tpng " ++ DotFile ++ " -o " ++ PngFile),
	io:format("Done! Image written to: '" ++ PngFile ++ "'~n").


%% @doc Return a list of nodes that are active and connected to the network.
get_live_nodes() ->
	filter_offline_nodes(get_all_nodes()).

%% @doc Return a list of all nodes that are claimed to be in the network with
%% a non local address.
get_all_nodes() ->
	Peers = filter_local_peers(
		ar_bridge:get_remote_peers(whereis(http_bridge_node))
	),
	get_all_nodes([], Peers).
get_all_nodes(Acc, []) -> Acc;
get_all_nodes(Acc, [Peer|Peers]) ->
	io:format("Getting peers from ~s... ", [ar_util:format_peer(Peer)]),
	MorePeers = filter_local_peers(peers_by_peer(Peer)),
	io:format(" got ~w!~n", [length(MorePeers)]),
	get_all_nodes(
		[Peer|Acc],
		(ar_util:unique(Peers ++ MorePeers)) -- [Peer|Acc]
	).

%% @doc Remove offline nodes from a list of peers.
filter_offline_nodes(Peers) ->
	NodesWithInfo = ar_util:pmap(
		fun(Peer) ->
			{Peer, ar_http_iface_client:get_info(Peer)}
		end,
		Peers
	),
	FilterMapper = fun
		({_, info_unavailable}) -> false;
		({Peer, _}) -> {true, Peer}
	end,
	lists:filtermap(FilterMapper, NodesWithInfo).

%% @doc Return a three-tuple with every live host in the network, it's average
%% position by peers connected to it, the number of peers connected to it.
get_nodes_connectivity() ->
	nodes_connectivity(generate_map(get_live_nodes())).

%% @doc Create a CSV file with all connections in the network suitable for
%% importing into Gephi - The Open Graph Viz Platform (https://gephi.org/). The
%% weight is based on the Wildfire ranking. Takes a file path to a JSON file
%% described in parse_names_file/1 and a list of peers used as entry points to
%% the network.
generate_gephi_csv(NamesJsonFile, StartPeers) ->
	OnlineNodeMap = lists:filter(
		fun
			({_, unavailable}) -> false;
			(_) -> true
		end,
		maps:to_list(app_net_crawler:crawl(StartPeers))
	),
	generate_gephi_csv1(
		parse_names_file(NamesJsonFile),
		OnlineNodeMap
	).

%% @doc Crawls the network and returns a proplist keyed by the node version and
%% with a counter as value.
get_nodes_version(StartPeers) ->
	FilterMapper = fun
		({_, unavailable}) ->
			false;
		({Node, Peers}) when is_list(Peers) ->
			{true, Node}
	end,
	Nodes = lists:filtermap(
		FilterMapper,
		maps:to_list(app_net_crawler:crawl(StartPeers))
	),
	get_nodes_version1(Nodes).

%% @doc Takes a list of peers and filters out the ones with a local address.
filter_local_peers(Peers) ->
	lists:filter(fun(Peer) -> not is_peer_local(Peer) end, Peers).


%% INTERNAL

%% @doc Fetches the peers for a given peer.
peers_by_peer(Peer) ->
	case ar_http_iface_client:get_peers(Peer) of
		unavailable -> [];
		Peers -> Peers
	end.

%% @doc Return a map of every peers connections including peers with local
%% addresses.
%% Returns a list of tuples with arity 2. The first element is the local peer,
%% the second element is the list of remote peers it talks to.
generate_map(Peers) ->
	ar_util:pmap(
		fun(Peer) ->
			{
				Peer,
				lists:filter(
					fun(RemotePeer) ->
						lists:member(RemotePeer, Peers)
					end,
					peers_by_peer(Peer)
				)
			}
		end,
		Peers
	).

%% @dock Takes a peer and return true if it's a local address, otherwise false.
is_peer_local({127, _, _, _, _}) -> true;
is_peer_local({10, _, _, _, _}) -> true;
is_peer_local({192, 168, _, _, _}) -> true;
is_peer_local(Peer)
	when     Peer >= {172, 16, 0, 0, port}
	andalso  Peer =< {172, 31, 255, 255, port} -> true;
is_peer_local({_, _, _, _, _}) -> false.

%% @doc Generate a filename with path for storing files generated by this module.
filename(Type, Extension) ->
	filename(erlang:timestamp(), Type, Extension).

filename(Timestamp, Type, Extension) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} =
		calendar:now_to_datetime(Timestamp),
	StrTime =
		lists:flatten(
			io_lib:format(
				"~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
				[Year, Month, Day, Hour, Minute, Second]
			)
		),
	lists:flatten(
		io_lib:format(
			"~s/~s-~s.~s",
			[?OUTPUT_DIR, Type, StrTime, Extension]
		)
	).

%% @doc Generate a dot file that can be rendered into a PNG.
generate_dot_file(File, Map) ->
	case file:open(File, [write]) of
		{ok, IoDevice} ->
			io:fwrite(IoDevice, "digraph network_map { ~n", []),
			io:fwrite(IoDevice,
					  "	init [style=filled,color=\".7 .3 .9\"];~n", []),
			do_generate_dot_file(Map, IoDevice),
			ok;
		_ ->
			io:format("Failed to open file for writing.~n"),
			io_error
	end.

do_generate_dot_file([], File) ->
	io:fwrite(File, "} ~n", []),
	file:close(File);
do_generate_dot_file([Host|Rest], File) ->
	{IP, Peers} = Host,
	lists:foreach(
		fun(Peer) ->
			io:fwrite(
				File,
				"\t\"~s\"  ->  \"~s\";  ~n",
				[ar_util:format_peer(IP), ar_util:format_peer(Peer)])
		end,
		Peers
	),
	do_generate_dot_file(Rest, File).

%% @doc Takes a host-to-connections map and returns a three-tuple with every
%% live host in the network, it's average position by peers connected to it, the
%% number of peers connected to it.
nodes_connectivity(ConnectionMap) ->
	WithoutScore = [{Host, empty_score} || {Host, _} <- ConnectionMap],
	WithoutScoreMap = maps:from_list(WithoutScore),
	WithScoreMap = avg_connectivity_score(add_connectivity_score(WithoutScoreMap,
																 ConnectionMap)),
	WithScore = [{Host, SumPos, Count} || {Host, {SumPos, Count}}
										  <- maps:to_list(WithScoreMap)],
	lists:keysort(2, WithScore).

%% @doc Updates the connectivity intermediate scores according the connection
%% map.
add_connectivity_score(ScoreMap, []) ->
	ScoreMap;
add_connectivity_score(ScoreMap, [{_, Connections} | ConnectionMap]) ->
	NewScoreMap = add_connectivity_score1(ScoreMap, add_list_position(Connections)),
	add_connectivity_score(NewScoreMap, ConnectionMap).

%% @doc Updates the connectivity scores according the connection map.
add_connectivity_score1(ScoreMap, []) ->
	ScoreMap;
add_connectivity_score1(ScoreMap, [{Host, Position} | Connections]) ->
	Updater = fun
		(empty_score) ->
			{Position, 1};
		({PositionSum, Count}) ->
			{PositionSum + Position, Count + 1}
	end,
	NewScoreMap = maps:update_with(Host, Updater, ScoreMap),
	add_connectivity_score1(NewScoreMap, Connections).

%% @doc Wraps each element in the list in a two-tuple where the second element
%% is the element's position in the list.
add_list_position(List) ->
	add_list_position(List, 1, []).

add_list_position([], _, Acc) ->
	lists:reverse(Acc);
add_list_position([Item | List], Position, Acc) ->
	NewAcc = [{Item, Position} | Acc],
	add_list_position(List, Position + 1, NewAcc).

%% @doc Replace the intermediate score (the sum of all positions and the number
%% of connections) with the average position and the number of connections.
avg_connectivity_score(Hosts) ->
	Mapper = fun (_, {PositionSum, Count}) ->
		{PositionSum / Count, Count}
	end,
	maps:map(Mapper, Hosts).

%% @doc Parses a JSON map of IP address to name obtained by running
%% nameMapToNetExplore() on the Observatory page.
parse_names_file(NamesJsonFile) ->
	{ok, Json} = file:read_file(NamesJsonFile),
	AddrNameMap = [{parse_ip_addr(IpAddr), Name} || {IpAddr, Name}
		<- maps:to_list(jiffy_to_map(jiffy:decode(Json)))],
	maps:from_list(AddrNameMap).

%% @doc Parse an IP address string (without port).
parse_ip_addr(Addr) ->
	remove_port(ar_util:parse_peer(Addr)).

%% @doc Remove the port element from the IP address octet tuple.
remove_port(IpAddrWithPort) ->
	{A, B, C, D, _} = IpAddrWithPort,
	{A, B, C, D}.

%% @doc Transform the funky jiffy decoded format into using maps.
jiffy_to_map({JiffyObj}) ->
	Props = [{Key, jiffy_to_map(Value)} || {Key, Value} <- JiffyObj],
	maps:from_list(Props);
jiffy_to_map(List) when is_list(List) ->
	[jiffy_to_map(Item) || Item <- List];
jiffy_to_map(Value) ->
	Value.

%% @doc Like generate_gephi_csv/0 but takes the host-to-peers map to use in the
%% export.
generate_gephi_csv1(AddrNameMap, NodeMap) ->
	{IoDevice, File} = create_gephi_file(),
	write_gephi_csv_header(IoDevice),
	write_gephi_csv_rows(use_names(AddrNameMap, gephi_edges(NodeMap)), IoDevice),
	ok = file:close(IoDevice),
	io:format("Gephi CSV file written to: '" ++ File ++ "'~n").

%% @doc Create the new CSV file in write mode and return the IO device and the
%% filename.
create_gephi_file() ->
	CsvFile = filename("gephi", "csv"),
	ok = filelib:ensure_dir(CsvFile),
	{ok, IoDevice} = file:open(CsvFile, [write]),
	{IoDevice, CsvFile}.

%% @doc Write the CSV header line to the IO device.
write_gephi_csv_header(IoDevice) ->
	Header = <<"Source,Target,Weight\n">>,
	ok = file:write(IoDevice, Header).

%% @doc Transform the host to peers map into a list of all connections where
%% each connection is a three-tuple of host, peer, weight.
gephi_edges(Map) ->
	OnlineNodes = [Peer || {Peer, Peers} <- Map, Peers /= unavailable],
	gephi_edges(Map, sets:from_list(OnlineNodes), []).

gephi_edges([], _, Acc) ->
	Acc;
gephi_edges([{Node, Peers} | Map], OnlineNodes, Acc) ->
	NonSelfPeers = list_remove(Node, Peers),
	NonLocalPeers = lists:filter(
		fun(Peer) -> not is_peer_local(Peer) end,
		NonSelfPeers
	),
	PeersWithPosition = add_list_position(NonLocalPeers),
	OnlinePeers = lists:filter(
		fun({Peer, _}) -> sets:is_element(Peer, OnlineNodes) end,
		PeersWithPosition
	),
	NewAcc = lists:foldl(
		fun ({Peer, Position}, FolderAcc) ->
			[{Node, Peer, 1 / Position} | FolderAcc]
		end,
		Acc,
		OnlinePeers
	),
	gephi_edges(Map, OnlineNodes, NewAcc).

list_remove(Remove, List) ->
	lists:filter(
		fun(Item) ->
			Item /= Remove
		end,
		List
	).

%% @doc Replace IP addresses with more human friendly names.
use_names(AddrNameMap, GephiEdges) ->
	Mapper = fun({Host, Peer, Weight}) ->
		{ip_addr_to_name(Host, AddrNameMap),
		 ip_addr_to_name(Peer, AddrNameMap),
		 Weight}
	end,
	lists:map(Mapper, GephiEdges).

ip_addr_to_name(AddrWithPort, AddrNameMap) ->
	{A, B, C, D, Port} = AddrWithPort,
	Addr = {A, B, C, D},
	case maps:find(Addr, AddrNameMap) of
		{ok, Name} ->
			<<Name/binary, ":", (integer_to_binary(Port))/binary>>;
		error ->
			ar_util:format_peer(AddrWithPort)
	end.

%% @doc Write the list of connections to the IO device.
write_gephi_csv_rows([], _) ->
	done;
write_gephi_csv_rows([Edge | Edges], IoDevice) ->
	{Host, Peer, Weight} = Edge,
	Row = io_lib:format("~s,~s,~f\n", [Host, Peer, Weight]),
	ok = file:write(IoDevice, Row),
	write_gephi_csv_rows(Edges, IoDevice).

%% @doc Get a list of all node versions used in the network with a counter of
%% the number of nodes on that version.
get_nodes_version1(Peers) ->
	Mapper = fun (Peer) ->
		{Peer, get_version(Peer)}
	end,
	CountByVersion = maps:map(
		fun (_, PeersByVersion) -> length(PeersByVersion) end,
		group_by_version(ar_util:pmap(Mapper, Peers))
	),
	lists:keysort(1, maps:to_list(CountByVersion)).

%% @doc Get the version a certain peer is running.
get_version(Peer) ->
	case ar_http_iface_client:get_info(Peer) of
		info_unavailable -> unavailable;
		Info -> {proplists:get_value(version, Info), proplists:get_value(release, Info)}
	end.

%% @doc Group a list of peer and version pairs to a list of peers by version.
group_by_version(PeersWithVersion) ->
	Grouper = fun ({_, Version}) -> Version end,
	group_by(PeersWithVersion, Grouper).

%% @doc Group a list into a map by keys generated by the Fun.
group_by(List, Fun) ->
	group_by(List, Fun, maps:new()).

group_by([], _, Acc) ->
	Acc;
group_by([Item | List], Fun, Acc) ->
	Key = Fun(Item),
	NewAcc = maps:put(Key,
					  [Item | maps:get(Key, Acc, [])],
					  Acc),
	group_by(List, Fun, NewAcc).
