-module(app_net_crawler).
-export([crawl/0, crawl/1]).

%%% A parallel network crawler that will find all nodes in the network.

%% @doc Crawls the network and returns a map of peers by node. A new fetch will
%% start as soon as a new node is found. Therefor, the number of parallel
%% fetches is not limited and exponential to the network size.
crawl() ->
	StartPeers = app_net_explore:filter_local_peers(
		ar_bridge:get_remote_peers(whereis(http_bridge_node))
	),
	crawl(StartPeers).

crawl(StartPeers) ->
	InitState = start_fetching(#{}, StartPeers),
	crawl_loop(InitState).

%% @doc Main loop for the crawler.
crawl_loop(State) ->
	case count_ongoing(State) of
		0 ->
			State;
		_ ->
			crawl_loop(wait_for_more(State))
	end.

%% @doc Returns the number of ongoing fetches.
count_ongoing(State) ->
	IsOngoing = fun
		(fetching) -> true;
		(_) -> false
	end,
	length(lists:filter(IsOngoing, maps:values(State))).

%% @doc Wait for one fetch worker to finish and start new fetch workers for the
%% new nodes found.
wait_for_more(State) ->
	receive
		{found_peers, Node, unavailable} ->
			maps:update(Node, unavailable, State);
		{found_peers, Node, Peers} ->
			UnseenNodes = lists:usort(Peers -- maps:keys(State)),
			NewState = start_fetching(State, UnseenNodes),
			io:format("Ongoing fetch count: ~p~n", [count_ongoing(NewState)]),
			maps:update(Node, Peers, NewState)
	end.

%% @doc Start fetches for all the supplied nodes.
start_fetching(State, Nodes) ->
	NonLocalNodes = app_net_explore:filter_local_peers(Nodes),
	lists:foreach(fun start_worker/1, NonLocalNodes),
	StateOverlay = [{Node, fetching} || Node <- NonLocalNodes],
	maps:merge(State, maps:from_list(StateOverlay)).

%% @doc Start a fetch worker for one node.
start_worker(Node) ->
	Master = self(),
	spawn_link(
		fun() ->
			Master ! {found_peers, Node, fetch_peers(Node)}
		end
	).

%% @doc Try to fetch peers for a node. Will timeout and try up to 4 times with
%% a 20 seconds delay in between.
fetch_peers(Node) ->
	fetch_peers(Node, {1, 4}).

fetch_peers(_, {_Attempt, _MaxAttempts}) when _Attempt >= _MaxAttempts ->
	unavailable;
fetch_peers(Node, {Attempt, MaxAttempts}) when Attempt > 1 ->
	timer:sleep(30 * 1000),
	io:format(
		"Retrying (try ~B of ~B) fetching peers from: ~p~n",
		[Attempt, MaxAttempts, Node]
	),
	fetch_peers1(Node, {Attempt, MaxAttempts});
fetch_peers(Node, {Attempt, MaxAttempts}) ->
	fetch_peers1(Node, {Attempt, MaxAttempts}).

fetch_peers1(Node, {Attempt, MaxAttempts}) ->
	case ar_http_iface_client:get_peers(Node, 10 * 1000) of
		unavailable ->
			fetch_peers(Node, {Attempt + 1, MaxAttempts});
		Peers ->
			Peers
	end.
