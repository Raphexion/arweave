%% @doc Represents a bridge node in the internal gossip network
%% to the external message passing interfaces.
%% end

-module(ar_bridge).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("ar.hrl").
-include("ar_bridge.hrl").

%% API
-export([start_link/3, stop/0]).
-export([add_tx/1, add_block/3]). % Called from ar_http_iface
-export([add_remote_peer/1, add_local_peer/1]).
-export([get_remote_peers/0, set_remote_peers/1]).
-export([ignore_id/1]).
-export([ignore_peer/1]).
-export([is_id_ignored/1]).

%% Behaviour callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link(ExtPeers, IntPeers, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {ExtPeers, IntPeers, Port}, []).

stop() ->
    gen_server:stop(?MODULE).

%%

add_tx(Tx) ->
    gen_server:call(?MODULE, {add_tx, Tx}).

add_block(OriginPeer, Block, Recall) ->
    gen_server:call(?MODULE, {add_block, OriginPeer, Block, Recall}).

%%

add_remote_peer(Peer) ->
    gen_server:call(?MODULE, {add_remote_peer, Peer}).

add_local_peer(Peer) ->
    gen_server:call(?MODULE, {add_local_peer, Peer}).

set_remote_peers(Peers) ->
    gen_server:call(?MODULE, {set_remote_peers, Peers}).

get_remote_peers() ->
    gen_server:call(?MODULE, get_remote_peers).

%%

ignore_peer(Peer) ->
    gen_server:call(?MODULE, {ignore_peer, Peer}).

ignore_id(Id) ->
    gen_server:call(?MODULE, {ignore_id, Id}).

is_id_ignored(Id) ->
    gen_server:call(?MODULE, {is_id_ignored, Id}).

%%------------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init({ExtPeers, IntPeers, Port}) ->
    ar:report([starting_ignored_ids_db]),
    ets:new(ignored_ids, [set, public, named_table]),
    State = #state {
	       gossip = ar_gossip:init(IntPeers),
	       remote_peers = ExtPeers,
	       port = Port
	      },
    reset_timer(),
    {ok, State, ?GET_MORE_PEERS_TIME}.

%% @hidden
handle_call(get_remote_peers, _From, State=#state{remote_peers = ExtPeers}) ->
    {reply, ExtPeers, State};

handle_call({add_remote_peer, Peer}, _From, State=#state{remote_peers = ExtPeers}) ->
    Peers = case is_loopback_ip(Peer) of
		true -> ExtPeers;
		false -> [Peer|ExtPeers]
	    end,
    {reply, ok, State#state{remote_peers = Peers}};

handle_call({ignore_peer, Peer}, _From, State=#state{ignored_peers = Peers}) ->
    erlang:send_after(?IGNORE_PEERS_TIME, self(), {unignore_peer, Peer}),
    {reply, ok, State#state{ignored_peers = [Peer|Peers]}};

handle_call({add_local_peer, Peer}, _From, State=#state{gossip=GS0}) ->
    Gossip = ar_gossip:add_peers(GS0, Peer)
    {reply, ok, State#state{gossip=Gossip};

handle_call({add_tx, TX}, _From, State) ->
    ar_bridge_maybe:maybe_send_tx_to_internal(State, TX);

handle_call({is_id_ignored, Id}, _From, State) ->
    Res = case ets:lookup(ignored_ids, Id) of
	      [{Id, ignored}] -> true;
	      _ -> false
	  end,
    {reply, Res, State};

handle_call({add_block, OriginPeer, Block, Recall}, _From, State) ->
    ar_bridge_send:send_block_to_internal(State,
					  OriginPeer,
					  Block,
					  Recall,
					  fun ar_bridge_add:add_processed_block/2);

handle_call({set_remote_peers, Peers}, _From, State) ->
    {reply, ok, State#state{remote_peers=Peers}}.

%% @hidden
handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info({unignore_peer, Peer}, State=#state{ignored_peers = OrgPeers}) ->
    Peers = lists:delete(Peer, OrgPeers),
    {noreply, State#state{ignored_peers = Peers}};

%% @hidden
handle_info(get_more_peers, State) ->
    Peers = ar_manage_peers:update(S#state.external_peers),
    lists:map(fun ar_http_iface_client:add_peer/1, Peers),
    reset_timer(),
    {noreply, State#state{remote_peers=Peers}}.

handle_info(_What, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

%% @doc Is the IP address in question a loopback ('us') address?
is_loopback_ip({A, B, C, D, _Port}) -> is_loopback_ip({A, B, C, D});
is_loopback_ip({127, _, _, _}) -> true;
is_loopback_ip({0, _, _, _}) -> true;
is_loopback_ip({169, 254, _, _}) -> true;
is_loopback_ip({255, 255, 255, 255}) -> true;
is_loopback_ip(_) -> false.

%% @doc Schedule a message timer.
reset_timer() ->
	erlang:send_after(?GET_MORE_PEERS_TIME, self(), get_more_peers).

%% @doc Possibly send a new message to external peers.
gossip_to_external(S = #state { processed = Procd }, {NewGS, Msg}) ->
    NewS = ar_bridge_send:send_to_external(S#state{gossip=NewGS}, Msg),
    ar_brige_add:add_processed(Msg, Procd),
    NewS.
