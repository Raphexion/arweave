%% @doc Help module for ar_bridge
%% Place support functions related to sending
%% end

-module(ar_bridge_send).
-include("ar.hrl").
-include("ar_bridge.hrl").

-export([send_to_peer/1]).
-export([send_to_external/2]).
-export([send_block_to_external/4]).
-export([send_block_to_external_parallel/5]).
-export([send_block_to_internal/5]).

%%------------------------------------------------------------------------
%% Send to peer
%%------------------------------------------------------------------------

%% @doc curried function to be used with fold or map
send_to_peer(Tx) ->
    fun(Peer, Acc) -> send_to_peer(Tx, Peer, Acc) end.

%% @doc send to peer that calls logic
send_to_peer(Tx, Peer, Acc) ->
    send_to_peer(Tx,
		 Peer,
		 Acc,
		 ar_http_iface_client:has_tx(Peer, Tx#tx.id),
		 Acc =< ?NUM_REGOSSIP_TX).

%% @doc send to peer with logic
send_to_peer(Tx, Peer, Acc, false, true) ->
    ar_http_iface_client:send_new_tx(Peer, Tx),
    Acc + 1;
send_to_peer(_Tx, _Peer, Acc, _, _) ->
    Acc.

%%------------------------------------------------------------------------
%% Send to external
%%------------------------------------------------------------------------

%% @doc Send an internal message externally
send_to_external(RemotePeers, {add_tx, Tx}) ->
    ar:report([
	       {sending_tx_to_external_peers, ar_util:encode(Tx#tx.id)},
	       {peers, length(RemotePeers)}
	      ]),
    lists:foldl(ar_bridge_send:send_to_peer(Tx), 0, ar_util:disorder(RemotePeers)).

%%------------------------------------------------------------------------
%% Send block to external
%%------------------------------------------------------------------------

%% @doc Send a block to external peers in a spawned process.
send_block_to_external(RemotePeers, B, OriginPeer, Recall) ->
    {RecallIndepHash, Key, Nonce} = Recall,
    case ar_block:get_recall_block(OriginPeer, RecallIndepHash, B#block.hash_list, Key, Nonce) of
	unavailable -> ok;
	RecallB ->
	    ar:report([
		       {sending_block_to_external_peers, ar_util:encode(B#block.indep_hash)},
		       {peers, length(RemotePeers)}
		      ]),
	    send_block_to_external_parallel(RemotePeers, B, RecallB, Key, Nonce)
    end.

%%------------------------------------------------------------------------
%% Send block to parallel
%%------------------------------------------------------------------------

%% @doc Send the new block to the peers by first sending it in parallel to the
%% best/first peers and then continuing sequentially with the rest of the peers
%% in order.
send_block_to_external_parallel(Peers, NewB, RecallB, Key, Nonce) ->
    {PeersParallel, PeersSequencial} = lists:split(
					 min(length(Peers), ?BLOCK_PROPAGATION_PARALLELIZATION),
					 Peers
					),
    Send = fun(Peer) ->
		   ar_http_iface_client:send_new_block(Peer, NewB, RecallB, Key, Nonce)
	   end,
    ar_util:pmap(Send, PeersParallel),
    lists:foreach(Send, PeersSequencial).

%%------------------------------------------------------------------------
%% Send block to internally
%%------------------------------------------------------------------------

%% @doc Potentially send a block to internal processes.
send_block_to_internal(State, OriginPeer, B, Recall, AddProcessedBlock) ->
    #state {
       gossip = GS,
       processed = Procd,
       remote_peers = RemotePeers
      } = State,

    %% TODO: Is it always appropriate not to check whether the block has
    %% already been processed?
    %%(not already_processed(Procd, Type, Data)) andalso
    %% The message is at least valid, distribute it.
    %% {OriginPeer, NewB, RecallIndepHash} = Data,

    Msg = {new_block, OriginPeer, B#block.height, B, Recall},
    {NewGS, _} = ar_gossip:send(GS, Msg),
    ar_bridge_send:send_block_to_external(RemotePeers, B, OriginPeer, Recall),
    AddProcessedBlock(B, Procd),
    State#state{gossip = NewGS}.
