%% @doc Help module for ar_bridge
%% Place support functions related to adding
%% end

-module(ar_bridge_maybe).
-include("ar.hrl").
-include("ar_bridge.hrl").

-export([maybe_send_tx_to_internal/2]).

%% @doc
maybe_send_tx_to_internal(State=#state{firewall = FW}, Data) ->
    maybe_send_tx_to_internal_scan(State, Data, ar_firewall:scan_tx(FW, Data)).

maybe_send_tx_to_internal_scan(State, _, reject) ->
    %% If the data does not pass the scan, ignore the message.
    State;

maybe_send_tx_to_internal_scan(State, Data, accept) ->
	#state{
		gossip=GS, 
		processed=Procd,
		remote_peers=RemotePeers
	} = State,
    %% The message is at least valid, distribute it.
    Msg = {add_tx, Data},
	{NewGS, _} = ar_gossip:send(GS, Msg),
    ar_bridge_send:send_to_external(RemotePeers, Msg),
    ar_bridge_add:add_processed_tx(Data, Procd),
    State#state{gossip = NewGS}.
