%% @doc Help module for ar_bridge
%% Place support functions related to adding
%% end

-module(ar_bridge_add).
-include("ar.hrl").

-export([add_processed/2]).
-export([add_processed_tx/2]).
-export([add_processed_block/2]).

%% @doc Add the ID of a new TX/block to a processed list.
add_processed({add_tx, TX}, Procd) ->
	add_processed_tx(TX, Procd);

add_processed({new_block, _, _, B, _}, Procd) ->
	add_processed_block(B, Procd);

add_processed(X, _Procd) ->
	ar:report([{could_not_ignore, X},
		   {record, X}
		  ]),
	ok.

%% @doc Add the ID of a new TX to a processed list.
add_processed_tx(#tx{id = Id}, _Procd) ->
	ets:insert(ignored_ids, {Id, ignored}).

%% @doc Add the ID of a new block to a processed list.
add_processed_block(#block{indep_hash = Hash}, _Procd) ->
	ets:insert(ignored_ids, {Hash, ignored}).
