%% @doc Runlevel N
%%
%% This runlevel is useless in the sense that it
%% does not affect a running system.
%% The purpose of this runlevel is to aid the
%% developer and his/her understanding and mental
%% model.
%%
%% Runlevel N only job is to start the last
%% real runlevel.
%%
%% By starting this runlevel you are guaranteed
%% to start the full system.

-module(ar_runlevelN).
-behaviour(supervisor).
-define(SERVER, ?MODULE).
-define(LAST_REAL_RUNLEVEL, runlevel1).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

init([]) ->
    Children = [ %% always start the last real runlevel
		 %% the last real runlevel right now is 1
		 #{id => ?LAST_REAL_RUNLEVEL,
		   type => supervisor,
		   start => {?LAST_REAL_RUNLEVEL, start_link, []}}

		 %% no children in this runlevel to start
	       ],
    {ok, {{one_for_one, 1, 1}, Children}}.
