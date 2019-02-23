%% @doc Runlevel 1
%%
%% This runlevel is the first real runlevel.
%% Services that have few or no dependencies
%% often go here.
%% Runlevel 1 mostly concern data services.

-module(ar_runlevel1).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

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
    Children = [ %% always start the previous runlevel
		 #{id => runlevel0,
		   type => supervisor,
		   start => {runlevel0, start_link, []}}

		 %% services that are started at this runlevel
		 %% #{id => ar_meta_db,
		 %%   type => worker,
		 %%   start => {ar_meta_db, start_link, []}}
	       ],
    {ok, {{one_for_one, 1, 1}, Children}}.
