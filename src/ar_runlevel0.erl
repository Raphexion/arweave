%% @doc Runlevel 0
%% This runlevel is useless in the sense that it
%% does not affect a running system.
%% The purpose of this runlevel is to aid the
%% developer and his/her understanding and mental
%% model.
%%
%% Runlevel 0 is a superviser that does neither start
%% any other supervisors nor any children.
%%
%% Think of this as the base case in recursion.

-module(ar_runlevel0).
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
    Children = [ %% no runlevel below this to start
		 %% no children in this runlevel to start
	       ],
    {ok, {{one_for_one, 1, 1}, Children}}.
