%%%-------------------------------------------------------------------
%%% @author HIROE Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%% Supervisor for omron_fins_port_manager and each port server.
%%% @end
%%% Created :  9 Aug 2013 by HIROE Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(omron_fins_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Manager = {'omron_fins_port_manager', 
	       {'omron_fins_port_manager', start_link, []},
	       Restart, Shutdown, Type, ['omron_fins_port_manager']},

    {ok, {SupFlags, [Manager]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
