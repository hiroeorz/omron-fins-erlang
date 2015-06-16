%%%-------------------------------------------------------------------
%%% @author HIROE Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created :  9 Aug 2013 by HIROE Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(omron_fins_port_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 set_pid/2,
	 get_pid/1,
	 delelte_pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {ports = dict:new() :: dict:dict()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, atom()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc set new port and pid to manager.
%% @end
%%--------------------------------------------------------------------
-spec set_pid(inet:port_number(), pid()) -> ok.
set_pid(Port, Pid) ->
    gen_server:call(?SERVER, {set_pid, Port, Pid}).

%%--------------------------------------------------------------------
%% @doc get pid to manager by port number.
%% @end
%%--------------------------------------------------------------------
-spec get_pid(inet:port_number()) -> {ok, pid()} | {error, not_found}.
get_pid(Port) ->
    gen_server:call(?SERVER, {get_pid, Port}).

%%--------------------------------------------------------------------
%% @doc delete pid from manager.
%% @end
%%--------------------------------------------------------------------
-spec delelte_pid(inet:port_number()) -> ok.
delelte_pid(Port) ->
    gen_server:call(?SERVER, {delete_pid, Port}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({set_pid, Port, Pid}, _From, State) ->
    Dict = State#state.ports,
    NewDict = dict:store(Port, Pid, Dict),
    {reply, ok, State#state{ports = NewDict}};

handle_call({get_pid, Port}, _From, State) ->
    Dict = State#state.ports,
    
    Reply = case dict:find(Port, Dict) of
		{ok, Pid} ->
		    {ok, Pid};
		error ->
		    {error, not_found}
	    end,

    {reply, Reply, State};

handle_call({delete_pid, Port}, _From, State) ->
    Dict = State#state.ports,
    NewDict = dict:erase(Port, Dict),
    {reply, ok, State#state{ports = NewDict}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
