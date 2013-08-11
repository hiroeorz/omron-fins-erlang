%%%-------------------------------------------------------------------
%%% @author HIROE Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created :  9 Aug 2013 by HIROE Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(omron_fins).

%% Include
-include("omron_fins.hrl").

%% API
-export([start_port/2,
	 read_dm_values/4,
	 write_dm_values/4,
	 write_dm_same_value/5,
	 read_dm_multi_values/3,
	 release_alert/3,
	 read_alert_history/4,
	 clear_alert_history/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc start port server.
%% @end
%%--------------------------------------------------------------------
-spec start_port(SrcIP, Port) -> supervisor:startchild_ret() when
      SrcIP :: inet:ip_address(),
      Port :: inet:port_number().
start_port(SrcIP, Port) ->
    _ = application:start(omron_fins),
    Child = {{omron_fins_port, Port}, 
	     {omron_fins_port, start_link, [SrcIP, Port]},
	     permanent, 2000, worker, [omron_fins_port]},
    supervisor:start_child(omron_fins_sup, Child).

%%--------------------------------------------------------------------
%% @doc read values from DM area.
%% @end
%%--------------------------------------------------------------------
-spec read_dm_values(DstIP, Port, StartAddress, Count) -> 
			    {ok, [non_neg_integer()]} |
			    {error, timeout} |
			    {error, omron_fins_error_code()} when
      DstIP :: inet:ip_address(),
      Port :: inet:port_number(),
      StartAddress :: non_neg_integer(),
      Count :: non_neg_integer().
read_dm_values(DstIP, Port, StartAddress, Count) ->
    Command = {?CODE_READ_IO, ?IO_FACILITY_DM_CHANEL, StartAddress, Count},
    omron_fins_port:send_command(DstIP, Port, Command).

%%--------------------------------------------------------------------
%% @doc write values to DM area.
%% @end
%%--------------------------------------------------------------------
-spec write_dm_values(DstIP, Port, StartAddress, List) -> 
			    ok |
			    {error, timeout} |
			    {error, omron_fins_error_code()} when
      DstIP :: inet:ip_address(),
      Port :: inet:port_number(),
      StartAddress :: non_neg_integer(),
      List :: [non_neg_integer()].
write_dm_values(DstIP, Port, StartAddress, List) ->
    Command = {?CODE_WRITE_IO, ?IO_FACILITY_DM_CHANEL, 
	       StartAddress, length(List), List},
    omron_fins_port:send_command(DstIP, Port, Command).

%%--------------------------------------------------------------------
%% @doc write same value to DM area.
%% @end
%%--------------------------------------------------------------------
-spec write_dm_same_value(DstIP, Port, StartAddress, Count, Value) -> 
				 ok |
				 {error, timeout} |
				 {error, omron_fins_error_code()} when
      DstIP :: inet:ip_address(),
      Port :: inet:port_number(),
      StartAddress :: non_neg_integer(),
      Count :: non_neg_integer(),
      Value :: non_neg_integer().
write_dm_same_value(DstIP, Port, StartAddress, Count, Value) ->
    Command = {?CODE_WRITE_IO_SAME_VALUE, ?IO_FACILITY_DM_CHANEL, 
	       StartAddress, Count, Value},
    omron_fins_port:send_command(DstIP, Port, Command).

%%--------------------------------------------------------------------
%% @doc read DM values from address list.
%% @end
%%--------------------------------------------------------------------
-spec read_dm_multi_values(DstIP, Port, AddressList) -> 
				  {ok, [non_neg_integer()]} |
				  {error, timeout} |
				  {error, omron_fins_error_code()} when
      DstIP :: inet:ip_address(),
      Port :: inet:port_number(),
      AddressList :: [non_neg_integer()].
read_dm_multi_values(DstIP, Port, AddressList) ->
    Command = {?CODE_READ_IO_MULTI, ?IO_FACILITY_DM_CHANEL, AddressList},
    omron_fins_port:send_command(DstIP, Port, Command).


%%--------------------------------------------------------------------
%% @doc release alert in PLC.
%% @end
%%--------------------------------------------------------------------
-spec release_alert(DstIP, Port, AlertCodeStr) -> 
			   ok |
			   {error, timeout} |
			   {error, omron_fins_error_code()} when
      DstIP :: inet:ip_address(),
      Port :: inet:port_number(),
      AlertCodeStr :: list() | {non_neg_integer() | non_neg_integer()}.
release_alert(DstIP, Port, AlertCodeStr) when is_list(AlertCodeStr) ->
    AlertCode = list_to_integer(AlertCodeStr, 16),
    release_alert(DstIP, Port, AlertCode);

release_alert(DstIP, Port, AlertCode) when is_integer(AlertCode) ->
    Command = {?CODE_RELEASE_ALERT, AlertCode},
    omron_fins_port:send_command(DstIP, Port, Command).

%%--------------------------------------------------------------------
%% @doc read PLC alert list.
%% @end
%%--------------------------------------------------------------------
-spec read_alert_history(DstIP, Port, StartRecordNo, Count) ->
				{ok, [tuple()]} |
				{error, timeout} |
				{error, omron_fins_error_code()} when
      DstIP :: inet:ip_address(),
      Port :: inet:port_number(),
      StartRecordNo :: non_neg_integer(),
      Count :: non_neg_integer().
read_alert_history(DstIP, Port, StartRecordNo, Count) ->
    Command = {?CODE_READ_ALERT_HISTORY, StartRecordNo, Count},
    omron_fins_port:send_command(DstIP, Port, Command).

%%--------------------------------------------------------------------
%% @doc clear PLC alert list.
%% @end
%%--------------------------------------------------------------------
-spec clear_alert_history(DstIP, Port) ->
				 ok |
				 {error, timeout} |
				 {error, omron_fins_error_code()} when
      DstIP :: inet:ip_address(),
      Port :: inet:port_number().
clear_alert_history(DstIP, Port) ->
    Command = {?CODE_CLEAR_ALERT_HISTORY},
    omron_fins_port:send_command(DstIP, Port, Command).

%%%===================================================================
%%% Internal functions
%%%===================================================================
