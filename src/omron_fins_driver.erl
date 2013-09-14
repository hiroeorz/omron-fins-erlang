%%%-------------------------------------------------------------------
%%% @author HIROE Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%% Driver for OMRON FINS COMMAND.
%%% @end
%%% Created :  5 Aug 2013 by HIROE Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(omron_fins_driver).

%% Include
-include("omron_fins.hrl").

%% API
-export([create_header/2, create_header/7,
	 update_header/3,
	 command/2, 
	 send_command/4, 
	 parse_response/2,
	 get_process_identifier/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc create fins command header.
%% @end
%%--------------------------------------------------------------------
-spec create_header(DstNode, SrcNode) -> #fins_header{} when
      DstNode :: non_neg_integer(),
      SrcNode :: non_neg_integer().
create_header(DstNode, SrcNode) ->
    create_header(16#00, DstNode, 16#00, 16#00, SrcNode, 16#00, 16#00).

-spec create_header(DstNetwork, DstNode, DstUnitNo, 
		    SrcNetwork, SrcNode, SrcUnitNo, 
		    Identifier) -> #fins_header{} when
      DstNetwork :: non_neg_integer(),
      DstNode :: non_neg_integer(),
      DstUnitNo :: non_neg_integer(), 
      SrcNetwork :: non_neg_integer(),
      SrcNode :: non_neg_integer(),
      SrcUnitNo :: non_neg_integer(), 
      Identifier :: non_neg_integer().
create_header(DstNetwork, DstNode, DstUnitNo, SrcNetwork, SrcNode, SrcUnitNo, 
	      Identifier) ->
    #fins_header{dst_address = DstNetwork,
		 dst_node    = DstNode, 
		 dst_unit_no = DstUnitNo, 
		 src_address = SrcNetwork,
		 src_node    = SrcNode, 
		 src_unit_no = SrcUnitNo, 
		 identifier  = Identifier}.

%%--------------------------------------------------------------------
%% @doc set plc process identifier to header record.
%% @end
%%--------------------------------------------------------------------
-spec update_header(non_neg_integer(), 
		    non_neg_integer(),
		    #fins_header{}) -> #fins_header{}.
update_header(DstIPNode, Identifier, Header) ->
    Header#fins_header{dst_node = DstIPNode,
		       identifier = Identifier}.

%%--------------------------------------------------------------------
%% @doc send request to PLC.
%% @end
%%--------------------------------------------------------------------
-spec send_command(Sock, Host, Port, CommandBin) ->
			  ok | {error, Reason} when
      Sock :: gen_udp:socket(), 
      Host :: inet:ip_address(), 
      Port :: inet:port_number(),
      CommandBin :: binary(),
      Reason :: atom().
send_command(Sock, Host, Port, CommandBin)  when is_binary(CommandBin) ->
    ok = gen_udp:send(Sock, Host, Port, CommandBin).

%%--------------------------------------------------------------------
%% @doc get process identifier from response command.
%% @end
%%--------------------------------------------------------------------
-spec get_process_identifier(Bin) -> Sid when
      Bin :: binary(),
      Sid :: non_neg_integer().
get_process_identifier(Bin) when is_binary(Bin) ->
    <<_Icf:8/unsigned-integer,
      _Rsv:8/unsigned-integer,
      _Gct:8/unsigned-integer,
      _Dna:8/unsigned-integer,
      _Da1:8/unsigned-integer,
      _Da2:8/unsigned-integer,
      _Sna:8/unsigned-integer,
      _Sa1:8/unsigned-integer,
      _Sa2:8/unsigned-integer,
      Sid:8/unsigned-integer,
      _CodeMR:8/unsigned-integer,
      _CodeSR:8/unsigned-integer,
      _FinishCode1:8/unsigned-integer,
      _FinishCode2:8/unsigned-integer,
      _BodyBin/binary>> = Bin,
    Sid.

%%--------------------------------------------------------------------
%% @doc create command data.
%% @end
%%--------------------------------------------------------------------
-spec command(Header, Params) -> binary() when
      Header :: #fins_header{},
      Params :: tuple().

%% read io values
command(Header, {?CODE_READ_IO, IOFacility, StartAddress, ReadCount}) 
  when is_record(Header, fins_header) andalso
       (IOFacility =:= ?IO_FACILITY_DM_CHANEL orelse 
	IOFacility =:= ?IO_FACILITY_DM_BIT) andalso
       is_integer(StartAddress) andalso
       is_integer(ReadCount) ->

    AddressBit = 0,
    Body = <<IOFacility:8/unsigned-integer,
	     StartAddress:16/big-unsigned-integer,
	     AddressBit:8/unsigned-integer,
	     ReadCount:16/big-unsigned-integer>>,

    fmt_command(Header, ?CODE_READ_IO, Body);

%% write io values
command(Header, {?CODE_WRITE_IO, IOFacility, StartAddress, Count, List}) 
  when is_record(Header, fins_header) andalso
       (IOFacility =:= ?IO_FACILITY_DM_CHANEL orelse 
	IOFacility =:= ?IO_FACILITY_DM_BIT) andalso
       is_integer(StartAddress) andalso
       is_integer(Count) andalso
       is_list(List)->

    DataBinList = [<<Val:16/big-unsigned-integer>> || Val <- List],
    DataBin = list_to_binary(DataBinList),
    AddressBit = 0,
    Body = <<IOFacility:8/unsigned-integer,
	     StartAddress:16/big-unsigned-integer,
	     AddressBit:8/unsigned-integer,
	     Count:16/big-unsigned-integer,
	     DataBin/binary>>,

    fmt_command(Header, ?CODE_WRITE_IO, Body);

%% write io to same values
command(Header, 
	{?CODE_WRITE_IO_SAME_VALUE, IOFacility, StartAddress, Count, Value}) 
  when is_record(Header, fins_header) andalso
       (IOFacility =:= ?IO_FACILITY_DM_CHANEL orelse 
	IOFacility =:= ?IO_FACILITY_DM_BIT) andalso
       is_integer(StartAddress) andalso
       is_integer(Count) andalso
       is_integer(Value) ->

    AddressBit = 0,
    Body = <<IOFacility:8/unsigned-integer,
	     StartAddress:16/big-unsigned-integer,
	     AddressBit:8/unsigned-integer,
	     Count:16/big-unsigned-integer,
	     Value:16/big-unsigned-integer>>,

    fmt_command(Header, ?CODE_WRITE_IO_SAME_VALUE, Body);

%% read io multi values
command(Header, {?CODE_READ_IO_MULTI, IOFacility, AddressList}) 
  when is_record(Header, fins_header) andalso
       (IOFacility =:= ?IO_FACILITY_DM_CHANEL orelse 
	IOFacility =:= ?IO_FACILITY_DM_BIT) andalso
       is_list(AddressList) ->

    AddressBit = 0,
    BinList = [<<IOFacility:8/unsigned-integer,
		 Address:16/big-unsigned-integer,
		 AddressBit:8/unsigned-integer>> || Address <- AddressList],

    Body = list_to_binary(BinList),
    fmt_command(Header, ?CODE_READ_IO_MULTI, Body);

%% read datetime
command(Header, {?CODE_READ_DATETIME}) 
  when is_record(Header, fins_header) ->
    fmt_command(Header, ?CODE_READ_DATETIME, <<>>);

%% release alert history
command(Header, {?CODE_RELEASE_ALERT, AlertCode}) 
  when is_record(Header, fins_header) andalso
       is_integer(AlertCode) ->
    Body = <<AlertCode:16/big-unsigned-integer>>,
    fmt_command(Header, ?CODE_RELEASE_ALERT, Body);

%% read alert history
command(Header, {?CODE_READ_ALERT_HISTORY, StartRecordNo, Count}) 
  when is_record(Header, fins_header) andalso
       is_integer(StartRecordNo) andalso
       is_integer(Count) ->

    Body = <<StartRecordNo:16/big-unsigned-integer,
	     Count:16/big-unsigned-integer>>,

    fmt_command(Header, ?CODE_READ_ALERT_HISTORY, Body);

%% clear alert history
command(Header, {?CODE_CLEAR_ALERT_HISTORY}) 
  when is_record(Header, fins_header) ->
    fmt_command(Header, ?CODE_CLEAR_ALERT_HISTORY, <<>>).

%%--------------------------------------------------------------------
%% @doc parse response.
%% @end
%%--------------------------------------------------------------------
-spec parse_response(IOFacility, Bin) -> 
			    ok |
			    {ok, term()} | 
			    {error, {FinishCode1, FinishCode2}} when
      IOFacility :: non_neg_integer(),
      Bin :: binary(),
      FinishCode1 :: non_neg_integer(),
      FinishCode2 :: non_neg_integer().
parse_response(IOFacility, Bin) when is_binary(Bin) ->
    <<_Icf:8/unsigned-integer,
      _Rsv:8/unsigned-integer,
      _Gct:8/unsigned-integer,
      _Dna:8/unsigned-integer,
      _Da1:8/unsigned-integer,
      _Da2:8/unsigned-integer,
      _Sna:8/unsigned-integer,
      _Sa1:8/unsigned-integer,
      _Sa2:8/unsigned-integer,
      _Sid:8/unsigned-integer,
      CodeMR:8/unsigned-integer,
      CodeSR:8/unsigned-integer,
      FinishCode1:8/unsigned-integer,
      FinishCode2:8/unsigned-integer,
      BodyBin/binary>> = Bin,
    
    case {FinishCode1, FinishCode2} of
	?FINISH_CODE_SUCCESS ->
	    handle_parse({CodeMR, CodeSR}, {IOFacility, BodyBin});
	_ ->
	    handle_response_error(FinishCode1, FinishCode2)
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc response parse handler.
%% @end
%%--------------------------------------------------------------------
-spec handle_parse(Code, {IO_FACILITY, BodyBin}) -> ok | {ok, term()} when
      Code :: {non_neg_integer(), non_neg_integer()},
      IO_FACILITY :: non_neg_integer(),
      BodyBin :: binary().
handle_parse(?CODE_READ_IO, {IO_FACILITY, BodyBin}) ->
    parse_uint_value(IO_FACILITY, BodyBin);

handle_parse(?CODE_WRITE_IO, {_IO_FACILITY, _BodyBin}) ->
    ok;

handle_parse(?CODE_WRITE_IO_SAME_VALUE, {_IO_FACILITY, _BodyBin}) ->
    ok;

handle_parse(?CODE_READ_IO_MULTI, {_IO_FACILITY, BodyBin}) ->
    parse_uint_multi_value(BodyBin);

handle_parse(?CODE_READ_DATETIME, {_, BodyBin}) ->
    parse_datetime(BodyBin);

handle_parse(?CODE_RELEASE_ALERT, {_, <<>>}) ->
    ok;

handle_parse(?CODE_READ_ALERT_HISTORY, {_, BodyBin}) ->
    <<_RecordMaxCount:16/big-unsigned-integer,
      TotalHistoryCount:16/big-unsigned-integer,
      ReadCount:16/big-unsigned-integer,
      HistoryBin/binary >> = BodyBin,
    
    HistoryList = parse_alert_history(HistoryBin),

    [{total_history_count, TotalHistoryCount},
     {read_count, ReadCount},
     {history, HistoryList}];

handle_parse(?CODE_CLEAR_ALERT_HISTORY, {_, _BodyBin}) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc parse alert history object from binary data.
%% @end
%%--------------------------------------------------------------------

parse_alert_history(Bin) ->
    parse_alert_history(Bin, []).

parse_alert_history(<<>>, Results) ->
    lists:reverse(Results);

parse_alert_history(Bin, Results) when byte_size(Bin) > 0 ->
    <<Code:16/big-unsigned-integer,
      Reason:16/big-unsigned-integer,
      Min10:4/unsigned-integer,   Min1:4/unsigned-integer,   %% BCD
      Sec10:4/unsigned-integer,   Sec1:4/unsigned-integer,   %% BCD
      Day10:4/unsigned-integer,   Day1:4/unsigned-integer,   %% BCD
      Hour10:4/unsigned-integer,  Hour1:4/unsigned-integer,  %% BCD
      Year10:4/unsigned-integer,  Year1:4/unsigned-integer,  %% BCD
      Month10:4/unsigned-integer, Month1:4/unsigned-integer, %% BCD
      TailBin/binary>> = Bin,
    
    Min = Min10 * 10 + Min1,
    Sec = Sec10 * 10 + Sec1,
    Day = Day10 * 10 + Day1,
    Hour = Hour10 * 10 + Hour1,
    Year = 2000 + Year10 * 10 + Year1,
    Month = Month10 * 10 + Month1,

    DateTime = {{Year, Month, Day}, {Hour, Min, Sec}},
    Alert = [{code, Code},
	     {reason, Reason},
	     {datetime, DateTime}],

    parse_alert_history(TailBin, [Alert | Results]).

parse_datetime(Bin) ->
    <<Year10:4/unsigned-integer,      Year1:4/unsigned-integer,     %% BCD
      Month10:4/unsigned-integer,     Month1:4/unsigned-integer,    %% BCD
      Day10:4/unsigned-integer,       Day1:4/unsigned-integer,      %% BCD
      Hour10:4/unsigned-integer,      Hour1:4/unsigned-integer,     %% BCD
      Min10:4/unsigned-integer,       Min1:4/unsigned-integer,      %% BCD
      Sec10:4/unsigned-integer,       Sec1:4/unsigned-integer,      %% BCD
      DayOfWeek:8/unsigned-integer >> = Bin,

    Year = 2000 + Year10 * 10 + Year1,
    Month = Month10 * 10 + Month1,
    Day = Day10 * 10 + Day1,
    Hour = Hour10 * 10 + Hour1,
    Min = Min10 * 10 + Min1,
    Sec = Sec10 * 10 + Sec1,
    [{datetime,    {{Year, Month, Day}, {Hour, Min, Sec}}},
     {day_of_week, DayOfWeek}].

%%--------------------------------------------------------------------
%% @private
%% @doc parse response, that is list of uint(16bit).
%% @end
%%--------------------------------------------------------------------
parse_uint_value(IOFacility, Bin) ->
    parse_uint_value(IOFacility, Bin, []).

parse_uint_value(_IOFacility, <<>>, Results) ->
    lists:reverse(Results);

parse_uint_value(?IO_FACILITY_DM_CHANEL = IOFacility, 
		 <<V:16/big-unsigned-integer, TailBin/binary>>, Results) ->
    parse_uint_value(IOFacility, TailBin, [V | Results]);

parse_uint_value(?IO_FACILITY_DM_BIT = IOFacility, 
		 <<Bit:8/unsigned-integer, TailBin/binary>>, Results) ->
    parse_uint_value(IOFacility, TailBin, [Bit | Results]).

%%--------------------------------------------------------------------
%% @private
%% @doc parse response, that is list of uint(16bit multi).
%% @end
%%--------------------------------------------------------------------

parse_uint_multi_value(Bin) ->
    parse_uint_multi_value(Bin, []).

parse_uint_multi_value(<<>>, Results) ->
    lists:reverse(Results);

parse_uint_multi_value(<<?IO_FACILITY_DM_BIT:8/unsigned-integer,
			 Bit:8/unsigned-integer, 
			 TailBin/binary>>, Results) ->
    parse_uint_multi_value(TailBin, [Bit | Results]);

parse_uint_multi_value(<<?IO_FACILITY_DM_CHANEL:8/unsigned-integer,
			 V:16/big-unsigned-integer, 
			 TailBin/binary>>, Results) ->
    parse_uint_multi_value(TailBin, [V | Results]).

%%--------------------------------------------------------------------
%% @private
%% @doc fromat fins command.
%% @end
%%--------------------------------------------------------------------
-spec fmt_command(Header, {CodeMR, CodeSR}, ParamsBin) -> binary() when
      Header :: #fins_header{},
      CodeMR :: non_neg_integer(),
      CodeSR :: non_neg_integer(),
      ParamsBin :: binary().
fmt_command(Header, {CodeMR, CodeSR}, ParamsBin) ->
    list_to_binary([fmt_header(Header), fmt_code(CodeMR, CodeSR), ParamsBin]).

%%--------------------------------------------------------------------
%% @private
%% @doc fromat fins command code.
%% @end
%%--------------------------------------------------------------------
-spec fmt_code(non_neg_integer(), non_neg_integer()) -> binary().
fmt_code(CodeMR, CodeSR) when is_integer(CodeMR) andalso
			      is_integer(CodeSR) ->
    <<CodeMR:8/unsigned-integer, CodeSR:8/unsigned-integer>>.

%%--------------------------------------------------------------------
%% @private
%% @doc format fins command header.
%% @end
%%--------------------------------------------------------------------
-spec fmt_header(#fins_header{}) -> binary().
fmt_header(#fins_header{dst_address = DstAddress, 
			dst_node    = DstNode, 
			dst_unit_no = DstUnitNo, 
			src_address = SrcAddress, 
			src_node    = SrcNode, 
			src_unit_no = SrcUnitNo, 
			identifier  = Identifier} = _Header) ->

    list_to_binary([icf(), rsv(), gct(), 
		    dna(DstAddress), da1(DstNode), da2(DstUnitNo), 
		    sna(SrcAddress), sa1(SrcNode), sa2(SrcUnitNo), 
		    sid(Identifier)]).

%%--------------------------------------------------------------------
%% @private
%% @doc information control field
%% @end
%%--------------------------------------------------------------------
icf() ->
    <<1:1, %% bridge enable(1) or disable(0). fixed 1.
      0:1, %% data facility (0:command, 1:response)
      0:1, %% system reserved
      0:1, %% system reserved
      0:1, %% system reserved
      0:1, %% system reserved
      0:1, %% system reserved
      0:1  %% response request (0:yes 1:no)
    >>.

%%--------------------------------------------------------------------
%% @private
%% @doc system reserved. 00HEX fixed.
%% @end
%%--------------------------------------------------------------------
rsv() ->
    <<16#00:8>>.

%%--------------------------------------------------------------------
%% @private
%% @doc allowed bridge passing count. 02HEX fixed.
%% @end
%%--------------------------------------------------------------------
gct() ->
    <<16#02:8>>.

%%--------------------------------------------------------------------
%% @private
%% @doc destination network address.
%% @end
%%--------------------------------------------------------------------
dna(DstNetwork) when is_integer(DstNetwork) andalso
		    DstNetwork =:= 16#00 orelse %% self
		    (1 =< DstNetwork andalso DstNetwork =< 16#7F) ->
    <<DstNetwork:8/unsigned-integer>>.

%%--------------------------------------------------------------------
%% @private
%% @doc destination node address.
%% @end
%%--------------------------------------------------------------------
da1(Node) when is_integer(Node) andalso
	       Node =:= 16#00 orelse %% self
	       Node =:= 16#FF orelse %% broadcast
	       (0 =< Node andalso Node =< 16#20) ->
    <<Node:8/unsigned-integer>>.

%%--------------------------------------------------------------------
%% @private
%% @doc destination unit address.
%% @end
%%--------------------------------------------------------------------
da2(UnitNo) when is_integer(UnitNo) andalso
		   UnitNo =:= 16#00 orelse %% self
		   UnitNo =:= 16#FE orelse %% Link Unit/Ethernet Unit
		   UnitNo =:= 16#E1 orelse %% Inner Board
		   (1 =< UnitNo andalso UnitNo =< 16#7F) ->
    <<UnitNo:8/unsigned-integer>>.

%%--------------------------------------------------------------------
%% @private
%% @doc source network address.
%% @end
%%--------------------------------------------------------------------
sna(SrcNetwork) when is_integer(SrcNetwork) andalso
		     SrcNetwork =:= 16#00 orelse %% self
		     1 =< SrcNetwork andalso SrcNetwork =< 16#7F ->
    <<SrcNetwork:8/unsigned-integer>>.

%%--------------------------------------------------------------------
%% @private
%% @doc source node address.
%% @end
%%--------------------------------------------------------------------
sa1(Node) when is_integer(Node) andalso
	       Node =:= 16#00 orelse %% self
	       (0 =< Node andalso Node =< 16#20) ->
    <<Node:8/unsigned-integer>>.

%%--------------------------------------------------------------------
%% @private
%% @doc source unit address.
%% @end
%%--------------------------------------------------------------------
sa2(UnitNo) when is_integer(UnitNo) andalso
		   UnitNo =:= 16#00 orelse %% self
		   (10 =< UnitNo andalso UnitNo =< 16#1F) ->
    <<UnitNo:8/unsigned-integer>>.

%%--------------------------------------------------------------------
%% @private
%% @doc source process identifier
%% @end
%%--------------------------------------------------------------------
sid(Identifier) when is_integer(Identifier) andalso
		     (0 =< Identifier andalso Identifier =< 16#FF) ->
    <<Identifier:8/unsigned-integer>>.

%%--------------------------------------------------------------------
%% @private
%% @doc fins command error response handler.
%% @end
%%--------------------------------------------------------------------

handle_response_error(FinishCode1, FinishCode2) ->
    {error, {FinishCode1, FinishCode2}}.
