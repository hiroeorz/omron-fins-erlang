%% RECORD
-record(fins_header, {dst_address  :: non_neg_integer(),
		      dst_node     :: non_neg_integer(),
		      dst_unit_no  :: non_neg_integer(),
		      src_address  :: non_neg_integer(),
		      src_node     :: non_neg_integer(),
		      src_unit_no  :: non_neg_integer(),
		      identifier   :: non_neg_integer()}).

%% MACRO
-define(CODE_READ_IO,             {16#01, 16#01}).
-define(CODE_WRITE_IO,            {16#01, 16#02}).
-define(CODE_WRITE_IO_SAME_VALUE, {16#01, 16#03}).
-define(CODE_READ_IO_MULTI,       {16#01, 16#04}).
-define(CODE_READ_DATETIME,       {16#07, 16#01}).
-define(CODE_WRITE_DATETIME,      {16#07, 16#02}).
-define(CODE_RELEASE_ALERT,       {16#21, 16#01}).
-define(CODE_READ_ALERT_HISTORY,  {16#21, 16#02}).
-define(CODE_CLEAR_ALERT_HISTORY, {16#21, 16#03}).

-define(FINISH_CODE_SUCCESS, {16#00, 16#00}).
-define(IO_FACILITY_DM_CHANEL, 16#82).
-define(IO_FACILITY_DM_BIT,    16#02).
-define(RESPONSE_TIMEOUT, 3000).
-define(HEADER_LENGTH, 10).
-define(CODE_LENGTH, 2).
-define(FINISH_CODE_LENGTH, 2).

%% TYPE
-type fins_command_code() :: {non_neg_integer(), non_neg_integer()}.
-type omron_fins_error_code() :: {non_neg_integer(), non_neg_integer()}.
