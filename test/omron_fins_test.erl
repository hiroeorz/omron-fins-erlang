-module(omron_fins_test).

-include_lib("eunit/include/eunit.hrl").

start_port_test_() ->
    {spawn, 
     {setup, 
      fun()  ->
	      meck:new(gen_udp, [unstick, passthrough]),
	      meck:expect(gen_udp, open, 2, {ok, sock})
      end, 
      fun(_) ->
	      stop_port(),
	      meck:unload(gen_udp)
      end,
      [
       {"Return {ok, pid()} if start_port/2 success.",
        fun() ->
		{ok, Pid} = omron_fins:start_port({127,0,0,1}, 9600),
		?assertEqual(true, is_pid(Pid))
	end
       }
      ]
     }
    }.

read_dm_values_test_() ->
    {spawn, 
     {setup, 
      fun()  -> setup() end, 
      fun(_) -> teardown() end,
      [
       {"Return DM area values",
        fun() ->
		meck:expect(gen_udp, open, 2, {ok, sock}),
		meck:expect(gen_udp, send, 4, ok),
		gen_server:call(port_pid(), reset_identifier),

		Pid = self(),
		F = fun() ->
			    PLCIPAddress = {127, 0, 0, 1},
			    Port = 9600,
			    StartAddress = 1,
			    WordCount = 10,

			    {ok, List} = omron_fins:read_dm_values(
					   PLCIPAddress, 
					   Port, 
					   StartAddress, 
					   WordCount),

			    ?assertEqual([10,9,8,7,6,5,4,3,2,1], List),
			    Pid ! {self(), ok}
			end,
		TestPid = spawn_link(F), timer:sleep(10),

		Bin = <<192,0,7,0,5,0,0,6,0,1,1,1,0,0,0,
			10,0,9,0,8,0,7,0,6,0,5,0,4,0,3,0,2,0,1>>,
		port_pid() ! {udp, sock, {127,0, 0, 1}, 9600, Bin},
		
		receive
		    {TestPid, ok} -> success
		end
	end
       },

       {"Parallel get values.",
        fun() ->
		meck:expect(gen_udp, open, 2, {ok, sock}),
		meck:expect(gen_udp, send, 4, ok),
		gen_server:call(port_pid(), reset_identifier),

		PLCIPAddress = {127, 0, 0, 1},
		Port = 9600,
		StartAddress = 1,
		WordCount = 10,


		Pid = self(),
		F = fun(I) ->
			    {ok, List} = omron_fins:read_dm_values(
					   PLCIPAddress, 
					   Port, 
					   StartAddress, 
					   WordCount),

			    ?assertEqual([10, 9, 8, 7, 6, 5, 4, 3, 2, I], List),
			    Pid ! {self(), ok}
			end,

		TestPids = [{I, spawn_link(fun() -> F(I) end)} 
			    || I <- [1, 2, 3, 4, 5]],

		timer:sleep(10),

		lists:foreach(
		  fun(I) ->
			  Bin = <<192,0,7,0,5,0,0,6,0, 
				  I:8/integer ,1,1,0,0,
				  0,10,0,9,0,8,0,7,0,6,0,5,
				  0,4,0,3,0,2,0,I:8/integer>>,
			  
			  port_pid() ! {udp, sock, {127,0, 0, 1}, 9600, Bin} 
		  end,
		  [1, 2, 3, 4, 5]),
		
		lists:foreach(fun({_, TestPid}) ->
				      receive
					  {TestPid, ok} -> success
				      end
			      end,
			      TestPids)
	end
       }

      ]
     }
    }.

write_dm_values_test_() ->
    {spawn, 
     {setup, 
      fun()  -> setup() end, 
      fun(_) -> teardown() end,
      [
       {"write values to DM area.",
        fun() ->
		meck:expect(gen_udp, open, 2, {ok, sock}),
		meck:expect(gen_udp, send, 4, ok),

		Pid = self(),
		F = fun() ->
			    PLCIPAddress = {127, 0, 0, 1},
			    Port = 9600,
			    StartAddress = 100,
			    WriteList = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1],
			    ok = omron_fins:write_dm_values(PLCIPAddress, 
							   Port, 
							   StartAddress, 
							   WriteList),
			    Pid ! {self(), ok}
			end,
		TestPid = spawn_link(F), timer:sleep(10),

		Bin = <<192,0,7,0,5,0,0,6,0,1,1,2,0,0>>,
		port_pid() ! {udp, sock, {127,0, 0, 1}, 9600, Bin},
		
		receive
		    {TestPid, ok} -> success
		end
	end
       }
      ]
     }
    }.

write_dm_same_value_test_() ->
    {spawn, 
     {setup, 
      fun()  -> setup() end, 
      fun(_) -> teardown() end,
      [
       {"write DM area to same values.",
        fun() ->
		meck:expect(gen_udp, open, 2, {ok, sock}),
		meck:expect(gen_udp, send, 4, ok),

		Pid = self(),
		F = fun() ->
			    PLCIPAddress = {127, 0, 0, 1},
			    Port = 9600,
			    StartAddress = 200,
			    WriteCount = 10,
			    ok = omron_fins:write_dm_same_value(
				   PLCIPAddress, Port, 
				   StartAddress, WriteCount, 22),
			    
			    Pid ! {self(), ok}
			end,
		TestPid = spawn_link(F), timer:sleep(10),

		Bin = <<192,0,7,0,5,0,0,6,0,1,1,3,0,0>>,
		port_pid() ! {udp, sock, {127,0, 0, 1}, 9600, Bin},
		
		receive
		    {TestPid, ok} -> success
		end
	end
       }
      ]
     }
    }.

read_dm_multi_values_test_() ->
    {spawn, 
     {setup, 
      fun()  -> setup() end, 
      fun(_) -> teardown() end,
      [
       {"Return DM area values.",
        fun() ->
		meck:expect(gen_udp, open, 2, {ok, sock}),
		meck:expect(gen_udp, send, 4, ok),

		Pid = self(),
		F = fun() ->
			    PLCIPAddress = {127, 0, 0, 1},
			    Port = 9600,
			    AddressList = [1, 3, 5, 7, 9],

			    {ok, List} = omron_fins:read_dm_multi_values(
					   PLCIPAddress, 
					   Port,
					   AddressList),

			    ?assertEqual([10, 8, 6, 4, 2], List),
			    Pid ! {self(), ok}
			end,
		TestPid = spawn_link(F), timer:sleep(10),

		Bin = <<192,0,7,0,5,0,0,6,0,1,1,4,0,0,
			130,0,10,130,0,8,130,0,6,130,0,4,130,0,2>>,
		port_pid() ! {udp, sock, {127,0, 0, 1}, 9600, Bin},
		
		receive
		    {TestPid, ok} -> success
		end
	end
       }
      ]
     }
    }.

read_datetime_test_() ->
    {spawn, 
     {setup, 
      fun()  -> setup() end, 
      fun(_) -> teardown() end,
      [
       {"Return datetime PLC internal",
        fun() ->
		meck:expect(gen_udp, open, 2, {ok, sock}),
		meck:expect(gen_udp, send, 4, ok),

		Pid = self(),
		F = fun() ->
			    PLCIPAddress = {127, 0, 0, 1},
			    Port = 9600,
			    
			    {ok, DateTime} = omron_fins:read_datetime(
					   PLCIPAddress, Port),

			    OKResult = {{datetime,{{2013,9,22}, {18,25,7}}},
					{day_of_week,1}},

			    ?assertEqual(OKResult, DateTime),
			    Pid ! {self(), ok}
			end,
		TestPid = spawn_link(F), timer:sleep(10),

		Bin = <<192,0,7,0,5,0,0,6,0,1,7,1,0,0,19,9,34,24,37,7,0>>,
		port_pid() ! {udp, sock, {127,0, 0, 1}, 9600, Bin},
		
		receive
		    {TestPid, ok} -> success
		end
	end
       }
      ]
     }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

setup() ->
    meck:new(gen_udp, [unstick, passthrough]),
    meck:expect(gen_udp, open, 2, {ok, sock}),
    {ok, _} = omron_fins:start_port({127,0,0,1}, 9600).
    
teardown() ->
    stop_port(),
    meck:unload(gen_udp).

port_pid() ->
    ChildList = supervisor:which_children(omron_fins_sup),
    serch_child({omron_fins_port, 9600}, ChildList).

serch_child(_, []) ->
    undefined;

serch_child(Id, [{Id, Pid, _, _} | _]) ->
    Pid;

serch_child(Id, [_ | Tail]) ->
    serch_child(Id, Tail).

stop_port() ->
    supervisor:terminate_child(omron_fins_sup, {omron_fins_port, 9600}),
    supervisor:delete_child(omron_fins_sup, {omron_fins_port, 9600}).

