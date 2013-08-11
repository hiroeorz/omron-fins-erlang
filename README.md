===================================================================
Omron Fins Client for Erlang/OTP Application.
====================================================================

omron-fins-erlang is fins command client written by Erlang.

This application support communication to omron PLC from Erlang application.

* omron: <http://www.omron.co.jp/>
* omron PLC: <http://www.fa.omron.co.jp/products/category/automation-systems/programmable-controllers/>

Install
------------------------------------------------------------------

    $ git clone git@github.com:hiroeorz/omron-fins-erlang.git
    $ cd omron-fins-erlang
    $ make

Example
------------------------------------------------------------------

Start Server

    > SrcIPAddress = {192,168,0,5}.
    > Port = 9600.
    > {ok, _Pid} = omron_fins:start_port(SrcIPAddress, Port).

Read DM Values.

    > PLCIPAddress = {192,168,0,6}. %% PLC IP Address
    > Port = 9600.                  %% Port number.
    > StartAddress = 1.             %% DM Start Address
    > WordCount = 10.               %% DM Read Count
    
    > omron_fins:read_dm_values(PLCIPAddress, Port, StartAddress, WordCount).
      {ok, [0,0,0,1,2,0,0,0,0,10]}

Write DM Values.

    > omron_fins:write_dm_values(PLCIPAddress, Port, StartAddress, [1,2,3]).    
       ok

Write DM Same Values

    > Val = 0.                      %% Value
    > WordCount = 10.               %% DM Write Count
    > omron_fins:write_dm_same_values(PLCIPAddress, Port, StartAddress, WordCount, Val).
       ok

Read DM Multi Values

    > AddressList = [1, 3, 5, 6].
    > omron_fins:read_dm_multi_values(PLCIPAddress, Port, AddressList).
      {ok, [0,0,2,0]}
