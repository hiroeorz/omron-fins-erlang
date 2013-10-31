

# Module omron_fins #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Authors:__ HIROE Shin ([`hiroe.orz@gmail.com`](mailto:hiroe.orz@gmail.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-omron_fins_error_code">omron_fins_error_code()</a> ###



<pre><code>
omron_fins_error_code() = {non_neg_integer(), non_neg_integer()}
</code></pre>





### <a name="type-plc_datetime">plc_datetime()</a> ###



<pre><code>
plc_datetime() = {datetime, <a href="calendar.md#type-datetime">calendar:datetime()</a>}
</code></pre>





### <a name="type-plc_datetime_and_daynum">plc_datetime_and_daynum()</a> ###



<pre><code>
plc_datetime_and_daynum() = {<a href="#type-plc_datetime">plc_datetime()</a>, <a href="#type-plc_daynum">plc_daynum()</a>}
</code></pre>





### <a name="type-plc_daynum">plc_daynum()</a> ###



<pre><code>
plc_daynum() = {day_of_week, <a href="calendar.md#type-daynum">calendar:daynum()</a>}
</code></pre>





### <a name="type-send_command_error">send_command_error()</a> ###



<pre><code>
send_command_error() = {error, timeout} | {error, enetdown} | {error, <a href="#type-omron_fins_error_code">omron_fins_error_code()</a>}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#clear_alert_history-2">clear_alert_history/2</a></td><td>clear PLC alert list.</td></tr><tr><td valign="top"><a href="#read_alert_history-4">read_alert_history/4</a></td><td>read PLC alert list.</td></tr><tr><td valign="top"><a href="#read_datetime-2">read_datetime/2</a></td><td>get datetime from PLC.</td></tr><tr><td valign="top"><a href="#read_dm_multi_values-3">read_dm_multi_values/3</a></td><td>read DM values from address list.</td></tr><tr><td valign="top"><a href="#read_dm_values-4">read_dm_values/4</a></td><td>read values from DM area.</td></tr><tr><td valign="top"><a href="#release_alert-3">release_alert/3</a></td><td>release alert in PLC.</td></tr><tr><td valign="top"><a href="#start_port-2">start_port/2</a></td><td>start port server.</td></tr><tr><td valign="top"><a href="#write_dm_same_value-5">write_dm_same_value/5</a></td><td>write same value to DM area.</td></tr><tr><td valign="top"><a href="#write_dm_values-4">write_dm_values/4</a></td><td>write values to DM area.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="clear_alert_history-2"></a>

### clear_alert_history/2 ###


<pre><code>
clear_alert_history(DstIP, Port) -&gt; ok | <a href="#type-send_command_error">send_command_error()</a>
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li></ul>

clear PLC alert list.
<a name="read_alert_history-4"></a>

### read_alert_history/4 ###


<pre><code>
read_alert_history(DstIP, Port, StartRecordNo, Count) -&gt; {ok, [tuple()]} | <a href="#type-send_command_error">send_command_error()</a>
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>StartRecordNo = non_neg_integer()</code></li><li><code>Count = non_neg_integer()</code></li></ul>

read PLC alert list.
<a name="read_datetime-2"></a>

### read_datetime/2 ###


<pre><code>
read_datetime(DstIP, Port) -&gt; {ok, <a href="#type-plc_datetime_and_daynum">plc_datetime_and_daynum()</a>} | <a href="#type-send_command_error">send_command_error()</a>
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ipaddress">inet:ipaddress()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li></ul>


get datetime from PLC.


day_of_week value fit to Erlang calendar:daynum (1..7).
<a name="read_dm_multi_values-3"></a>

### read_dm_multi_values/3 ###


<pre><code>
read_dm_multi_values(DstIP, Port, AddressList) -&gt; {ok, [non_neg_integer()]} | <a href="#type-send_command_error">send_command_error()</a>
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>AddressList = [non_neg_integer()]</code></li></ul>

read DM values from address list.
<a name="read_dm_values-4"></a>

### read_dm_values/4 ###


<pre><code>
read_dm_values(DstIP, Port, StartAddress, Count) -&gt; {ok, [non_neg_integer()]} | <a href="#type-send_command_error">send_command_error()</a>
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>StartAddress = non_neg_integer()</code></li><li><code>Count = non_neg_integer()</code></li></ul>

read values from DM area.
<a name="release_alert-3"></a>

### release_alert/3 ###


<pre><code>
release_alert(DstIP, Port, AlertCodeStr) -&gt; ok | <a href="#type-send_command_error">send_command_error()</a>
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>AlertCodeStr = list() | {non_neg_integer() | non_neg_integer()}</code></li></ul>

release alert in PLC.
<a name="start_port-2"></a>

### start_port/2 ###


<pre><code>
start_port(SrcIP, Port) -&gt; <a href="supervisor.md#type-startchild_ret">supervisor:startchild_ret()</a>
</code></pre>

<ul class="definitions"><li><code>SrcIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li></ul>

start port server.
<a name="write_dm_same_value-5"></a>

### write_dm_same_value/5 ###


<pre><code>
write_dm_same_value(DstIP, Port, StartAddress, Count, Value) -&gt; ok | <a href="#type-send_command_error">send_command_error()</a>
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>StartAddress = non_neg_integer()</code></li><li><code>Count = non_neg_integer()</code></li><li><code>Value = non_neg_integer()</code></li></ul>

write same value to DM area.
<a name="write_dm_values-4"></a>

### write_dm_values/4 ###


<pre><code>
write_dm_values(DstIP, Port, StartAddress, List) -&gt; ok | <a href="#type-send_command_error">send_command_error()</a>
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>StartAddress = non_neg_integer()</code></li><li><code>List = [non_neg_integer()]</code></li></ul>

write values to DM area.
