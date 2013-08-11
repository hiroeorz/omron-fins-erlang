

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


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#read_alert_history-4">read_alert_history/4</a></td><td>read PLC alert list.</td></tr><tr><td valign="top"><a href="#read_dm_multi_values-3">read_dm_multi_values/3</a></td><td>read DM values from address list.</td></tr><tr><td valign="top"><a href="#read_dm_values-4">read_dm_values/4</a></td><td>read values from DM area.</td></tr><tr><td valign="top"><a href="#start_port-2">start_port/2</a></td><td>start port server.</td></tr><tr><td valign="top"><a href="#write_dm_same_value-5">write_dm_same_value/5</a></td><td>write same value to DM area.</td></tr><tr><td valign="top"><a href="#write_dm_values-4">write_dm_values/4</a></td><td>write values to DM area.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="read_alert_history-4"></a>

### read_alert_history/4 ###


<pre><code>
read_alert_history(DstIP, Port, StartRecordNo, Count) -&gt; {ok, [tuple()]} | {error, timeout} | {error, <a href="#type-omron_fins_error_code">omron_fins_error_code()</a>}
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>StartRecordNo = non_neg_integer()</code></li><li><code>Count = non_neg_integer()</code></li></ul>

read PLC alert list.
<a name="read_dm_multi_values-3"></a>

### read_dm_multi_values/3 ###


<pre><code>
read_dm_multi_values(DstIP, Port, AddressList) -&gt; ok | {error, timeout} | {error, <a href="#type-omron_fins_error_code">omron_fins_error_code()</a>}
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>AddressList = [non_neg_integer()]</code></li></ul>

read DM values from address list.
<a name="read_dm_values-4"></a>

### read_dm_values/4 ###


<pre><code>
read_dm_values(DstIP, Port, StartAddress, Count) -&gt; {ok, term()} | {error, timeout} | {error, <a href="#type-omron_fins_error_code">omron_fins_error_code()</a>}
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>StartAddress = non_neg_integer()</code></li><li><code>Count = non_neg_integer()</code></li></ul>

read values from DM area.
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
write_dm_same_value(DstIP, Port, StartAddress, Count, Value) -&gt; ok | {error, timeout} | {error, <a href="#type-omron_fins_error_code">omron_fins_error_code()</a>}
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>StartAddress = non_neg_integer()</code></li><li><code>Count = non_neg_integer()</code></li><li><code>Value = non_neg_integer()</code></li></ul>

write same value to DM area.
<a name="write_dm_values-4"></a>

### write_dm_values/4 ###


<pre><code>
write_dm_values(DstIP, Port, StartAddress, List) -&gt; ok | {error, timeout} | {error, <a href="#type-omron_fins_error_code">omron_fins_error_code()</a>}
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>StartAddress = non_neg_integer()</code></li><li><code>List = [non_neg_integer()]</code></li></ul>

write values to DM area.
