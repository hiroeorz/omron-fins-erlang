

# Module omron_fins_port #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`hiroe.orz@gmail.com`](mailto:hiroe.orz@gmail.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#send_command-3">send_command/3</a></td><td>Send fins command to PLC.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>
Starts the server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="send_command-3"></a>

### send_command/3 ###


<pre><code>
send_command(DstIP, Port, Command) -&gt; ok | {ok, Data} | {error, <a href="inet.md#type-posix">inet:posix()</a>} | <a href="inet.md#type-posix">inet:posix()</a>
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>Command = tuple()</code></li><li><code>Data = term()</code></li></ul>

Send fins command to PLC.
<a name="start_link-2"></a>

### start_link/2 ###


<pre><code>
start_link(Port, SrcIPAddress) -&gt; {ok, pid()} | ignore | {error, atom()}
</code></pre>

<ul class="definitions"><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>SrcIPAddress = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li></ul>


Starts the server
