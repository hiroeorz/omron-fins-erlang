

# Module omron_fins_port_manager #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`hiroe.orz@gmail.com`](mailto:hiroe.orz@gmail.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delelte_pid-1">delelte_pid/1</a></td><td>delete pid from manager.</td></tr><tr><td valign="top"><a href="#get_pid-1">get_pid/1</a></td><td>get pid to manager by port number.</td></tr><tr><td valign="top"><a href="#set_pid-2">set_pid/2</a></td><td>set new port and pid to manager.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Starts the server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delelte_pid-1"></a>

### delelte_pid/1 ###


<pre><code>
delelte_pid(Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -&gt; ok
</code></pre>
<br />

delete pid from manager.
<a name="get_pid-1"></a>

### get_pid/1 ###


<pre><code>
get_pid(Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -&gt; {ok, pid()} | {error, not_found}
</code></pre>
<br />

get pid to manager by port number.
<a name="set_pid-2"></a>

### set_pid/2 ###


<pre><code>
set_pid(Port::<a href="inet.md#type-port_number">inet:port_number()</a>, Pid::pid()) -&gt; ok
</code></pre>
<br />

set new port and pid to manager.
<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, pid()} | ignore | {error, atom()}
</code></pre>
<br />

Starts the server
