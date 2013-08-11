

# Module omron_fins_driver #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



Driver for OMRON FINS COMMAND.
Copyright (c) (C) 2013, HIROE Shin

__Authors:__ HIROE Shin ([`hiroe.orz@gmail.com`](mailto:hiroe.orz@gmail.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#command-2">command/2</a></td><td>create command data.</td></tr><tr><td valign="top"><a href="#create_header-2">create_header/2</a></td><td>create fins command header.</td></tr><tr><td valign="top"><a href="#create_header-7">create_header/7</a></td><td></td></tr><tr><td valign="top"><a href="#get_process_identifier-1">get_process_identifier/1</a></td><td>get process identifier from response command.</td></tr><tr><td valign="top"><a href="#parse_response-2">parse_response/2</a></td><td>parse response.</td></tr><tr><td valign="top"><a href="#send_command-4">send_command/4</a></td><td>send request to PLC.</td></tr><tr><td valign="top"><a href="#update_header-3">update_header/3</a></td><td>set plc process identifier to header record.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="command-2"></a>

### command/2 ###


<pre><code>
command(Header, Params) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>Header = #fins_header{dst_address = undefined | non_neg_integer(), dst_node = undefined | non_neg_integer(), dst_unit_no = undefined | non_neg_integer(), src_address = undefined | non_neg_integer(), src_node = undefined | non_neg_integer(), src_unit_no = undefined | non_neg_integer(), identifier = undefined | non_neg_integer()}</code></li><li><code>Params = tuple()</code></li></ul>

create command data.
<a name="create_header-2"></a>

### create_header/2 ###


<pre><code>
create_header(DstNode, SrcNode) -&gt; #fins_header{dst_address = undefined | non_neg_integer(), dst_node = undefined | non_neg_integer(), dst_unit_no = undefined | non_neg_integer(), src_address = undefined | non_neg_integer(), src_node = undefined | non_neg_integer(), src_unit_no = undefined | non_neg_integer(), identifier = undefined | non_neg_integer()}
</code></pre>

<ul class="definitions"><li><code>DstNode = non_neg_integer()</code></li><li><code>SrcNode = non_neg_integer()</code></li></ul>

create fins command header.
<a name="create_header-7"></a>

### create_header/7 ###


<pre><code>
create_header(DstNetwork, DstNode, DstUnitNo, SrcNetwork, SrcNode, SrcUnitNo, Identifier) -&gt; #fins_header{dst_address = undefined | non_neg_integer(), dst_node = undefined | non_neg_integer(), dst_unit_no = undefined | non_neg_integer(), src_address = undefined | non_neg_integer(), src_node = undefined | non_neg_integer(), src_unit_no = undefined | non_neg_integer(), identifier = undefined | non_neg_integer()}
</code></pre>

<ul class="definitions"><li><code>DstNetwork = non_neg_integer()</code></li><li><code>DstNode = non_neg_integer()</code></li><li><code>DstUnitNo = non_neg_integer()</code></li><li><code>SrcNetwork = non_neg_integer()</code></li><li><code>SrcNode = non_neg_integer()</code></li><li><code>SrcUnitNo = non_neg_integer()</code></li><li><code>Identifier = non_neg_integer()</code></li></ul>


<a name="get_process_identifier-1"></a>

### get_process_identifier/1 ###


<pre><code>
get_process_identifier(Bin) -&gt; Sid
</code></pre>

<ul class="definitions"><li><code>Bin = binary()</code></li><li><code>Sid = non_neg_integer()</code></li></ul>

get process identifier from response command.
<a name="parse_response-2"></a>

### parse_response/2 ###


<pre><code>
parse_response(IOFacility, Bin) -&gt; ok | {ok, term()} | {error, Reason} | {error, {FinishCode1, FinishCode2}}
</code></pre>

<ul class="definitions"><li><code>IOFacility = non_neg_integer()</code></li><li><code>Bin = binary()</code></li><li><code>Reason = atom()</code></li><li><code>FinishCode1 = non_neg_integer()</code></li><li><code>FinishCode2 = non_neg_integer()</code></li></ul>

parse response.
<a name="send_command-4"></a>

### send_command/4 ###


<pre><code>
send_command(Sock, Host, Port, CommandBin) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Sock = <a href="gen_udp.md#type-socket">gen_udp:socket()</a></code></li><li><code>Host = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>CommandBin = binary()</code></li><li><code>Reason = atom()</code></li></ul>

send request to PLC.
<a name="update_header-3"></a>

### update_header/3 ###


<pre><code>
update_header(DstIPNode::non_neg_integer(), Identifier::non_neg_integer(), Fins_header::#fins_header{dst_address = undefined | non_neg_integer(), dst_node = undefined | non_neg_integer(), dst_unit_no = undefined | non_neg_integer(), src_address = undefined | non_neg_integer(), src_node = undefined | non_neg_integer(), src_unit_no = undefined | non_neg_integer(), identifier = undefined | non_neg_integer()}) -&gt; #fins_header{dst_address = undefined | non_neg_integer(), dst_node = undefined | non_neg_integer(), dst_unit_no = undefined | non_neg_integer(), src_address = undefined | non_neg_integer(), src_node = undefined | non_neg_integer(), src_unit_no = undefined | non_neg_integer(), identifier = undefined | non_neg_integer()}
</code></pre>

<br></br>


set plc process identifier to header record.
