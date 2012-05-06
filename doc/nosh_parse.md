

#Module nosh_parse#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)






__Version:__ 0.1.4

__<font color="red">To do</font>__
<br></br>
* <font color="red"> Tokenizing</font>
* <font color="red"> [...] Lists</font>
* <font color="red"> [`...`] Lists command substituion</font>
* <font color="red"> (...) second position parameter Lists</font>
* <font color="red"> {...} Tuples</font>
* <font color="red"> `<...>` Pids</font>
* <font color="red"> `<<...>>` Bitstrings</font>
* <font color="red"> Erlang Stack</font>
* <font color="red"> Line continuation</font>

<a name="types"></a>

##Data Types##




###<a name="type-block">block()</a>##



<pre>block() = nonempty_string() | {<a href="#type-context_type">context_type()</a>, [<a href="#type-block">block()</a>]}</pre>



###<a name="type-context_type">context_type()</a>##



<pre>context_type() = {context, <a href="#type-exec_type">exec_type()</a>} | {context, <a href="#type-group_type">group_type()</a>} | {context, <a href="#type-quote_type">quote_type()</a>} | {context, <a href="#type-term_type">term_type()</a>}</pre>



###<a name="type-exec_type">exec_type()</a>##



<pre>exec_type() = brne | line | erln</pre>



###<a name="type-group_type">group_type()</a>##



<pre>group_type() = pren | semi | ifok | ambi | ifnz | pipe</pre>



###<a name="type-io_proc">io_proc()</a>##



<pre>io_proc() = pid()</pre>



###<a name="type-quote_type">quote_type()</a>##



<pre>quote_type() = back | doub | sing | escp | dbcp</pre>



###<a name="type-term_type">term_type()</a>##



<pre>term_type() = word | list | plst | tupl | epid | bstr</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>Parse command line string and return a list of nested quoting and
grouping Stack blocks, or else <code>failed</code> on a caught syntax exception.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="parse-2"></a>

###parse/2##




<pre>parse(Subject::nonempty_string(), Stderr::<a href="#type-io_proc">io_proc()</a>) -> failed | [<a href="#type-block">block()</a>]</pre>
<br></br>






Parse command line string and return a list of nested quoting and
grouping Stack blocks, or else `failed` on a caught syntax exception.

Handle thrown errors for unmatched quoting and grouping characters.