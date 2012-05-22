

#Module ?module#

* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Submodule of [`nosh_parse`](nosh_parse.md).

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.5

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>

* <font color="red"> [...] Lists</font>
* <font color="red"> [`...`] Lists command substituion</font>
* <font color="red"> (...) second position parameter Lists</font>
* <font color="red"> {...} Tuples</font>
* <font color="red"> `<...>` Pids</font>
* <font color="red"> `<<...>>` Bitstrings</font>
<a name="description"></a>

##Description##


_May be refactored back into nosh_parse at a later date._
<a name="types"></a>

##Data Types##




###<a name="type-close_result">close_result()</a>##



	close_result() = {close_context, <a href="#type-context_stack">context_stack()</a>, <a href="#type-symbol_list">symbol_list()</a>}



###<a name="type-context_desc">context_desc()</a>##



	context_desc() = <a href="nosh_parse.md#type-context_desc">nosh_parse:context_desc()</a>



###<a name="type-context_list">context_list()</a>##



	context_list() = <a href="nosh_parse.md#type-context_list">nosh_parse:context_list()</a>



###<a name="type-context_result">context_result()</a>##



	context_result() = {<a href="#type-context_list">context_list()</a>, <a href="#type-context_stack">context_stack()</a>}



###<a name="type-context_stack">context_stack()</a>##



	context_stack() = [<a href="#type-context_desc">context_desc()</a>]



###<a name="type-context_type">context_type()</a>##



	context_type() = <a href="nosh_parse.md#type-context_type">nosh_parse:context_type()</a>



###<a name="type-parse_result">parse_result()</a>##



	parse_result() = <a href="#type-context_result">context_result()</a> | <a href="#type-close_result">close_result()</a>



###<a name="type-symbol">symbol()</a>##



	symbol() = nonempty_string()



###<a name="type-symbol_list">symbol_list()</a>##



	symbol_list() = [<a href="#type-symbol">symbol()</a>]
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close_context-3">close_context/3</a></td><td>Wind up context block.</td></tr><tr><td valign="top"><a href="#parse_context-3">parse_context/3</a></td><td>Unwind context and group stream.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="close_context-3"></a>

###close_context/3##


	close_context(QType::<a href="#type-context_type">context_type()</a>, Stack::<a href="#type-context_stack">context_stack()</a>, List::<a href="#type-symbol_list">symbol_list()</a>) -> <a href="#type-context_result">context_result()</a>
<br></br>


Wind up context block.<a name="parse_context-3"></a>

###parse_context/3##


	parse_context(QType::<a href="#type-context_type">context_type()</a>, Stack::<a href="#type-context_stack">context_stack()</a>, List::<a href="#type-symbol_list">symbol_list()</a>) -> <a href="#type-parse_result">parse_result()</a>
<br></br>


Unwind context and group stream.