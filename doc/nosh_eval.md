

#Module nosh_eval#
* [Description](#description)


This is a preliminary draft of the command line evaluation module
for `nosh`.



Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.0.0

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>
* <font color="red"> Tilde expansion</font>
* <font color="red"> Reserved words</font>
* <font color="red"> $ - Parameter expansion</font>
* <font color="red"> ${...} - Parameter expansion</font>
* <font color="red"> $(...) - Command substitution</font>
* <font color="red"> $((...)) - Arithmetic Expansion</font>
* <font color="red"> Fileglobs</font>
* <font color="red"> Redirection</font>
* <font color="red"> Here document</font>
<a name="description"></a>

##Description##
 

__Draft Notes:__ In first position, parentheses (`(...)`) group commands within for
execution in a subshell environment, as per command substitution rules.
A parentheses group appearing in second position, following a command,
is treated as an Erlang function parameter List.  Parentheses groups
after second position are invalid, resulting in an evaluation error.