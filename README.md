What we need to do for the parser:
+, -, *, /, % 
&&, ||, !
==, !=, <, >, <=. >=
For those seeking an extra challenge: The parser supports nested assignment statements as well as assignments inside expressions. Try writing your interpreter so that assignment operators return a value as well as initialize a variable

You should write a function called interpret that takes a filename, calls parser with the filename, evaluates the parse tree returned by parser, and returns the proper value. You are to maintain a state for the variables and return an error message if the user attempts to use a variable before it is declared. You can use the Scheme function (error ...) to return the error.

Your interpreter needs to return the proper value.  How you achieve this is up to you, and we will later learn the proper way to handle the return.  A simple solution that is sufficient for this project is to have a special variable called return in the state that you assign to the return value.  When you are done interpreting the program your code can then lookup return in the state, get, and return that value.

Your state needs to store binding pairs, but the exact implementation is up to you. I recommend either a list of binding pairs (for example: ((x 5) (y 12) ...) ), or two lists, one with the variables and one with the values (for example: ((x y ...) (5 12 ...))). The first option will be simpler to program, but the second will be more easily adapted for an object-oriented language at the end of the course. The exact way you decide to implement looking up a binding, creating a new binding, or updating an existing binding is up to you. It is not essential that you be efficient here, just do something that works. With such a simple language, an efficient state is unneeded.
