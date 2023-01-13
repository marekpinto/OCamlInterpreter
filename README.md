# OCamlInterpreter
Interpreter for a simple stack-based programming language written from scratch in OCaml. Written in order to practice working on a larger project in OCaml and understand the concepts going into building an interpreter for a new programming language. 

Syntax for the program code to be successfully interpreted:

Each command should be separated by a new line. These commands will manipulate a stack built into the programming language. When the "Quit" command is reached, the program terminates and either the resulting stack or the word "Error" is written to a file passed into the interpreter.

List of Commands:

1. Quit
This command causes the interpreter to stop and does not have to be on the last line of the program. When
called, the created stack should be written out to an output file that is specified as the second argument to the
main function, and no commands after the Quit call get evaluated.

2. Push <const>
Pushes the argument to the stack, as long as it is a valid argument.
  
3. Pop
Removes top argument from the stack. If the stack is empty, returns an error.
  
4. Add
Add consumes the top 2 values in the stack, and pushes their sum to the stack. If there are fewer than 2 values
on the stack, terminate with error. If not all of the top 2 values on the stack are not integers, terminate with error.
  
5. Sub
Sub consumes the top 2 values on the stack, and pushes the difference between the top value and the second
top value to the stack.
If there are fewer than 2 values on the stack, terminate with error. If the top 2 values on the stack are
not integers, terminate with error.
  
6. Mul
Mul consumes the top 2 values in the stack, and pushes their product to the stack.
If there are fewer than 2 values on the stack, terminate with error. If the top 2 values on the stack are
not integers, terminate with error.

7. Div
Div consumes the top 2 values on the stack, and pushes the quotient between the top value and the second top
value onto the stack.
If there are fewer than 2 values on the stack, terminate with error. If the second value is 0, terminate with
error. If the top 2 values on the stack are not integers, terminate with error.
  
8. Swap
Swap takes the top 2 elements in the stack and swaps their order. If there are less than 2 items in the stack,
terminate with error

9. Neg
Neg would negate the top element on the stack. If the top element in the stack is not an integer, terminate with
error. If the stack is empty, terminate with error.

10. Concat
Concat consumes the top 2 values on the stack, and pushes the concatenation between the top value and the
second top value onto the stack.
If there are fewer than 2 values in the stack, terminate with error. If the top 2 elements in the stack are
not of string type, then terminate with error.
  
11. Add
And consumes the two 2 values in the stack and pushes their conjunction to the stack. If there are fewer then
2 values on the stack, terminate with error. If the 2 top values in the stack are not booleans, terminate with error.
  
12. Or
Or consumes the top 2 values in the stack and pushes their disjunction to the stack. If there are fewer than
2 values on the stack, terminate with error. If the 2 top values in the stack are not booleans, terminate with error.

13. Not
Not consumes the top value of the stack and pushes its negation to the stack. If the stack is empty, terminate
with error. If the top value on the stack is not a boolean, terminate with error.
  
14. Equal
Equal consumes the top 2 values in the stack and pushes true to the stack if they are equal integers and false
if they are not equal integers. If there are fewer than 2 values on the stack, terminate with error. If the 2 top
values in the stack are not integers, terminate with error.

15. Lte
Lte consumes the top 2 integer values in the stack and pushes true on the stack if the top value is less than or
equal to the second top value. If there are fewer than 2 values on the stack, terminate with error. If the 2 top
values in the stack are not integers, terminate with error.

16. Local
Local comes with a name argument and consumes the top element of the stack, which can be any value, and
relates them in the current local environment. If the name is already assigned a value in the current local environment, overwrite it. Push <name> should push the value paired with that name in the current environment
on to the stack.
If the stack is empty, terminate with error. If a name that is not currently in the environment is used with
Push, terminate with error.

17. Global
Global comes with a name argument and consumes the top element of the stack, which can be any value, and
relates them in the global environment until the end of the program. If the name is already assigned a value
in the global environment, overwrite it. Push <name> should push the value paired with that name in the
environment on to the stack.
If the stack is empty, terminate with error. If a name that is not currently in the environment is used with
Push, terminate with error.
  
18. Begin/End (Creates a new nested scope)
A sequence of commands in a Begin/End block will be executed on a new empty stack with a copy of the current
binding scope. When the commands finish, the top value from the stack will be pushed to the outer stack, and
new local bindings made from within the block disregarded. Glocal bindings made from within the block are
valid for the rest of the program.
  
Ex: 
  
Push 55
Local x
Push x
Begin
Push 3
Push 5
Global x
Push 7
Push x
End
Push x
Quit

Should result in the following output file:
55
55
55

19. If-Then-Else
The IfThen/Else command will consume the top element of the stack. If that element is true it will execute
the commands in the then branch, otherwise if false it will execute the commands in the else branch. In both
cases, the remaining stack is used directly for executing the commands in corresponding branch. The resulting
stack after evaluating the correct branch is used to evaluate the rest of the program.
If stack is empty, terminate with error. If the top value on the stack is not a boolean, terminate with error.
  
Ex: 
  
Push 10
Push 1
IfThen
Push 5
Add
Else
Push 5
Sub
End
Quit

Should result in the following output file:
15

20. injL/injR
InjL (respectively, InjR) consumes the top value of the stack and pushes a union value to the stack. If the stack
is empty, terminate with error.
  
Ex:
  
Push 5
InjL
Quit
  
Should result in the following output file:
Left 5
  
21. CaseLeft/Right
CaseLeft/Right consumes the top element of the stack. If the element is a left union, it will execute the
commands in the first branch with the contents of the left pushed to the stack; otherwise, if the element is a
right, it will execute the commands in the second branch with the contents of the right pushed to the stack.
In both cases, the remaining stack is used directly for executing the commands in corresponding branch. The
resulting stack after evaluating the correct branch is used to evaluate the rest of the program.
If the stack is empty, terminate with error. If the top value on the stack is not a union, terminate with error.

Ex:
  
Push 5
InjL
CaseLeft
Push 1
Add
25
Right
Push "Bob"
Concat
End
Quit
  
Should result in the following output file:
6
  
22. Tuple
Tuple followed by an integer n consumes the top n values from the stack, places them in a tuple, and pushes
the tuple to the stack. The 1st, 2nd, . . . , nth values from the top of the stack will be put in the n-tuple in the
order: (nth, . . . , 2nd, 1st).
If there are fewer than n values on the stack, terminate with error. If n is a negative number, terminate
with error.

Ex:
  
Push 1
Push "two"
Push 3
Tuple 3
Quit

Should result in the following output file:
(1, "two", 3)
  
23. Get
Get followed by an integer n takes (not consume) the top values from the stack and pushes the nth element of
the tuple to the stack. Tuples are zero-indexed, so the first element is 0, the second is 1, etc.
If the stack is empty, terminate with error. If the top value on the stack is not a tuple, terminate with error.
If n is out of bounds of the tuple, terminate with error.

 24. Functions
Functions are declared with the fun command:
  
Fun fname arg
prog
{ Mut fname arg
prog }
End

Here, fname is the name of the function and arg is the name of the parameter to the function. prog are the
commands that get executed when the function is called.
After a function is defined with the Fun command, a new closure is formed and bound to the functions name
in the local environment. Each fname, arg, and prog in the mutually recursive functions with all local bindings
in the current environment will be added to the new closure.
  
Ex:
  
Fun foo my_arg
End
Push foo
Quit
  
Should result in the following output file:
Clo (foo my_arg)

 25. Call
 Call consumes a closure and an argument value from the top of the stack. It then executes the active code
inside the closure with a new stack, current global environment, and the closure’s local environment extended
with the following binds:
• the functions’ formal parameter to the argument value received by Call
• all mutually declared function names to their corresponding closures
The active prog contained within the closure is executed using this newly formed environment and a fresh
stack. As with Begin/End, all global bindings made within the called closure are valid after the closure returns
and also within all of its recursive calls. But all the local bindings and stack made within the called closure are
not valid after the closure returns and not valid within recursive calls.
Call then pushes the returned value (if the call terminates) from the closure onto the stack.

26. Return
Return consumes the top value of the stack and returns it as the result of a closure.
If the stack is empty, terminate with error. If not inside a closure, terminate with an error. If the end of a
function is reached with no return, terminate with an error.
  
The allowed data types for a const are:

- Integers
- Strings
- Names (subset of strings that correspond to variable names)
- Booleans
- Unions (represent options, either Left or Right)
- Tuples (collections of items)
- Closures (commands paired with a local environment, represent functions)
