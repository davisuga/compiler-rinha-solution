Implement a complete interpreter in Scala 3 using Truffle Language Implementation framework:

Specification
This is the specification of the abstract syntax tree. An abstract syntax tree is a structure designed to be read by a computer expressing a program. For example, if you have a program that says "Add two numbers and show the result", the Abstract Syntactic Tree will show that there is one main action (add two numbers and show the result) and that this action is composed of two parts (add and show). This makes it easier for the computer to understand and execute the program correctly.

A representation of the abstract tree of 1 + 2 would be:

└ Add
  ├── Literal
  │ └── 1
  └── Literal
      └── 2
Or in JSON from the Rinha language

{
  "name": "ata.rinha",
  "expression": {
    "kind": "Binary",
    "lhs": {
      "kind": "Int",
      "value": 1,
      "location": ..
    },
    "op": "Add",
    "rhs": {
      "kind": "Int",
      "value": 2,
      "location": ..
    },
    "location": ..
  },
  "location": ..
}
Where .. is a location node that has been hidden for brevity.

Nodes
File
File is a structure that holds data from the entire file and contains the following fields:

Name Type
name String
expression Term
location Location
Location
Location is a structure that contains fields for locating a piece of the tree within the source code.

Name Type
start Int
end Int
filename String
Parameter
Parameter represents the name of a parameter. It is defined by:

Name Type
text String
location Location
Var (Name of a variable)
Var represents the name of a variable. It is defined by:

Name Type
kind String
text String
location Location
Function (anonymous function)
Function is the creation of an anonymous function that can capture the environment, it is represented by:

Name Type
kind String
parameters [Parameter]
value Term
location Location
Every function when called should give an error if the number of parameters is different from the number of arguments.

Call (Function application)
A call is a function application between a term and several other terms called arguments. This structure is represented by:

Name Type
kind String
callee Term
arguments [Term]
location Location
Let
Let is a structure that represents a let in, that is, in addition to containing a let, it specifies the next structure. Every let can do shadowing, i.e. use the same name as another variable and "hide" the value of the old variable, but this will not be tested.

Name Type
kind String
name Parameter
value Term
next Term
location Location
Hoisting is allowed as a way of enabling the creation of recursive functions.

Str (Text)
Str is a structure that represents a text literal. It is represented by:

Name Type
kind String
value String
location Location
Int (Integer)
Int is a structure that represents a signed integer literal with a size of 32 bits, i.e. an Int32. It is represented by:

Name Type
kind String
value Number
location Location
BinaryOp (Binary Operator)
A BinaryOp is an enumerator that represents a binary operation. These are the available variants:

Name Description Examples that should be valid
Add Sum 3 + 5 = 8, "a" + 2 = "a2", 2 + "a" = "2a", "a" + "b" = "ab"
Subtraction 0 - 1 = -1
Multiplication 2 * 2 = 4
Div Division 3 / 2 = 1
Rem Remainder of division 4 % 2 = 0
Eq Equality "a" == "a", 2 == 1 + 1, true == true
Neq Different "a" != "b", 3 != 1 + 1, true != false
Lt Minor 1 < 2
Gt Larger 2 > 3
Lte Less than or equal to 1 <= 2
Gte Greater or equal 1 >= 2
And Conjunction true && false
Or Disjunction false || true
Overflow will not be tested.

Bool (Boolean)
Bool is a structure that represents a boolean literal. It is represented by:

Name Type
kind String
value Bool
location Location
If
If is a structure that represents an if/else block within the language. It is used to make decisions based on a condition and always returns a value, like a JS ternary. The structure's format is similar to the following example:

The if condition must always be a boolean.

if (true) {
  a;
} else {
  b;
}
Name Type
kind String
condition Term
then Term
otherwise Term
location Location
Binary
Binary is a binary operation between two terms and is represented by:

Name Type
kind String
lhs Term
op BinaryOp
rhs Term
location Location
Tuple (Creating a 2-Tuple)
Tuple is an element that describes the creation of a tuple with the syntax:

(x, y)
It has the following elements:

Name Type
kind String
first Term
second Term
location Location
First (Function to take the first element of a tuple)
First is a function call that takes the first element of a tuple. It is defined by:

first((1, 2))
Name Type
kind String
value Term
location Location
When first is called with something that is not a tuple it should give a runtime error.

Second (Function to take the second element of a tuple)
Second is a function call that takes the second element of a tuple. It is defined by:

second((1, 2))
Name Type
kind String
value Term
location Location

When the second is called with something that is not a tuple, it should give a runtime error.

Print (Print function to standard output)
Print is the call to the print function for standard output. It is defined by:

Examples that should be valid: print(a), print("a"), print(2), print(true), print((1, 2))

Name Type
kind String
value Term
location Location
The values should be printed as:

Type How should be printed
String a string without double quotes ex a
Number the number literal ex 0
Boolean true or false
Closure <#closure>
Tuple (term, term)
Print returns the value that was passed in. The output adds a newline character to the end (LF - 0x0A).

In compound terms, calls to Print must occur in the order in which they appear in the AST. For example:

Code How it should be printed (\n is the LF character)
let _ = print(1); print(2) 1\n2\n
f(print(1), print(2), print(3)) 1\n2\n3\n
let tuple = (print(1), print(2)); print(tuple) 1\n2\n(1, 2)\n
print(print(1) + print(2)) 1\n2\n3\n
Term
A term can be any of the following structures:

Int
Str
Call
Binary
Function
Let
If
Print
First
Second
Bool
Tuple
Var