The main aim of this section was to develop a salsa language parser, which can be used in specifying animations. The
logics of the grammar language tells me to look first into precedence, perform left factorization and immediate left
recursion then rewrite the grammer then hopefully grammar should appeal as it should. There were however
functions in the simpleparse as well as other monadic parser libraries that serves helpful when used in certain
sections as well as a design choices.
The functions in the parser developed during implemention are listed as follows:
• The colour function: It was implemented using the return the symbol “blue”, “plum”, “red”, “green” and
“orange” respectively.
• The prim function: This was implemented as integer, sident.x, sident.y and expr in braces.
• The integer function: This is implemented as non negative arbituary constants. It can't return negative
values.
• The expr function: The expr function is either prim function or addion or subtractions of expressions and
primitives. This requires precedence, left factorization to generate required parsed grammer. The addition
should have higher precedence.
• The sident and vident functions: These function were implemented as non empty sequence of digits, letter
and underscores however with vident starting with uppercase and sident starting with lowercase. The sident
had to reject reserved keywords given within implementation
• The pos function: This was implemented as two xpressions either separated by braces “(” and “)”, or a
plus“+”with two curly braces”(” “)”.
• The sidents and vidents functions: These were implemented as many either sident or vident respectively
• The command function: This function as specified in the implementation was to have the @ command
function having higher precedence than par command. This section would also if necessary require
rewriting the grammar and left factorizing much or more less.
• The definition function: This should be implemented returning Viewdef, Rectangle, Circle, View, and
Group and results in the grammar.
• The defcom function:The defcom function is supposed to return either definition or command.
• The defcoms function: The defcoms is several definitions or commands or otherwise many defoms.
• The program function: This section of implementation should be able to use different commands and
operations within the application. It should be able to parse certain functions or operations within the
software.
• The parsestring function: This function helps parse string and help generate correspong Error if it cant be
parsed.
• The parseFile function: This would help parse a file if given at a corresponding path..
Design Choices and careful considerations:
The Salsa parser was implemented using SimpleParse inspite of readP and Parsec having more functionality and
addional features. The SimpleParse toolkit was chosen as a preferable ideal monadic parser combinator library for
the given tasks of the implementation.
Minimizing errors: The code was iterated incrementally by first declaring type signatures and labelling each function as
“undefined” . This ensured multiplicity subsequently when having to work independently on followed functions
within the API. It was also useful in isolating errors. The api as a design pattern was strictly followed to and no
changes where made to the abstract syntax tree in ensuring functionality.

In a nutshell development requires a bit of understanding on lexical grammars and parsing with haskell using parser
combinators.

1.2 Assessment
A given suite of HUnit tests (TestSalsaParser.hs) was implemented covering the grammar and separation of tokens
by whitespace. I used simpleparse as a library to develop my parser inspite parsec and readp examples in TA session.
It was helpful eventhough some of the functions in parsec are not included in the simpleparse. The initials stages of
development required a thorough breakdown of the schemantics of the new language implemented using the
grammars given in the documentation. I tried using parsec however after carefully assessing the functionality of the
application decided to use simpleparse.
Assumptions for future implementations
Note: I think it might be helpful for future and students. I've always been using notepad ++ for development however realized
it might not be best tool for parsers in haskell. The lines spaces and tabs if not tuned off causes lots of bug errors only making more or
less the development process less fun and more difficult. It might be best just using basic notepad without all the additional special text
formating( generates plaintext). Either that or another preferable plaintext editor. This I noticed when trying to implement the
definition functions in the grammer(I had to rewrite all the code in the basic notepad before it compiled freely).
I made 115 unit tests that covers the parsing functions for the given Salsa language. All test passes, since each call
gets expected results. The tests are include in TestSalsaParser.hs file (see Appendix A.) and included in the code zip
file.
I preformed the various example programs in the exam set. They worked(successfully parsed) but there were some
small differences. For example the the abstract syntax tree I had parens around given integers not just single digits
but parses when incl isn't a problem since it is semantically the same.
The examples programs included in the assignment were tested by hand and the output compared to the example
ASTs. The manual testing of these were done in ensuring clarity and also just incase of issues posted on course
forum regarding inconsistencies in relation to output described in the question and the example programs.
Test coverage can be improved by using randomized testing with Quick Check. Thus I believe parser developed is
kinder good for the given representation. It run numerious tests operations.