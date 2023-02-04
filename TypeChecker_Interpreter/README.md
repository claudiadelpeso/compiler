# Type checker and Interpreter for a fragment of C++

##Summary 
This includes a type checker and an interpreter for the fragment "C--" of the C++ programming language introduced below.

The type checker checks the program and sends it to the interpreter at success.
The interpreter then runs the program and correctly performs all its input and output actions.
In case type checking fails, a type error is reported. The code passes all the test included in the testsuite folder. 

##Files
- CCM.cf -> Grammar used for type checker and interpreter
- TypeChecker.hs -> Contains type checker code. It returns an annotated syntax abstract tree. 
- Interpreter.hs -> Contains interpreter code. It takes in the annotated syntax abstract tree produced by the type checker and interprets the 
- testsuite -> sample C++ files and a test runner written in Haskell