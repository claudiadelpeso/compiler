# Code Generator for C--

## Summary
This is a code generator for a fragment of C-- of the C++ programming language to JVM, the Java Virtual Machine. The code generator produces Java class files, which can be run in the Java bytecode interpreter so that they correctly perform all their input and output actions. The code generator emits Jasmin assembly code into text files, which are then processed further by [Jasmin](#jasmin-tips) to create class files.


##Files 
- CCM.cf -> Grammar used for the code generator.
- TypeChecker.hs -> Contains the code generation code.
- testsuite -> sample C++ files and a test runner written in Haskell