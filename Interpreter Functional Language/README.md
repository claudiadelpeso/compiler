# Interpreter for a functional language

## Summary
This contains an interpreter for a small, untyped functional programming language. This language is a tiny subset of Haskell (the approximate size of the grammar is 15 rules). The interpreter walks through programs and print out the value of the `main` function.

##Files
- Fun.cf -> Grammar for the small fragment of haskell
- Interpreter.hs -> Contains interpreter code for the functional language. It takes as a variable different interpretation types: by value of by name. 
- testsuite -> test files to check interpreter code. It contains small programs that follow the grammar in Fun.cf