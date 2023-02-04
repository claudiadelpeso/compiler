# Parser for a fragment of C++ using BNF grammar. 

## Summary
The goal was to write a parser for a fragment of the C++ programming language. The parser returns an abstract tree or an error (with line number) if there was a failure. The grammar passes all the tests in the testsuite folder. 

The objective of this lab is to write an LR parser for a fragment of the C++ programming language.
The parser should return an abstract syntax tree at success, and report an error with a line number at failure.

Before the lab can be submitted, the parser has to pass the tests in the included test suite.

The recommended implementation of the parser is via a BNF grammar processed by the BNF Converter (BNFC) tool.
The approximate size of the parser source code should be 100 rules.

With BNFC, just the grammar (.cf file) has to be written and submitted by you.

##Files 
- CC.cf -> This is the LBNF file that will be processed by BNFC, and it only includes the start symbol "Program". It contains the parser code. 
- testsuite -> This folder contains the testsuite for this project, including sample C++ files and a test runner written in Haskell. 
