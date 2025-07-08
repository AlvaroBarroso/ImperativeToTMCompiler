# ImperativeToTMCompiler
A Simple Imperative Language to Turing Machine Compiler in Haskell.

Generate Lexer and Parser using BNFC:
We use the BNF defined in simpleimperativelanguage.cf to generate the lexer and parser for the language.
```bash
bnfc --haskell -d -m -o src grammar/SimpleImperativeLanguage.cf
```