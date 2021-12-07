# ParserGenerator

A CLR(1) with focus on correctness of theory rather than developer interace.

This parsers aims to allow two main things:

- Implement custom error strategies based on dfa states search at 
  error points. This technique has been proof to provide good 
  error reporting capabilities in the LL range of options. 

- Easy integration with HDayuri, a functional language I'm working on.


## To Do

- Build core of grammar analysis, from surface syntax to dfa.
- Refactor code to improve readability.
- Document code.
- Add macro system to augment BNF to EBNF.
- Add lexer functionality by integrating a regular grammar analysis. 
- Add code generation starting with Haskell and allowing other languages.
