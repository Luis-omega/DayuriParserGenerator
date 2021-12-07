# ParserGenerator

A CLR(1) with focus on correctness of theory rather than developer interace.

This parsers aims to allow two main things:

- Implement custom error strategies based on dfa states search at 
  error points. This technique has been proof to provide good 
  error reporting capabilities in the LL range of options. 

- Easy integration with HDayuri, a functional language I'm working on.
