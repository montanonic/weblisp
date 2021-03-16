# Helpful Compiler

## Compilation Debugging

### Error when source code refers to unbound variable

If variables could be defined at runtime this wouldn't work, but I currently don't see a good argument for runtime-defined variables when macros+maps can be used to emulate the same. Thus, we should be able to do a scope analysis on all unquoted atoms and verify if they are bound in the global or lexical environment, which will help avoid issues with typos.