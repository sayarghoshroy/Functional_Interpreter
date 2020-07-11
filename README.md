# Functional Interpreter

### Interpreter for a Functional Arithmetic Language in Haskell

#### Steps to Load & Run

```haskell
ghci
:l ASTParser.hs
:l functional.hs
```

#### Usage Example
```haskell
> ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude> :l ASTParser.hs
[1 of 1] Compiling ASTParser        ( ASTParser.hs, interpreted )
Ok, modules loaded: ASTParser.
*ASTParser> :l functional.hs
[1 of 2] Compiling ASTParser        ( ASTParser.hs, interpreted )
[2 of 2] Compiling Main             ( functional.hs, interpreted )
Ok, modules loaded: ASTParser, Main.
*Main> (run "(assume ((x 7) . (y 3)) (assume ((add (function(x)(+ x y)))) (add 3)))")
NumVal 6
```

---
