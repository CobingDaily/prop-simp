## Prop-Simp

### Examples

#### p buried deeper inside the Lor
```haskell
simplify_loop (Land (Lor (Symbol "q") (Lor (Symbol "r") (Symbol "p"))) (Symbol "p"))
=> Symbol "p"
```
#### Land/Lor swapped, with an inner idempotent Land
```haskell
simplify_loop (Lor (Land (Land (Symbol "p") (Symbol "q")) (Symbol "p")) (Symbol "p"))
=> Symbol "p"
```

#### The simplification happens inside a subterm; the outer Lor stays
```haskell
simplify_loop (Lor (Land (Lor (Symbol "p") (Symbol "q")) (Symbol "p")) (Symbol "r"))
=> Lor (Symbol "p") (Symbol "r")
```

#### A compound term (p ∧ q) does the absorbing, not just a symbol
```haskell
simplify_loop (Land (Lor (Land (Symbol "p") (Symbol "q")) (Symbol "r")) (Land (Symbol "p") (Symbol "q")))
=> Land (Symbol "p") (Symbol "q")
```

#### Both sides simplify independently
```haskell
simplify_loop (Land (Land (Lor (Symbol "p") (Symbol "q")) (Symbol "p")) (Lor (Land (Symbol "r") (Symbol "s")) (Symbol "r")))
=> Land (Symbol "p") (Symbol "r")
```

#### Nothing to simplify: should come back unchanged
```haskell
simplify_loop (Land (Lor (Symbol "p") (Symbol "q")) (Lor (Symbol "r") (Symbol "s")))
=> Land (Lor (Symbol "p") (Symbol "q")) (Lor (Symbol "r") (Symbol "s"))
```

### Todo
- [x] Write tests
- [ ] Parse mathematical notation into `Prop` ast
