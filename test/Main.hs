module Main where

import Logic
import Test.Hspec

not_not :: Prop -> Prop
not_not p = Lnot (Lnot p)

main :: IO ()
main = hspec $ do
  describe "simplify_loop" $ do
    it "Not not p yields p" $
      simplify_loop (not_not (Symbol "p")) `shouldBe` Symbol "p"
    it "p or True yields True" $
      simplify_loop (Lor (Symbol "p") (Value True)) `shouldBe` Value True
    it "De Morgan's Law 1: (!p or !q) <=> !(p and q)" $
      simplify_loop (Lor (Lnot (Symbol "p")) (Lnot (Symbol "q"))) `shouldBe` (Lnot (Land (Symbol "p") (Symbol "q")))
    it "De Morgan's Law 2: (!p and !q) <=> !(p or q)" $
      simplify_loop (Land (Lnot (Symbol "p")) (Lnot (Symbol "q"))) `shouldBe` (Lnot (Lor (Symbol "p") (Symbol "q")))
    it "(p and q) or (not p or not q) yields True" $
      simplify_loop (Lor (Land (Symbol "p") (Symbol "q")) (Lor (Lnot (Symbol "p")) (Lnot (Symbol "q")))) `shouldBe` Value True
    it "(p or q) and (not p and not q) yields False" $
      simplify_loop (Land (Lor (Symbol "p") (Symbol "q")) (Land (Lnot (Symbol "p")) (Lnot (Symbol "q")))) `shouldBe` Value False
    it "p and False yields False" $
      simplify_loop (Land (Symbol "p") (Value False)) `shouldBe` Value False
    it "p buiried inside Lor" $
      simplify_loop (Land (Lor (Symbol "q") (Lor (Symbol "r") (Symbol "p"))) (Symbol "p")) `shouldBe` Symbol "p"
    it "Land/Lor swapped, with an inner idempotent Land" $
      simplify_loop (Lor (Land (Land (Symbol "p") (Symbol "q")) (Symbol "p")) (Symbol "p")) `shouldBe` Symbol "p"
    it "The simplification happens inside a subterm; the outer Lor stays" $
      simplify_loop (Lor (Land (Lor (Symbol "p") (Symbol "q")) (Symbol "p")) (Symbol "r")) `shouldBe` Lor (Symbol "p") (Symbol "r")
    it "A compound term (p ∧ q) does the absorbing, not just a symbol" $
      simplify_loop (Land (Lor (Land (Symbol "p") (Symbol "q")) (Symbol "r")) (Land (Symbol "p") (Symbol "q"))) `shouldBe` Land (Symbol "p") (Symbol "q")
    it "Both sides simplify independently" $
      simplify_loop (Land (Land (Lor (Symbol "p") (Symbol "q")) (Symbol "p")) (Lor (Land (Symbol "r") (Symbol "s")) (Symbol "r"))) `shouldBe` Land (Symbol "p") (Symbol "r")
    it "Nothing to simplify: should come back unchanged" $
      simplify_loop (Land (Lor (Symbol "p") (Symbol "q")) (Lor (Symbol "r") (Symbol "s"))) `shouldBe` Land (Lor (Symbol "p") (Symbol "q")) (Lor (Symbol "r") (Symbol "s"))

-- !(p and q) and (p or q)
-- !(p or q) or (p and q)
-- (!p and !q) or (p and q)
