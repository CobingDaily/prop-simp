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
