module Main where

import Logic
import Test.Hspec

notNot :: Prop -> Prop
notNot p = Lnot (Lnot p)

main :: IO ()
main = hspec $ do
  describe "simplifyLoop" $ do
    it "Not not p yields p" $
      simplifyLoop (notNot (Symbol "p")) `shouldBe` Symbol "p"
    it "p or True yields True" $
      simplifyLoop (Lor (Symbol "p") (Value True)) `shouldBe` Value True
    it "De Morgan's Law 1: (!p or !q) <=> !(p and q)" $
      simplifyLoop (Lor (Lnot (Symbol "p")) (Lnot (Symbol "q"))) `shouldBe` (Lnot (Land (Symbol "p") (Symbol "q")))
    it "De Morgan's Law 2: (!p and !q) <=> !(p or q)" $
      simplifyLoop (Land (Lnot (Symbol "p")) (Lnot (Symbol "q"))) `shouldBe` (Lnot (Lor (Symbol "p") (Symbol "q")))
    it "(p and q) or (not p or not q) yields True" $
      simplifyLoop (Lor (Land (Symbol "p") (Symbol "q")) (Lor (Lnot (Symbol "p")) (Lnot (Symbol "q")))) `shouldBe` Value True
    it "(p or q) and (not p and not q) yields False" $
      simplifyLoop (Land (Lor (Symbol "p") (Symbol "q")) (Land (Lnot (Symbol "p")) (Lnot (Symbol "q")))) `shouldBe` Value False
    it "p and False yields False" $
      simplifyLoop (Land (Symbol "p") (Value False)) `shouldBe` Value False
    it "p buiried inside Lor" $
      simplifyLoop (Land (Lor (Symbol "q") (Lor (Symbol "r") (Symbol "p"))) (Symbol "p")) `shouldBe` Symbol "p"
    it "Land/Lor swapped, with an inner idempotent Land" $
      simplifyLoop (Lor (Land (Land (Symbol "p") (Symbol "q")) (Symbol "p")) (Symbol "p")) `shouldBe` Symbol "p"
    it "The simplification happens inside a subterm; the outer Lor stays" $
      simplifyLoop (Lor (Land (Lor (Symbol "p") (Symbol "q")) (Symbol "p")) (Symbol "r")) `shouldBe` Lor (Symbol "p") (Symbol "r")
    it "A compound term (p ∧ q) does the absorbing, not just a symbol" $
      simplifyLoop (Land (Lor (Land (Symbol "p") (Symbol "q")) (Symbol "r")) (Land (Symbol "p") (Symbol "q"))) `shouldBe` Land (Symbol "p") (Symbol "q")
    it "Both sides simplify independently" $
      simplifyLoop (Land (Land (Lor (Symbol "p") (Symbol "q")) (Symbol "p")) (Lor (Land (Symbol "r") (Symbol "s")) (Symbol "r"))) `shouldBe` Land (Symbol "p") (Symbol "r")
    it "Nothing to simplify: should come back unchanged" $
      simplifyLoop (Land (Lor (Symbol "p") (Symbol "q")) (Lor (Symbol "r") (Symbol "s"))) `shouldBe` Land (Lor (Symbol "p") (Symbol "q")) (Lor (Symbol "r") (Symbol "s"))
    -- it "(a and (c and d)) or (!a and (c and d)) yields True" $
    --   simplifyLoop () `shouldBe` 

-- !(p and q) and (p or q)
-- !(p or q) or (p and q)
-- (!p and !q) or (p and q)
--
-- !(!p or !q) => (p and q)
-- (!p or !q) and (p and q) <=> False
-- (!p or !q) or (p and q) <=> True
--
-- (a or b) and (c and d) = (a and (c and d)) or (b and (c and d))
-- 1. (a or !a) and (c and d) = (a and (c and d)) or (!a and (c and d))
-- (a or b) and (c and !c) = (a and (c and !c)) or (b and (c and !c))

