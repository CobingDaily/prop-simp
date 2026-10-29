module Main where

main = putStrLn "Hello"

p_and_not_p prop = Land prop (Lnot prop)

p_or_not_p prop = Lor prop (Lnot prop)

data Prop
  = Land Prop Prop
  | Lor Prop Prop
  | Lxor Prop Prop
  | Leq Prop Prop
  | Lnot Prop
  | Value Bool
  | Symbol String
  deriving (Show, Eq)

neg :: Prop -> Prop
neg (Land p q) = (Lor (neg p) (neg q))
neg (Lor p q) = (Land (neg p) (neg q))
neg (Lxor p q) = (Leq p q)
neg (Leq p q) = (Lxor p q)
neg (Lnot p) = p
neg (Value b) = Value (not b)
neg (Symbol name) = Lnot (Symbol name)

implies :: Prop -> Prop -> Bool
implies p q
  | simplify(p) == simplify(q) = True
implies (Lor p q) r = p `implies` r || q `implies` r
implies a (Land b c) = a `implies` b || a `implies` c
implies _ _ = False

simplify :: Prop -> Prop
simplify (Lnot (Lnot p)) = simplify p
simplify (Land l r)
  | l == r = simplify l
  | l == (Lnot r) = Value False
  | r == (Lnot l) = Value False
  | l `implies` r = simplify r
  | r `implies` l = simplify l
  | otherwise = (Land (simplify l) (simplify r))
simplify (Lor l r)
  | l == r = simplify l
  | l == (Lnot r) = Value True
  | r == (Lnot l) = Value True
  | l `implies` r = simplify l
  | r `implies` l = simplify r
  | otherwise = (Lor (simplify l) (simplify r))
simplify (Leq l r)
  | l == r = Value True
  | otherwise = Leq (simplify l) (simplify r)
simplify (Lxor l r)
  | l == r = Value False
  | otherwise = Lxor (simplify l) (simplify r)
simplify p = p

simplify_loop p0 =
  if (p0 == p1) then p1 else simplify_loop p1
    where p1 = simplify p0

-- eval :: Prop -> Bool
-- eval (Land p q) = (eval p) && (eval q)
-- eval (Lor p q) = (eval p) || (eval q)
-- eval (Lxor p q) = (eval p) /= (eval q)
-- eval (Leq p q) = (eval p) == (eval q)
-- eval (Lnot p) = eval (neg p)
-- eval (Value b) = b
