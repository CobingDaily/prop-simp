module Logic where

data Prop
  = Land Prop Prop
  | Lor Prop Prop
  | Leq Prop Prop
  | Lnot Prop
  | Value Bool
  | Symbol String
  deriving (Show, Eq)

neg :: Prop -> Prop
neg (Land p q) = (Lor (neg p) (neg q))
neg (Lor p q) = (Land (neg p) (neg q))
neg (Leq p q) = (Leq (neg p) q)
neg (Lnot p) = p
neg (Value b) = Value (not b)
neg (Symbol name) = Lnot (Symbol name)

implies :: Prop -> Prop -> Bool
implies p q
  | simplify p == simplify q = True
implies (Lor p q) r = p `implies` r || q `implies` r
implies a (Land b c) = a `implies` b || a `implies` c
implies _ _ = False

trivially_true :: Prop -> Bool
trivially_true (Value True) = True
trivially_true (Land l r) = trivially_true l && trivially_true r
trivially_true (Lor l r) = trivially_true l || trivially_true r
trivially_true (Leq l r) =
  (trivially_true l && trivially_true r) || (trivially_false l && trivially_false l)
trivially_true _ = False

trivially_false :: Prop -> Bool
trivially_false (Value False) = True
trivially_false (Land l r) = trivially_false l || trivially_false r
trivially_false (Lor l r) = trivially_false l && trivially_false r
trivially_false (Leq l r) =
  (trivially_true l && trivially_false r) || (trivially_false l && trivially_true l)
trivially_false _ = False

simplify :: Prop -> Prop
simplify p
  | trivially_true p = Value True
  | trivially_false p = Value False
simplify (Lnot (Lnot p)) = simplify p
simplify (Land (Lnot p) (Lnot q)) = Lnot (Lor (simplify p) (simplify q))
simplify (Lor (Lnot p) (Lnot q)) = Lnot (Land (simplify p) (simplify q))
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
simplify p = p

simplify_loop :: Prop -> Prop
simplify_loop p0 =
  if (p0 == p1) then p1 else simplify_loop p1
  where
    p1 = simplify p0
