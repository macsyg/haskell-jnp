module Main where
import Test.QuickCheck

infixr 5 :>
data Stream a = a :> (Stream a)

streamToList :: Stream a -> [a]
streamToList (x:>xs) = x : (streamToList xs)

instance Show a => Show (Stream  a) where
  show s = showLoop 20 s

showLoop n (x:>xs)
  | n > 0 = show x ++ "," ++ showLoop (n-1) xs
  | otherwise = "..."


rep :: a -> Stream a
rep a = a :> rep a

-- | Example:
--
-- >>> rep 0
-- 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...


from :: Integer -> Stream Integer
from n = n :> from (n+1)

nats :: Stream Integer
nats = from 0

-- | Example:
--
-- >>> nats
-- 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,...
--

zipStreamsWith :: (a->b->c) -> Stream a -> Stream b -> Stream c
zipStreamsWith f (x:>xs) (y:>ys) = f x y :> zipStreamsWith f xs ys

-- | Example:
--
-- >>> zipStreamsWith (+) nats nats
-- 0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,...
--

instance Functor Stream where
  fmap f (x:>xs) = f x :> fmap f xs 

-- | Example:

-- >>> fmap (+1) nats
-- 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,...
--

instance Applicative Stream where
  pure a  = rep a
  (<*>) (f:>fs) (x:>xs) = (f x) :> (<*>) fs xs -- źle
-- | Example:

-- >>> pure (+1) <*> nats
-- 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,...
--

-- | Should satisfy:

-- - >>> show (pure (*2) <*> nats) == show (fmap (*2) nats)
-- True

newtype Supply s a = S { runSupply :: Stream s -> (a, Stream s) }

get :: Supply s s
get = S getElem -- ??? 

getElem :: Stream s -> (s, Stream s)
getElem (x:>xs) = (x, xs)

-- | Example:
--
-- >>> evalSupply get nats
-- 0
--

pureSupply :: a -> Supply s a
pureSupply a = S (funPure a)

funPure :: a -> Stream s -> (a, Stream s)
funPure a s = (a, s)

-- | Example:
--
-- >>> evalSupply (pure 42) nats
-- 42
--

mapSupply :: (a->b) -> Supply s a -> Supply s b
mapSupply f (S g) = S h where
  h s = (f (fst (g s)), snd (g s))

-- | Example:
--
-- >>> evalSupply (mapSupply (+1) get) nats
-- 1
--

mapSupply2 :: (a->b->c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S ga) (S gb) = S gc where
  gc s = (f (fst (ga s)) (fst (gb (snd (ga s)))), snd (gb (snd (ga s))))

bindSupply :: Supply s a -> (a->Supply s b) -> Supply s b
bindSupply (S fa) k = S fb where
  fb s = runSupply (k (fst (fa s))) (snd (fa s)) -- źle

instance Functor (Supply s) where
  fmap = mapSupply

instance Applicative (Supply s) where
  pure = pureSupply
  (<*>) = mapSupply2 id

instance Monad (Supply s) where
  (>>=) = bindSupply

evalSupply :: Supply s a -> Stream s -> a
evalSupply p s = fst $ runSupply p s

-- evalSupply2 :: Supply s a -> Stream s -> Stream s
-- evalSupply2 p s = snd $ runSupply p s

-- | Example:
--
-- >>> evalSupply (get >> get >> get) nats
-- 2
--

data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving (Eq, Show)

size :: Tree a -> Int
size (Leaf _) = 1
size (Branch l r) = size l + size r

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Branch l r) = toList l ++ toList r

labelTree :: Tree a -> Tree Integer
labelTree t = evalSupply (go t) nats
  where
    go :: Tree a -> Supply s (Tree s)
    go t = S (relabelTree t)

relabelTree :: Tree a -> Stream s -> (Tree s, Stream s)
relabelTree (Branch tl tr) s =  (tRelabel, streamFinal) where
    (tlRelabel, stream1) = relabelTree tl s
    (trRelabel, stream2) = relabelTree tr stream1
    (tRelabel, streamFinal) = (Branch tlRelabel trRelabel, stream2)
relabelTree (Leaf x) (sHead:>sTail) = (Leaf sHead, sTail)

-- | Example:
--
-- >>> labelTree $ Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))
-- Branch (Leaf 0) (Branch (Leaf 1) (Leaf 2))
--


-- Hic sunt leones

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized tree'
    where tree' n | n <= 0 = Leaf <$> arbitrary
          tree' n =
                    oneof [Leaf <$> arbitrary,
                           Branch <$> subtree <*> subtree]
                    where subtree = tree' (n `div` 2)
  shrink (Leaf _) = []
  shrink (Branch l r) = [l, r] ++ [Branch l' r' | (l', r') <- shrink (l, r)]


prop_sizeLabelTree :: Tree Integer -> Bool
prop_sizeLabelTree t = size (labelTree t) == size t

prop_labelTree :: Tree Integer -> Bool
prop_labelTree t = toList (labelTree t) == [0..n]
    where n = fromIntegral $ size t - 1

prop_labelTreeIdempotent :: Tree Integer -> Bool
prop_labelTreeIdempotent t = labelTree (labelTree t) == labelTree t

writeln = putStrLn

main = do
  writeln "prop_sizeLabelTree"
  quickCheck prop_sizeLabelTree
  writeln "prop_LabelTree"
  quickCheck prop_labelTree
  writeln "prop_LabelTreeIdempotent"
  quickCheck prop_labelTreeIdempotent