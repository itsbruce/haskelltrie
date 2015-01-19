{- |
Module      : $Header$
Description : A Trie alternative to Data.Map
Copyright   : (c) Bruce Richardson
License     : Hmmm
Maintainer  : itsbruce@workshy.org
Stability   : unstable
Portability : portable

A Trie that can be used as a drop-in replacement for Data.Map
 -}
module Trie (
  Trie,
  (!),
  null,
  size,
  empty,
  singleton,
  member,
  notMember,
  lookup,
  fromList,
  assocs,
  toList,
  toAscList,
  toDescList,
  insert,
  delete,
  foldr,
  foldl,
  elems,
  elemAt,
  map,
  filter,
  union,
  valid,
) where

import Prelude hiding (filter,foldl,foldr,lookup,map,null)
import qualified Prelude (filter,foldl,foldr,lookup,map,null)
import qualified Data.Foldable as Foldable
import Control.Monad
import Data.Monoid

-- data Empty
-- data Trie k a = Empty | Leaf a | Node (Maybe a) !(ChildMap k a) deriving (Show,Read)
data Trie k a = Empty | Leaf a | Node (Maybe a) (ChildMap k a) deriving (Show,Read)

instance (Eq k, Eq a) => Eq (Trie k a) where
  x == y = toAscList x == toAscList y

instance Functor (Trie k) where
  fmap = map

instance Foldable.Foldable (Trie k) where
  foldr = foldr
  foldl = foldl
  -- foldMap f Empty = mempty
  -- foldMap f (Leaf x) = f x

instance (Ord k) => Monoid (Trie k a) where
  mempty = empty
  mappend = union
--  mconcat = unions

-- ChildMap type and functions
--
type ChildMap k a = [(k, Trie k a)]
type Child k a = (k, Trie k a)

childless :: ChildMap k a
childless = []

childMap :: Trie k a -> ChildMap k a
childMap t = case t of
              Node _ cs -> cs
              _ -> childless

childFromMap :: Eq k => k -> ChildMap k a -> Trie k a
childFromMap k cs = maybe Empty id $ Prelude.lookup k cs

deleteFromMap :: Eq k => k -> ChildMap k a -> ChildMap k a
deleteFromMap k = Prelude.filter ((/= k) . fst)

childTrie :: Eq k => k -> Trie k a -> Trie k a
childTrie k = childFromMap k . childMap

add2Map :: Ord k => k -> Trie k a -> ChildMap k a -> ChildMap k a
add2Map k Empty [] = []
add2Map k t [] = [(k, t)]
add2Map k Empty cs = deleteFromMap k cs
-- add2Map k t cs = (Prelude.filter ((< k) . fst) cs) ++ ((k, t) : (Prelude.filter ((> k) . fst) cs))
add2Map k t cm@((k0,t0):cs)
  | k < k0 = (k, t) : cm
  | k == k0 = (k, t) : cs
  | otherwise = (k0, t0) : add2Map k t cs

-- Node Helper Functions

node :: Maybe a -> ChildMap k a -> Trie k a
node Nothing [] = Empty
node (Just x) [] = Leaf x
node m cs = Node m cs

nodeValue :: Trie k a -> Maybe a
nodeValue t = case t of
                Empty -> Nothing
                Leaf x -> Just x
                Node m _ -> m

asLeaf :: Trie k a -> Trie k a
asLeaf t = case t of
            Leaf _ -> t
            Node (Just x) _ -> Leaf x
            _ -> Empty

-- Context types and functions
--
type Context k a = [(k, Maybe a, [Child k a], [Child k a])]
--type Context k a = [(k, Trie k a)]
type Location k a = (Trie k a, Context k a)

cTop :: Trie k a -> Location k a
cTop t = (t, [])

cAscend :: Location k a -> Location k a
cAscend l = case l of
              (Empty, (_, m, l, r):cxt) -> (node m (l ++ r), cxt)
              (t, (k, m, l, r):cxt) -> (node m (l ++ ((k,t):r)), cxt)

cDescend :: Ord k => k -> Location k a -> Location k a
cDescend k (Node m cm, cxt) =
    let (left, notLeft) = span ((<k) . fst) cm
        (match, right) = span ((==k) . fst) notLeft
        t = childFromMap k match
    in (t, (k, m, left, right):cxt)

cFirstDown :: Location k a -> Location k a
cFirstDown (Node m ((k, t):cs), cxt) = (t, (k, m, [], cs):cxt)

cToTop :: Location k a -> Location k a
cToTop l = case l of
            (t, []) -> l
            _       -> cToTop $ cAscend l

cUpdate :: (Trie k a -> Trie k a) -> Location k a -> Location k a
cUpdate f (t, c) = (f t, c)

foldrLoc :: (Location k a -> a -> b -> b) -> b -> Location k a -> b
foldrLoc f x l = case l of
  (Empty, _) -> x
  (Leaf y, _) -> f l y x
  (Node Nothing [], _) -> x
  (Node (Just y) cs, cxt) -> f l y $ foldrLoc f x (Node Nothing cs, cxt) -- node/Node?
  (Node Nothing ((k, t):cs), cxt) ->
    let x' = foldrLoc f x (Node Nothing cs,cxt)
    in foldrLoc f x' $ cFirstDown l

foldlLoc :: (Location k b -> a -> b -> a) -> a -> Location k b -> a
foldlLoc f x l = case l of
  (Empty, _) -> x
  (Leaf y, _) -> f l x y
  (Node Nothing [], _) -> x
  (Node (Just y) cs, cxt) -> foldlLoc f (f l x y) (Node Nothing cs, cxt)
  (Node Nothing ((k, t):cs), cxt) ->
    let x' = foldlLoc f x $ cFirstDown l
    in foldlLoc f x' (Node Nothing cs, cxt)

keyFromLoc :: Location k a -> [k]
keyFromLoc = reverse . Prelude.map fst . snd
  where fst (k, _, _, _) = k

-- Helper Functions

modError :: String -> a
modError = error . ("Trie: " ++)

fnode :: Eq k => (Trie k a -> b) -> [k] -> Trie k a -> b
fnode f [] t = f t
fnode f (k:ks) (Node _ cs) = fnode f ks $ childFromMap k cs
fnode f _ _ = f Empty

-- Exported Functions

infixl 9 !
-- | Retrieve a value by its key.  Throws an error if there is
-- no such key.
(!) :: Eq k => Trie k a -> [k] -> a
t ! ks = let err = modError "element not in the trie"
         in maybe err id $ fnode nodeValue ks t

-- |Does the trie contain any elements?
null :: Trie k a -> Bool
null = foldr (\x b -> False) True

-- |The number of elements in the trie.
size :: Trie k a -> Int
size = foldr (\t x -> succ x) 0

-- |Returns an empty trie.
empty :: Trie k a
empty = Empty

-- |Returns a trie containing only the given key and value.
singleton :: [k] -> a -> Trie k a
singleton [] a = Leaf a
singleton (k:ks) a = Node Nothing [(k, singleton ks a)]

-- |Is the key a member of the trie?  See also 'notMember'
member :: Eq k => [k] -> Trie k a -> Bool
member = fnode f
    where f Empty = False
          f (Node Nothing _) = False
          f _ = True

-- |Is the key not a member? See also 'member'
notMember :: Eq k => [k] -> Trie k a -> Bool
notMember ks = not . member ks

-- |Retrieve a value by its key. Returns either @(Just 'value')@
-- or 'Nothing'.
lookup :: Eq k => [k] -> Trie k a -> Maybe a
lookup = fnode nodeValue

-- May go to Ord
findWithDefault :: Eq k => a -> [k] -> Trie k a -> a
findWithDefault d ks t = maybe d id $ lookup ks t

-- |Creates a trie from the key/value pairs in the list.  Where there multiple
-- values for the same key, the final value will remain.
fromList :: Ord k => [([k], a)] -> Trie k a
fromList = Prelude.foldl (\t (ks, a) -> insert ks a t) Empty -- foldl' ??
-- fromList = Prelude.foldr (\(ks, a) t -> insert ks a t) Empty -- First one wins

-- |Returns a list of the trie key/value pairs. See also 'toAscList'
assocs :: Trie k a -> [([k], a)]
assocs = toAscList

-- |Returns a list of the trie key/value pairs. See also 'toAscList'
toList :: Trie k a -> [([k], a)]
toList = toAscList

-- |Returns a list of all trie key/value pairs in ascending order
-- of their keys
toAscList :: Trie k a -> [([k], a)]
toAscList = foldrWithKey (\ks a b -> (ks, a) : b) []

-- |Returns a list of all trie key/value pairs in descending order
-- of their keys
toDescList :: Trie k a -> [([k], a)]
toDescList = foldlWithKey (\ks a b -> (ks, b) : a) []

-- |Inserts a key and value into the trie, replacing any previous value (if
-- there was one).
insert :: Ord k => [k] -> a -> Trie k a -> Trie k a
insert [] x (Node _ cs) = node (Just x) cs
insert [] x _ = Leaf x
insert ks x Empty = insert ks x $ Node Nothing childless
insert ks x (Leaf y) = insert ks x (Node (Just y) childless)
insert (k:ks) x (Node m cs) =
    let t = insert ks x $ childFromMap k cs
    in Node m (add2Map k t cs)

-- |Removes the value associated with the given key
delete :: Ord k => [k] -> Trie k a -> Trie k a
delete []  (Node _ cs) = node Nothing cs
delete [] _ = Empty
delete (k:ks) (Node m cs) =
    let t = delete ks $ childFromMap k cs
    in node m $ add2Map k t cs
delete ks t = t

-- |If the trie contains a value under the given key, updates the value by
-- applying the function.
adjust :: Ord k => (a -> a) -> [k] -> Trie k a -> Trie k a
adjust f [] Empty = Empty
adjust f [] (Leaf x) = Leaf $ f x
adjust f [] (Node m cs) = node (fmap f m) cs
adjust f (k:ks) (Node m cs) =
    let adjustChild c@(x, t) | x == k = (x, adjust f ks t)
                 | otherwise = c
    in Node m $ Prelude.map adjustChild cs

{------------------------------------------------------------------
  Folds
------------------------------------------------------------------}

-- |Fold the values in the trie using a right-associative binary operator.
-- Equivalent to 'Prelude.foldr f z . elems'
foldr :: (a -> b -> b) -> b -> Trie k a -> b
foldr f x t = case t of
                Empty -> x
                Leaf y -> f y x
                Node Nothing [] -> x
                Node (Just y) cs -> f y . foldr f x $ Node Nothing cs
                Node Nothing ((k,t):cs) ->
                  let x' = foldr f x (Node Nothing cs)
                  in foldr f x' t

-- |Fold the values in the trie using a left-associative binary operator.
-- Equivalent to 'Prelude.foldl f z . elems'
foldl :: (a -> b -> a) -> a -> Trie k b -> a
foldl f x t = case t of
                Empty -> x
                Leaf y -> f x y
                Node Nothing [] -> x
                Node (Just y) cs -> foldl f (f x y) $ Node Nothing cs
                Node Nothing ((k,t):cs) ->
                  let x' = foldl f x t
                  in foldl f x' (Node Nothing cs)

foldrWithKey :: ([k] -> a -> b -> b) -> b -> Trie k a -> b
foldrWithKey f b t = foldrLoc (f . keyFromLoc) b $ cTop t

foldlWithKey :: ([k] -> a -> b -> a) -> a -> Trie k b -> a
foldlWithKey f a t = foldlLoc (f . keyFromLoc) a $ cTop t

-- Returns a list of all the trie elements in ascending order of their keys
elems :: Trie k a -> [a]
elems = foldr (:) []

-- Returns a list of all keys in ascending order
keys :: Trie k a -> [[k]]
keys = foldrLoc (\l a b -> keyFromLoc l : b) [] . cTop

elemAt :: Int -> Trie k a -> ([k], a)
elemAt i t = toAscList t !! i

-- |Returns a new trie that is the result of applying a given function to all
-- elements
map :: (a -> b) -> Trie k a -> Trie k b
map f t = case t of
            Empty -> Empty
            Leaf x -> Leaf $ f x
            Node m cs -> node (fmap f m) [(k, map f t') | (k, t') <- cs]

{-
mapWithKey :: ([k] -> a -> b) -> Trie k a -> Trie k b
mapWithKey f Empty = Empty
mapWithKey f (Leaf x) = Leaf $ f [] x
-}

-- |Returns a trie that only contains elements matching the given predicate
filter :: (a -> Bool) -> Trie k a -> Trie k a
filter f t = case t of
  Empty -> Empty
  Leaf x -> if f x then t else Empty
  Node m [] -> maybe Empty Leaf m
  Node m cs ->
    let fCS = [(k, t'') | (k, t') <- cs, let t'' = filter f t', not (null t'')]
    in node (mfilter f m) fCS

-- |Combines the elements of two trees to create a third that is the union of
-- both.  The operation is left-biased, so that if both trees have a value
-- stored for the same key, the first is kept.
union :: Ord k => Trie k a -> Trie k a -> Trie k a
union t Empty = t
union Empty t = t
union t@(Leaf x) (Leaf _) = t
union (Leaf x) (Node m cs) = node (Just x) cs
union (Node m cs) (Leaf x) = Node (msum [m, Just x]) cs -- node?
union (Node m1 cs1) (Node m2 cs2) =
    Node (msum [m1, m2]) $ unify cs1 cs2
    where
        unify [] cs = cs
        unify cs [] = cs
        unify l1@(c1@(k1,t1):cs1) l2@(c2@(k2,t2):cs2)
            | k1 < k2 = c1 : unify cs1 l2
            | k1 > k2 = c2 : unify l1 cs2
            | otherwise = (k1, t1 `union` t2) : unify cs1 cs2

-- |Produces the cumulative, left-associative union of a list of tries.
unions :: Ord k => [Trie k a] -> Trie k a
unions = Prelude.foldr union Empty

{------------------------------------------------------------------
  Assertions
------------------------------------------------------------------}

valid :: Ord k => Trie k a -> Bool
valid t = ordered t && lintFree t

ordered :: Ord k => Trie k a -> Bool
ordered (Node _ cs) = ord cs && and [ordered t | (_, t) <- cs]
    where ord (x:y:ys) | fst x < fst y = ord (y:ys)
                       | otherwise = False
          ord _ = True
ordered _ = True

lintFree :: Trie k a -> Bool
lintFree Empty = True
lintFree t = lf t
    where lf Empty = False
          lf (Leaf _) = True
          lf (Node _ []) = False
          lf (Node _ cs) = and [lf t' | (_, t') <- cs]
