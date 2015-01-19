import Control.Applicative ((<$>))
import Test.QuickCheck
-- import Trie

instance Arbitrary (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Trie k v) where
	arbitrary = fromList <$> arbitrary

prop_union_associative :: Trie k a -> Trie k a -> Trie k a -> Bool
prop_union_associative t1 t2 t3 = t1 `union` (t2 `union` t3) == (t1 `union` t2) `union` t3
