import           Prelude.Unicode
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.MultiIndex

main :: IO ()
main = defaultMain tests

tests ∷ TestTree
tests = testGroup "MultiIndex" [toListTest, fromListTest]

toListTest ∷ TestTree
toListTest =
    testGroup "toList"
              [ testProperty "" (toList Nil ≡ ([] ∷ [Int]))
              , testProperty "" (toList (OneCons Nil) ≡ ([1] ∷ [Int]))
              , testProperty "" (toList (HeadSucc $ OneCons Nil) ≡ ([2] ∷ [Int]))
              , testProperty "" (toList (OneCons $ OneCons Nil) ≡ ([1,1] ∷ [Int]))
              , testProperty "" (toList (OneCons $ HeadSucc $ OneCons Nil) ≡ ([1,2] ∷ [Int]))
              , testProperty "" (toList (HeadSucc $ OneCons $ HeadSucc $ OneCons Nil) ≡ ([2,2] ∷ [Int]))
              ]


fromListTest ∷ TestTree
fromListTest =
    testGroup "fromList"
              [ testProperty "" (fromList ([] ∷ [Int]) ≡ Just Nil) ]
