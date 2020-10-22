{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE TypeApplications #-}

module Test.Chapter4
    ( chapter4
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter4


chapter4 :: Spec
chapter4 = describe "Chapter4" $ do
    chapter4normal
    chapter4advanced

chapter4normal :: Spec
chapter4normal = describe "Chapter4Normal" $ do
    describe "Task2: Functor for Secret" $ do
        let trap = Trap "it's a trap"
        it "doen't affect trap" $
            fmap @(Secret String) @Bool not trap `shouldBe` trap
        it "change reward, same type" $
            fmap @(Secret String) @Bool not (Reward False) `shouldBe` Reward True
        it "change reward, other type" $
            fmap @(Secret String) @Int even (Reward 5) `shouldBe` Reward False
        it "change reward, other type" $
            fmap @(Secret String) @Int even (Reward 4) `shouldBe` Reward True
    describe "Task4: Applicative for Secret" $ do
        let trap :: Secret String Int
            trap = Trap "it's a trap"
        it "pure int" $
            pure @(Secret String) "x" `shouldBe` Reward "x"
        it "pure bool" $
            pure @(Secret String) False `shouldBe` Reward False
        it "trap <*> reward" $
            Trap "it's a trap" <*> Reward 42 `shouldBe` trap
        it "trap <*> trap" $
            Trap "it's a trap" <*> Trap "42" `shouldBe` trap
        it "reward <*> trap" $
            Reward not <*> Trap 42 `shouldBe` Trap 42
        it "reward <*> reward - same type" $
            Reward not <*> Reward True `shouldBe` (Reward False :: Secret String Bool)
        it "reward <*> reward" $
            Reward odd <*> Reward 42 `shouldBe` (Reward False :: Secret String Bool)
    describe "Task5: Applicative for List" $ do
        it "pure int" $ pure 1 `shouldBe` (Cons 1 Empty)
        it "pure bool" $ pure True `shouldBe` (Cons True Empty)
        it "pure false" $ pure False `shouldBe` (Cons False Empty)
        it "Empty append list" $ Empty `append` list `shouldBe` list
        it "list append empty" $ list `append` Empty `shouldBe` list
        it "list append another list" $ list `append` anotherList `shouldBe` sumOfLists
        it "list <*> list" $ fmap (+) list <*> anotherList `shouldBe` (Cons 3 (Cons 21 (Cons 12 (Cons 30 Empty))))
    describe "Task6: Monad for Secret" $ do
        it "Trap" $ (Trap "aaar" >>= halfSecret) `shouldBe` Trap "aaar"
        it "Reward even" $ (Reward 42 >>= halfSecret) `shouldBe` Reward 21
        it "Reward odd" $ (Reward 11 >>= halfSecret) `shouldBe` Trap "it's a trap"
    describe "Task7: Monad for List" $ do
        it "flatten list" $ flatten (Cons(Cons True Empty) (Cons(Cons False Empty) (Cons(Cons True Empty) (Cons(Cons False Empty) Empty)))) `shouldBe` (Cons True (Cons False (Cons True (Cons False Empty))))
        it "Empty" $ (Empty >>= numbers) `shouldBe` Empty
        it "List" $ (listWithNumbers >>= numbers) `shouldBe` listOfNumbers

list :: List Int
list = Cons 1 (Cons 10 Empty)
anotherList :: List Int
anotherList = Cons 2 (Cons 20 Empty)
sumOfLists :: List Int
sumOfLists = Cons 1 (Cons 10 (Cons 2 (Cons 20 Empty)))
listWithNumbers :: List Int
listWithNumbers = Cons 1 (Cons 21 (Cons 321 (Cons 4321 Empty)))
listOfNumbers :: List Int
listOfNumbers = (Cons 1 (Cons 2 (Cons 1 (Cons 3 (Cons 2 (Cons 1 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Empty))))))))))

chapter4advanced :: Spec
chapter4advanced = describe "Chapter4Advanced" $ do
    describe "Task 8*: Before the Final Boss" $ do
        it "Nothing - Nothing" $ andM Nothing Nothing `shouldBe` Nothing
        it "Nothing - Just" $ andM Nothing (Just True) `shouldBe` Nothing
        it "Just True - Nothing" $ andM (Just True) Nothing `shouldBe` Nothing
        it "Just False - Nothing" $ andM (Just False) Nothing `shouldBe` Nothing
        it "Just - Just : False" $ andM (Just True) (Just False) `shouldBe` Just False
        it "Just - Just : True" $ andM (Just True) (Just True) `shouldBe` Just True
        it "True and False for small list" $ (Cons False Empty) `andM` (Cons False(Cons False Empty)) `shouldBe` (Cons False(Cons False Empty))
        it "True and False for list" $ (Cons True (Cons False Empty)) `andM` (Cons False(Cons True Empty)) `shouldBe` (Cons False(Cons True(Cons False (Cons False Empty))))
    
    describe "Task 8*: Before the Final Boss v1" $ do
        it "Nothing - Nothing" $ andMv1 Nothing Nothing `shouldBe` Nothing
        it "Nothing - Just" $ andMv1 Nothing (Just True) `shouldBe` Nothing
        it "Just True - Nothing" $ andMv1 (Just True) Nothing `shouldBe` Nothing
        it "Just False - Nothing" $ andMv1 (Just False) Nothing `shouldBe` Nothing
        it "Just - Just : False" $ andMv1 (Just True) (Just False) `shouldBe` Just False
        it "Just - Just : True" $ andMv1 (Just True) (Just True) `shouldBe` Just True
        it "True and False for small list" $ (Cons False Empty) `andMv1` (Cons False(Cons False Empty)) `shouldBe` (Cons False(Cons False Empty))
        it "True and False for list" $ (Cons True (Cons False Empty)) `andMv1` (Cons False(Cons True Empty)) `shouldBe` (Cons False(Cons True(Cons False (Cons False Empty))))
        
{-  this version has a problem with List. Because of (pure False) in the implementation, it has fewer elements then it should have  
    describe "Task 8*: Before the Final Boss andMWithListProblem" $ do
        it "True and False for small list" $ (Cons False Empty) `andMWithListProblem` (Cons False(Cons False Empty)) `shouldBe` (Cons False(Cons False Empty))
        it "True and False for list" $ (Cons True (Cons False Empty)) `andMWithListProblem` (Cons False(Cons True Empty)) `shouldBe` (Cons False(Cons True(Cons False (Cons False Empty))))
-}
    describe "Task 9: Final Boss" $ do
        it "fmap Tree" $ (*2) `fmap` (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)) `shouldBe` (Node 2 (Node 4 Leaf Leaf) (Node 6 Leaf Leaf))
        it "reverse Tree" $ reverseTree (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)) `shouldBe` (Node 1 (Node 3 Leaf Leaf) (Node 2 Leaf Leaf))
        it "convert Tree to List" $ convertTreeToList (Node 1 (Node 20 (Node 21 Leaf Leaf) (Node 22 Leaf Leaf)) (Node 30 (Node 31 Leaf Leaf) (Node 32 Leaf Leaf))) `shouldBe` (Cons 1(Cons 20 (Cons 21 (Cons 22 (Cons 30 (Cons 31 (Cons 32 Empty)))))))
        it "convert Tree" $ treeToList (Node 1 (Node 20 (Node 21 Leaf Leaf) (Node 22 Leaf Leaf)) (Node 30 (Node 31 Leaf Leaf) (Node 32 Leaf Leaf))) `shouldBe` [1, 20, 21, 22, 30, 31, 32]

halfSecret :: Int -> Secret String Int
halfSecret n
    | even n = Reward (div n 2)
    | otherwise = Trap "it's a trap"
    
numbers :: Int -> List Int
numbers x = go (abs x) Empty
  where
    go :: Int -> List Int -> List Int
    go number l
      | left == 0 = (Cons lastOne l)
      | left < 10 = (Cons left (Cons lastOne l))
      | otherwise = go left (Cons lastOne l)  
      where (left, lastOne) = number `divMod` 10