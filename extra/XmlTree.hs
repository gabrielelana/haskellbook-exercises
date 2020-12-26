{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Protolude (foldM, panic, when)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (Gen, choose, forAll, property)

-- a rose tree
data Tree a = Node a [Tree a] deriving (Eq, Show)

-- left euler tour (a.k.a XML with no text)
data V a = U | D a deriving (Eq, Show)

-- useful for test (and for solution :-))
reverseNode :: Tree a -> Tree a
reverseNode (Node x xs) = Node x $ reverse xs

-- EXERCISE: define the following function (easy)
-- unfoldTree :: Tree a -> [V a]
-- unfoldTree = undefined

unfoldTree :: Tree a -> [V a]
unfoldTree (Node a []) = [D a, U]
unfoldTree (Node a as) = [D a] ++ concat (unfoldTree <$> as) ++ [U]

-- EXERCISE: define the following function (not that easy)
-- foldTree :: [V a] -> Tree a
-- foldTree = undefined

foldTree :: [V a] -> Tree a
foldTree (D a : as) = fst $ foldTree' as (Node a [])
  where foldTree' :: [V a] -> Tree a -> (Tree a, [V a])
        foldTree' [D _] _ = panic "impossible"
        foldTree' (U : as) t = (reverseNode t, as)
        foldTree' (D a : as) (Node n ns) =
          let (t', as') = foldTree' as (Node a []) in
            foldTree' as' $ Node n (t' : ns)

----------------- proof or die  ---------------------

-- generate a tree node
node ::
  Int -> -- depth shrinker
  Int -> -- node name
  Gen (Tree Int, Int) -- the node and the last name used
node l m = do
  n <- choose (0, l)
  (m', fs) <- foldM
    do
      \(m, rs) _ -> do
        (r, m') <- node (l - 1) m
        pure (m', r : rs)
    do (succ m, [])
    do [1 .. n]
  pure (reverseNode $ Node m fs, m')

main :: IO ()
main = hspec do
  describe "unfoldTree" $ do
    it "unroll a tree" $ shouldBe
      do
        unfoldTree do
          Node
            0
            [ Node
                1
                [ Node 2 [],
                  Node 3 [Node 4 []]
                ],
              Node
                5
                [ Node 6 [Node 7 []],
                  Node 8 []
                ],
              Node 9 []
            ]
      do [D 0, D 1, D 2, U, D 3, D 4, U, U, U, D 5, D 6, D 7, U, U, D 8, U, U, D 9, U, U]

  describe "foldTree" $ do
    it "roll up a tree" $ shouldBe
      do foldTree [D 0, D 1, D 2, U, D 3, D 4, U, U, U, D 5, D 6, D 7, U, U, D 8, U, U, D 9, U, U]
      do
        Node
          0
          [ Node
              1
              [ Node 2 [],
                Node 3 [Node 4 []]
              ],
            Node
              5
              [ Node 6 [Node 7 []],
                Node 8 []
              ],
            Node 9 []
          ]

    describe "decodec" $ do
      it "works from random small trees" $
        property $ forAll
          do fst <$> node 3 0
          do \x -> (foldTree . unfoldTree) x == (x :: Tree Int)
      -- !!! set me to True when food is ready!
      when False $
        it "works from random big trees" $
          property $ forAll
            do fst <$> node 10 0
            do \x -> (foldTree . unfoldTree) x == (x :: Tree Int)
