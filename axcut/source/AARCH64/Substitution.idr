module AARCH64.Substitution

import AARCH64.Code

import Data.List
import Data.Fin
import Data.Vect


data Tree : Type where
  Back : Tree
  Node : Register -> List Tree -> Tree

data Root : Type where
  Start : Register -> List Tree -> Root

treeNodes : Tree -> List Register
treeNodes Back = []
treeNodes (Node x ts) = x :: concatMap treeNodes ts

refersBack : Tree -> Bool
refersBack Back = True
refersBack (Node _ xs) = any refersBack xs

visitedBy : Root -> List Register
visitedBy (Start x ts) = if any refersBack ts
  then x :: concatMap treeNodes ts
  else concatMap treeNodes ts

deleteTargets : List Register -> Vect 31 (List Register) -> Vect 31 (List Register)
deleteTargets visited graph = map (filter (\y => all (/= y) visited)) graph

deleteAll : Register -> List Register -> List Register
deleteAll x ys = filter (/= x) ys

spanningTree : Vect 31 (List Register) -> Register -> Register -> Tree
spanningTree graph root node = if root == node
  then Back
  else Node node (map (spanningTree graph root) (index node graph))

spanningForest : Vect 31 (List Register) -> Vect n Register -> Vect n Root
spanningForest graph [] = []
spanningForest graph (x :: xs) = do
  let root = Start x (map (spanningTree graph x) (deleteAll x (index x graph)))
  root :: spanningForest (deleteTargets (visitedBy root) graph) xs

treeMoves : Register -> Tree -> List Code
treeMoves x Back = MOVR temp x :: []
treeMoves x (Node y ts) = MOVR y x :: concatMap (treeMoves y) ts

rootMoves : Root -> List Code
rootMoves (Start x xs) = if any refersBack xs
  then reverse (MOVR x temp :: concatMap (treeMoves x) xs)
  else reverse (concatMap (treeMoves x) xs)

public export
codeExchange : Vect 31 (List Register) -> List Code
codeExchange graph = concatMap rootMoves (spanningForest graph range)


