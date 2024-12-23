module Tree

import Data.List


public export
data Tree : Type where
  Atom : String -> Tree
  Node : List Tree -> Tree

public export
treeSize : Tree -> Nat
treeSize (Atom _) = 1
treeSize (Node ts) = 1 + sum (map treeSize ts)

-- public export
-- Show Tree where
--   show (Atom a) = "Atom " ++ show a
--   show (Node ts) = "Node [" ++ concat (intersperse "," (map show ts)) ++ "]"


