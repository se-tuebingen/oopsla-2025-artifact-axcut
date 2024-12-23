module X86_64.Substitution

import X86_64.Code

import Data.List
import Data.Fin
import Data.Vect


data Tree : Type where
  Back : Tree
  Node : Temporary -> List Tree -> Tree

data Root : Type where
  Start : Temporary -> List Tree -> Root

treeNodes : Tree -> List Temporary
treeNodes Back = []
treeNodes (Node x ts) = x :: concatMap treeNodes ts

refersBack : Tree -> Bool
refersBack Back = True
refersBack (Node _ xs) = any refersBack xs

IsSpill : Type
IsSpill = Bool

mutual
  spillEdgeSpill : IsSpill -> Tree -> Bool
  spillEdgeSpill rootIsSpill Back = rootIsSpill
  spillEdgeSpill rootIsSpill (Node (Left register) xs) = any (spillEdgeRegister rootIsSpill) xs
  spillEdgeSpill _ (Node (Right spill) xs) = True

  spillEdgeRegister : IsSpill -> Tree -> Bool
  spillEdgeRegister _ Back = False
  spillEdgeRegister rootIsSpill (Node (Left register) xs) = any (spillEdgeRegister rootIsSpill) xs
  spillEdgeRegister rootIsSpill (Node (Right spill) xs) = any (spillEdgeSpill rootIsSpill) xs

containsSpillEdge : Root -> Bool
containsSpillEdge (Start (Left register) ts) = any (spillEdgeRegister False) ts
containsSpillEdge (Start (Right spill) ts) = any (spillEdgeSpill True) ts

visitedBy : Root -> List Temporary
visitedBy (Start x ts) = if any refersBack ts
  then x :: concatMap treeNodes ts
  else concatMap treeNodes ts

deleteTargets : List Temporary -> Vect (RegisterNum + SpillNum) (List Temporary) -> Vect (RegisterNum + SpillNum) (List Temporary)
deleteTargets visited graph = map (filter (\y => all (/= y) visited)) graph

deleteAll : Temporary -> List Temporary -> List Temporary
deleteAll x ys = filter (/= x) ys

spanningTree : Vect (RegisterNum + SpillNum) (List Temporary) -> Temporary -> Temporary -> Tree
spanningTree graph root node = if root == node
  then Back
  else Node node (map (spanningTree graph root) (index (fuseFins node) graph))

spanningForest : Vect (RegisterNum + SpillNum) (List Temporary) -> Vect n Temporary -> Vect n Root
spanningForest graph [] = []
spanningForest graph (x :: xs) = do
  let root = Start x (map (spanningTree graph x) (deleteAll x (index (fuseFins x) graph)))
  root :: spanningForest (deleteTargets (visitedBy root) graph) xs

SpillMove : Type
SpillMove = Bool

storeTemporaryReverse : Temporary -> SpillMove -> List Code
storeTemporaryReverse (Left register) False =
  MOV temp register :: []
storeTemporaryReverse (Left register) True =
  MOVS register stack (stackOffset spillTemp) :: []
storeTemporaryReverse (Right position) False =
  MOVL temp stack (stackOffset position) :: []
storeTemporaryReverse (Right position) True =
  MOVS temp stack (stackOffset spillTemp) ::
  MOVL temp stack (stackOffset position) ::
  []

restoreTemporaryReverse : Temporary  -> SpillMove -> List Code
restoreTemporaryReverse (Left register) False =
  MOV register temp :: []
restoreTemporaryReverse (Left register) True =
  MOVL register stack (stackOffset spillTemp) :: []
restoreTemporaryReverse (Right position) False =
  MOVS temp stack (stackOffset position) :: []
restoreTemporaryReverse (Right position) True =
  MOVS temp stack (stackOffset position) ::
  MOVL temp stack (stackOffset spillTemp) ::
  []

moveToTemporaryReverse : Temporary -> Temporary -> List Code
moveToTemporaryReverse (Left registerSource) (Left registerDest) =
  MOV registerDest registerSource :: []
moveToTemporaryReverse (Left registerSource) (Right positionDest) =
  MOVS registerSource stack (stackOffset positionDest) :: []
moveToTemporaryReverse (Right positionSource) (Left registerDest) =
  MOVL registerDest stack (stackOffset positionSource) :: []
moveToTemporaryReverse (Right positionSource) (Right positionDest) =
  MOVS temp stack (stackOffset positionDest) ::
  MOVL temp stack (stackOffset positionSource) ::
  []

treeMovesReverse : Temporary -> SpillMove -> Tree -> List Code
treeMovesReverse x containsSpillMove Back =
  storeTemporaryReverse x containsSpillMove
treeMovesReverse x containsSpillMove (Node y ts) =
  moveToTemporaryReverse x y ++
  concatMap (treeMovesReverse y containsSpillMove) ts

rootMovesReverse : Root -> List Code
rootMovesReverse (Start x ts) = if any refersBack ts
  then let containsSpillMove = containsSpillEdge (Start x ts)
       in restoreTemporaryReverse x containsSpillMove ++
          concatMap (treeMovesReverse x containsSpillMove) ts
  else concatMap (treeMovesReverse x False) ts

rootMoves : Root -> List Code
rootMoves root = reverse (rootMovesReverse root)

range : {a : Nat} -> {b : Nat} -> Vect (a + b) (Either (Fin a) (Fin b))
range = tabulate unfuseFins

public export
codeExchange : Vect (RegisterNum + SpillNum) (List Temporary) -> List Code
codeExchange graph = concatMap rootMoves (spanningForest graph range)
