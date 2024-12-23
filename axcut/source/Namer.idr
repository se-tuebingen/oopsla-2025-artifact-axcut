module Namer

import Types
import Syntax
import Named

import Data.Vect


nameIdentifier : Vect a Identifier -> Identifier -> Either String (Fin a)
nameIdentifier [] y = do
  Left ("unknown name: " ++ y)
nameIdentifier (x :: xs) y = if x == y
  then do
    pure FZ
  else do
    z <- nameIdentifier xs y
    pure (FS z)

nameIdentifiers : Vect a Identifier -> (bs : List Identifier) -> Either String (c ** (Vect (length bs) Identifier, Vect c Identifier, a = length bs + c))
nameIdentifiers [] [] = do
  pure (0 ** ([], [], Refl))
nameIdentifiers (x :: xs) [] = do
  (c ** (xs, ys, Refl)) <- nameIdentifiers xs []
  pure (S c ** (xs, x :: ys, Refl))
nameIdentifiers (x :: xs) (y :: ys) = if x == y
  then do
    (c ** (xs, ys, Refl)) <- nameIdentifiers xs ys
    pure (c ** (x :: xs, ys, Refl))
  else do
    Left "names in environment don't match"
nameIdentifiers [] (_ :: _) = do
  Left "too many names in environment"


mutual
  -- TODO shadowing should replace position
  nameStatement : Vect d Identifier -> Vect i Identifier -> Vect p Identifier -> Vect a Identifier -> Syntax.Statement -> Either String (Named.Statement d i p a)
  nameStatement ts es ls xs (Substitute vs (Binder ys s)) = do
    ws <- for (fromList vs) (nameIdentifier xs)
    rest <- nameStatement ts es ls (fromList ys) s
    pure (Substitute ws (length ys) rest)
  nameStatement ts es ls xs (Jump l) = do
    -- TODO check that names in environments match
    l <- nameIdentifier ls l
    pure (Jump l)
  nameStatement ts es ls xs (Let x t i bs s) = do
    type <- nameIdentifier ts t
    (_ ** (_, zs, Refl)) <- nameIdentifiers xs bs
    rest <- nameStatement ts es ls (x :: zs) s
    pure (Let type i rest)
  nameStatement ts es ls xs (Switch a cs) = do
    (_ ** (_, ys, Refl)) <- nameIdentifiers xs [a]
    cont <- for cs (nameClause ts es ls ys)
    pure (Switch cont)
  nameStatement ts es ls xs (New x t bs cs s) = do
    type <- nameIdentifier ts t
    (_ ** (ys, zs, Refl)) <- nameIdentifiers xs bs
    cont <- for cs (nameMethod ts es ls ys)
    rest <- nameStatement ts es ls (x :: zs) s
    pure (New type cont rest)
  nameStatement ts es ls xs (Invoke a i) = do
    (_ ** (_, _, Refl)) <- nameIdentifiers xs [a]
    pure (Invoke i)
  nameStatement ts es ls xs (Extern i as cs) = do
    name <- nameIdentifier es i
    args <- for as (nameIdentifier xs)
    tabl <- for cs (nameClause ts es ls xs)
    pure (Extern name args tabl)

  nameClause : Vect d Identifier -> Vect i Identifier -> Vect p Identifier -> Vect a Identifier -> Block -> Either String (Clause d i p a)
  nameClause ts es ls xs (Binder as s) = do
    body <- nameStatement ts es ls (fromList as ++ xs) s
    pure (ClauseBinder (length as) body)

  nameMethod : Vect d Identifier -> Vect i Identifier -> Vect p Identifier -> Vect a Identifier -> Block -> Either String (Method d i p a)
  nameMethod ts es ls xs (Binder as s) = do
    body <- nameStatement ts es ls (xs ++ fromList as) s
    pure (MethodBinder (length as) body)

nameType : Vect d Identifier -> (Polarity, Identifier) -> Either String (Tpe d)
nameType ts (Pro, type) = do
  type <- nameIdentifier ts type
  pure (Pro type)
nameType ts (Con, type) = do
  type <- nameIdentifier ts type
  pure (Con type)
nameType ts (Ext, type) = do
  pure (Ext type)

nameTypes : Vect d Identifier -> Syntax.Program -> Either String (List (Signature d))
nameTypes ts End = do
  pure []
nameTypes ts (Define _ _ _ _ rest) = do
  nameTypes ts rest
nameTypes ts (Typ name sign rest) = do
  sign <- for sign (\args => for args (nameType ts))
  rest <- nameTypes ts rest
  pure (sign :: rest)
nameTypes ts (Import _ _ _ rest) = do
  nameTypes ts rest

nameDefinitions : Vect d Identifier -> Syntax.Program -> Either String (List (Arguments d))
nameDefinitions ts End = do
  pure []
nameDefinitions ts (Define name _ tpes _ rest) = do
  tpes <- for tpes (nameType ts)
  rest <- nameDefinitions ts rest
  pure (tpes :: rest)
nameDefinitions ts (Typ _ _ rest) = do
  nameDefinitions ts rest
nameDefinitions ts (Import _ _ _ rest) = do
  nameDefinitions ts rest

nameImports : Vect d Identifier -> Syntax.Program -> Either String (List (Import d))
nameImports ts End = do
  pure []
nameImports ts (Define _ _ _ _ rest) = do
  nameImports ts rest
nameImports ts (Typ _ _ rest) = do
  nameImports ts rest
nameImports ts (Import name tpes args rest) = do
  tpes <- for tpes (nameType ts)
  args <- for args (\arg => for arg (nameType ts))
  rest <- nameImports ts rest
  pure (Interface name tpes args :: rest)

namePrg : Vect d Identifier -> Vect i Identifier -> Vect p Identifier -> Syntax.Program -> Either String (List (a ** Named.Statement d i p a))
namePrg ts es ls End = do
  pure []
namePrg ts es ls (Define name prms _ body rest) = do
  body <- nameStatement ts es ls (fromList prms) body
  rest <- namePrg ts es ls rest
  pure ((length prms ** body) :: rest)
namePrg ts es ls (Typ _ _ rest) = do
  namePrg ts es ls rest
namePrg ts es ls (Import _ _ _ rest) = do
  namePrg ts es ls rest


-- TODO check that names are unique
typeTable : Syntax.Program -> (d ** Vect d Identifier)
typeTable End =
 (_ ** [])
typeTable (Define _ _ _ _ rest) =
  typeTable rest
typeTable (Typ name _ rest) = do
  let (_ ** rest) = typeTable rest
  (_ ** name :: rest)
typeTable (Import _ _ _ rest) = do
  typeTable rest

-- TODO check that names are unique
importTable : Syntax.Program -> (i ** Vect i Identifier)
importTable End = do
  (_ ** [])
importTable (Define _ _ _ _ rest) = do
  importTable rest
importTable (Typ _ _ rest) = do
  importTable rest
importTable (Import name _ _ rest) = do
  let (_ ** rest) = importTable rest
  (_ ** name :: rest)

-- TODO check that names are unique
labelTable : Syntax.Program -> (p ** Vect p Identifier)
labelTable End = do
  (_ ** [])
labelTable (Define name _ _ _ rest) = do
  let (_ ** rest) = labelTable rest
  (_ ** name :: rest)
labelTable (Typ _ _ rest) = do
  labelTable rest
labelTable (Import _ _ _ rest) = do
  labelTable rest

nameLen : (n : Nat) -> List a -> Either String (Vect n a)
nameLen n xs = case toVect n xs of
  Nothing => Left "length doesn't match"
  Just as => pure as

public export
name : Syntax.Program -> Either String (d ** p ** Named.Program d p)
name program = do
  let (d ** symbols) = typeTable program
  let (i ** imports) = importTable program
  let (p ** labels) = labelTable program
  types <- nameTypes symbols program
  externs <- nameImports symbols program
  signatures <- nameDefinitions symbols program
  statements <- namePrg symbols imports labels program
  types <- nameLen d types
  externs <- nameLen i externs
  signatures <- nameLen p signatures
  statements <- nameLen p statements
  pure (d ** p ** Definitions types externs signatures statements)

