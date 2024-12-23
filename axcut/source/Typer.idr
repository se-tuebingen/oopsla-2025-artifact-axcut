module Typer

import Types
import Named
import Typed

import Data.Vect
import Decidable.Equality


Elms : Vect n a -> List a
Elms [] = []
Elms (x :: xs) = x :: Elms xs

From : (xs : List a) -> Vect (length xs) a
From [] = []
From (x :: xs) = x :: From xs

elmsAppend : (bs : Vect n a) -> (as : Vect m a) -> Elms bs ++ Elms as = Elms (bs ++ as)
elmsAppend [] as = Refl
elmsAppend (b :: bs) as = rewrite (elmsAppend {bs} {as}) in Refl

elmsFrom : {as : List a} -> as = Elms (From as)
elmsFrom {as = []} = Refl
elmsFrom {as = a :: as} = rewrite sym (elmsFrom {as}) in Refl

splitVect : (b : Nat) -> (as : Vect (b + c) x) -> (bs : Vect b x ** cs : Vect c x ** as = bs ++ cs)
splitVect Z as = ([] ** as ** Refl)
splitVect (S b) (a :: as) = case splitVect b as of
  (bs ** cs ** refl) => (a :: bs ** cs ** rewrite refl in Refl)


checkTpe : (a : Tpe d) -> (b : Tpe d) -> Either String (a = b)
checkTpe (Pro t) (Pro u) = case decEq t u of
  Yes Refl => Right Refl
  No _ => Left "signature name mismatch"
checkTpe (Con t) (Con u) = case decEq t u of
  Yes Refl => Right Refl
  No _ => Left "signature name mismatch"
checkTpe (Ext t) (Ext u) = case decEq t u of
  Yes Refl => Right Refl
  No _ => Left "external name mismatch"
checkTpe _ _ = do
  Left "polarity mismatch"

checkArguments : (as : Arguments d) -> (bs : Arguments d) -> Either String (as = bs)
checkArguments [] [] = do
  pure Refl
checkArguments (a :: as) (b :: bs) = do
  Refl <- checkTpe a b
  Refl <- checkArguments as bs
  pure Refl
checkArguments _ _ = do
  Left "arguments mismatch"

checkSignature : (as : Signature d) -> (bs : Signature d) -> Either String (as = bs)
checkSignature [] [] = do
  pure Refl
checkSignature (a :: as) (b :: bs) = do
  Refl <- checkArguments a b
  Refl <- checkSignature as bs
  pure Refl
checkSignature _ _ = do
  Left "signature mismatch"


checkPro : (a : Tpe d) -> Either String (t ** a = Pro t)
checkPro (Pro t) = pure (t ** Refl)
checkPro (Con _) = Left "type must be produced"
checkPro (Ext _) = Left "type must be produced"

checkCon : (a : Tpe d) -> Either String (t ** a = Con t)
checkCon (Pro _) = Left "type must be consumed"
checkCon (Con t) = pure (t ** Refl)
checkCon (Ext _) = Left "type must be consumed"

checkLabel : (qs : Vect n (Arguments d)) -> (x : Fin n) -> (as : (Arguments d)) -> Either String (Symbol (Elms qs) as)
checkLabel (as :: qs) FZ bs = do
  Refl <- checkArguments as bs
  pure Z
checkLabel (_ :: qs) (FS x) bs = do
  l <- checkLabel qs x bs
  pure (S l)

checkTag : (ts : Signature d) -> Nat -> Either String (cs ** Tag ts cs)
checkTag [] i = do
  Left "tag too large"
checkTag (cs :: ts) Z = do
  pure (cs ** Z)
checkTag (cs :: ts) (S i) = do
  (cs ** i) <- checkTag ts i
  pure (cs ** S i)

checkArity : Vect n a -> (r : Nat) -> Either String (n = r)
checkArity [] Z = do
  pure Refl
checkArity [] (S _) = do
  Left "not enough parameters"
checkArity (_ :: _) Z = do
  Left "too many parameters"
checkArity (_ :: vs) (S r) = do
  Refl <- checkArity vs r
  pure Refl


inferVariable : (as : Vect n (Tpe d)) -> (x : Fin n) -> Either String (a ** Symbol (Elms as) a)
inferVariable (a :: as) FZ = do
  pure (a ** Z)
inferVariable (_ :: as) (FS x) = do
  (t ** y) <- inferVariable as x
  pure (t ** S y)

inferSubstitution : (as : Vect a (Tpe d)) -> Vect b (Fin a) -> Either String (bs : Vect b (Tpe d) ** Substitution (Elms as) (Elms bs))
inferSubstitution as [] = do
  pure ([] ** [])
inferSubstitution as (x :: xs) = do
  (t ** y) <- inferVariable as x
  (ts ** ys) <- inferSubstitution as xs
  pure (t :: ts ** y :: ys)


mutual
  checkStatement : (ts : Vect d (Signature d)) -> (es : Vect i (Import d)) -> (ps : Vect p (Arguments d)) -> (as : Vect a (Tpe d)) -> Named.Statement d i p a -> Either String (Typed.Statement ts (Elms ps) (Elms as))
  checkStatement ts es ps as (Substitute vs r s) = do
    (bs ** vs) <- inferSubstitution as vs
    Refl <- checkArity bs r
    rest <- checkStatement ts es ps bs s
    pure (Substitute vs rest)
  checkStatement ts es ps as (Jump l) = do
    label <- checkLabel ps l (Elms as)
    pure (Jump label)
  checkStatement ts es ps as (Let {b} {a} t i s) = do
    let (bs ** ds ** Refl) = splitVect {c = a} b as
    (cs ** indx) <- checkTag (index t ts) i
    Refl <- checkArguments cs (Elms bs)
    rest <- checkStatement ts es ps (Pro t :: ds) s
    pure (rewrite sym (elmsAppend bs ds) in Let indx rest)
  checkStatement ts es ps (a :: as) (Switch cs) = do
    (t ** refl) <- checkPro a
    cont <- checkClauses ts es ps as (index t ts) cs
    pure (rewrite refl in Switch cont)
  checkStatement ts es ps as (New {b} {a} t cs s) = do
    let (bs ** ds ** Refl) = splitVect {c = a} b as
    impl <- checkMethods ts es ps bs (index t ts) cs
    rest <- checkStatement ts es ps (Con t :: ds) s
    pure (rewrite sym (elmsAppend bs ds) in (New impl rest))
  checkStatement ts es ps (a :: as) (Invoke i) = do
    (t ** refl) <- checkCon a
    (ws ** indx) <- checkTag (index t ts) i
    Refl <- checkArguments ws (Elms as)
    pure (rewrite refl in Invoke indx)
  checkStatement ts es ps as (Extern i xs cs) = do
    let Interface name bs qs = index i es
    (us ** args) <- inferSubstitution as (From xs)
    Refl <- checkArguments (Elms us) bs
    tabl <- checkClauses ts es ps as qs cs
    pure (Extern name args tabl)

  checkClause : (ts : Vect d (Signature d)) -> (es : Vect i (Import d)) -> (ps : Vect p (Arguments d)) -> (as : Vect a (Tpe d)) -> (bs : Vect b (Tpe d)) -> Named.Clause d i p a -> Either String (Typed.Clause ts (Elms ps) (Elms as) (Elms bs))
  checkClause ts es ps as bs (ClauseBinder r s) = do
    Refl <- checkArity bs r
    body <- checkStatement ts es ps (bs ++ as) s
    pure (rewrite elmsAppend bs as in body)

  checkMethod : (ts : Vect d (Signature d)) -> (es : Vect i (Import d)) -> (ps : Vect p (Arguments d)) -> (as : Vect a (Tpe d)) -> (bs : Vect b (Tpe d)) -> Named.Method d i p a -> Either String (Typed.Method ts (Elms ps) (Elms as) (Elms bs))
  checkMethod ts es ps as bs (MethodBinder r s) = do
    Refl <- checkArity bs r
    body <- checkStatement ts es ps (as ++ bs) s
    pure (rewrite elmsAppend as bs in body)

  checkClauses : (ts : Vect d (Signature d)) -> (es : Vect i (Import d)) -> (ps : Vect p (Arguments d)) -> (as : Vect a (Tpe d)) -> (qs : List (Arguments d)) -> List (Named.Clause d i p a) -> Either String (Typed.Clauses ts (Elms ps) (Elms as) qs)
  checkClauses ts es ps as [] [] = do
    pure []
  checkClauses ts es ps as (q :: qs) (c :: cs) = do
    cont <- checkClause ts es ps as (From q) c
    tabl <- checkClauses ts es ps as qs cs
    pure (rewrite elmsFrom {as = q} in cont :: tabl)
  checkClauses ts es ps as _ _ = do
    Left "too many or too few clauses"

  checkMethods : (ts : Vect d (Signature d)) -> (es : Vect i (Import d)) -> (ps : Vect p (Arguments d)) -> (as : Vect a (Tpe d)) -> (qs : List (Arguments d)) -> List (Named.Method d i p a) -> Either String (Typed.Methods ts (Elms ps) (Elms as) qs)
  checkMethods ts es ps as [] [] = do
    pure []
  checkMethods ts es ps as (q :: qs) (c :: cs) = do
    cont <- checkMethod ts es ps as (From q) c
    tabl <- checkMethods ts es ps as qs cs
    pure (rewrite elmsFrom {as = q} in cont :: tabl)
  checkMethods ts es ps as _ _ = do
    Left "too many or too few methods"


checkProgram : {qs : Vect q (Arguments d)} -> (ts : Vect d (Signature d)) -> (es : Vect i (Import d)) -> (ps : Vect p (Arguments d)) -> Vect q (a ** Named.Statement d i p a) -> Either String (Table (Typed.Statement ts (Elms ps)) (Elms qs))
checkProgram {qs = []} ts es ps [] = do
  pure []
checkProgram {qs = as :: qs} ts es ps ((a ** body) :: rest) = do
  Refl <- checkArity (From as) a
  body <- checkStatement ts es ps (From as) body
  rest <- checkProgram ts es ps rest
  pure ((rewrite elmsFrom {as = as} in body) :: rest)

public export
check : Named.Program d p -> Either String (ts : Vect d (Signature d) ** ps : List (Arguments d) ** Typed.Program ts ps)
check (Definitions types externs signatures program) = do
  typed <- checkProgram types externs signatures program
  pure (types ** Elms signatures ** typed)

