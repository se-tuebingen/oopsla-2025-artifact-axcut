module RISCV.Coder

import Types
import RISCV.Code
import RISCV.Limited
import RISCV.Substitution

import Data.Vect
import Data.List


-- GOTCHAS
-- 0 must not be the address of a block
-- initially heap must point to a fresh block
-- initially free must point to enough space
-- free must be initially filled with 0


positionFromEnd : (as : Arguments d) -> Symbol as a -> Nat
positionFromEnd (b :: []) Z = Z
positionFromEnd (b :: c :: cs) Z = S (positionFromEnd (c :: cs) Z)
positionFromEnd (b :: bs) (S x) = positionFromEnd bs x

unsafeFits : Nat -> Fin 32
unsafeFits n = case natToFin (reserved + n) 32 of
  Just r => r
  Nothing => assert_total (idris_crash "out of registers")

symbolRegister1 : (as : Arguments d) -> Symbol as a -> Register
symbolRegister1 as x = unsafeFits (2 * positionFromEnd as x)

symbolRegister2 : (as : Arguments d) -> Symbol as a -> Register
symbolRegister2 as x = unsafeFits (S (2 * positionFromEnd as x))

freshRegister1 : (as : Arguments d) -> Register
freshRegister1 as = unsafeFits (2 * length as)

freshRegister2 : (as : Arguments d) -> Register
freshRegister2 as = unsafeFits (S (2 * length as))


public export
labelIndex : Symbol ps p -> Fin (length ps)
labelIndex Z = FZ
labelIndex (S x) = FS (labelIndex x)

public export
offsets : Nat -> Vect n (List (Instruction a)) -> Vect n Nat
offsets o Nil = Nil
offsets o (is :: ss) = o :: offsets (o + length is) ss

tagIndex : Symbol as a -> Nat
tagIndex Z = 0
tagIndex (S i) = S (tagIndex i)


address : Nat -> Immediate a
address n = Literal (natToInteger (4 * n))


FieldsPerBlock : Nat
FieldsPerBlock = 3

referenceCountOffset : Immediate n
referenceCountOffset = address 0

nextElementOffset : Immediate n
nextElementOffset = address 0

fieldOffset1 : Nat -> Immediate n
fieldOffset1 i = address (2 + 2 * i)

fieldOffset2 : Nat -> Immediate n
fieldOffset2 i = address (2 + S (2 * i))


skipIfZero : Register -> List (Instruction n) -> List (Instruction n)
skipIfZero this code =
  BEQ this zero (address (S (length code))) ::
  code

ifZeroThenElse : Register -> List (Instruction n) -> List (Instruction n) -> List (Instruction n)
ifZeroThenElse this thn els =
  BEQ this zero (address (S (S (length els)))) ::
  els ++
  JAL zero (address (S (length thn))) ::
  thn


shareBlockN : Register -> Nat -> List (Instruction n)
shareBlockN this k =
  skipIfZero this (
    LW temp this referenceCountOffset ::
    ADDI temp temp (Literal (natToInteger k)) ::
    SW temp this referenceCountOffset ::
    [])

shareBlock : Register -> List (Instruction n)
shareBlock this = shareBlockN this 1

eraseBlock : Register -> List (Instruction n)
eraseBlock this =
  skipIfZero this (
    LW temp this referenceCountOffset ::
    ifZeroThenElse temp (
      SW free this nextElementOffset ::
      ADD free this zero ::
      []
      ) (
      ADDI temp temp (Literal (-1)) ::
      SW temp this referenceCountOffset ::
      []))


shareFields : Register -> Register -> Nat -> List (Instruction n)
shareFields accu this Z = []
shareFields accu this (S i) =
  LW accu this (fieldOffset1 i) ::
  shareBlock accu ++
  shareFields accu this i

eraseFields : Register -> Register -> Nat -> List (Instruction n)
eraseFields accu this Z = []
eraseFields accu this (S i) =
  LW accu this (fieldOffset1 i) ::
  eraseBlock accu ++
  eraseFields accu this i


acquireBlock : Register -> Register -> List (Instruction n)
acquireBlock accu this =
  ADD this heap zero ::
  LW heap heap nextElementOffset ::
  ifZeroThenElse heap (
    ADD heap free zero ::
    LW free free nextElementOffset ::
    ifZeroThenElse free (
      ADDI free heap (fieldOffset1 FieldsPerBlock) ::
      []
      ) (
      SW zero heap nextElementOffset ::
      eraseFields accu heap FieldsPerBlock)
    ) (
    SW zero this referenceCountOffset ::
    [])

releaseBlock : Register -> Register -> List (Instruction n)
releaseBlock accu this =
  LW temp this referenceCountOffset ::
  ifZeroThenElse temp (
    SW heap this nextElementOffset ::
    ADD heap this zero ::
    []
    ) (
    ADDI temp temp (Literal (-1)) ::
    SW temp this referenceCountOffset ::
    -- TODO only share those that are actually loaded
    shareFields accu this FieldsPerBlock)


storeValue : (v : Tpe d) -> (as : Arguments d) -> Register -> (k : Nat) -> List (Instruction n)
storeValue (Ext t) as into k =
  SW (symbolRegister2 (Ext t :: as) Z) into (fieldOffset2 k) ::
  SW zero into (fieldOffset1 k) ::
  []
storeValue v as into k =
  SW (symbolRegister2 (v :: as) Z) into (fieldOffset2 k) ::
  SW (symbolRegister1 (v :: as) Z) into (fieldOffset1 k) ::
  []

loadBinder : (x : Tpe d) -> (as : Arguments d) -> Register -> (k : Nat) -> List (Instruction n)
loadBinder (Ext t) as from k =
  LW (symbolRegister2 (Ext t :: as) Z) from (fieldOffset2 k) ::
  []
loadBinder x as from k =
  LW (symbolRegister2 (x :: as) Z) from (fieldOffset2 k) ::
  LW (symbolRegister1 (x :: as) Z) from (fieldOffset1 k) ::
  []


storeBlock : (as : Arguments d) -> Register -> Nat -> List (Instruction n)
storeBlock as into k =
  SW (freshRegister1 as) into (fieldOffset1 k) ::
  []

loadBlock : (as : Arguments d) -> Register -> Nat -> List (Instruction n)
loadBlock as from k =
  LW (freshRegister1 as) from (fieldOffset1 k) ::
  []


storeZeroes : Register -> Nat -> List (Instruction n)
storeZeroes into Z =
  []
storeZeroes into (S k) =
  SW zero into (fieldOffset1 k) ::
  storeZeroes into k

storeValues : (vs : Arguments d) -> (as : Arguments d) -> Register -> Nat -> List (Instruction n)
storeValues [] as into k =
  storeZeroes into k
storeValues (v :: vs) as into Z =
  -- TODO should not happen
  []
storeValues (v :: vs) as into (S k) =
  storeValue v (vs ++ as) into k ++
  storeValues vs as into k

loadBinders : (xs : Arguments d) -> (as : Arguments d) -> Register -> Nat -> List (Instruction n)
loadBinders [] as from k =
  []
loadBinders (x :: xs) as from Z =
  -- TODO should not happen
  []
loadBinders (x :: xs) as from (S k) =
  loadBinder x (xs ++ as) from k ++
  loadBinders xs as from k


storeRest : (vs : Arguments d) -> (as : Arguments d) -> List (Instruction n)
storeRest [] as = do
  []
storeRest (v :: vs) as = do
  let vars = take (pred FieldsPerBlock) (v :: vs)
  let rest = drop (pred FieldsPerBlock) (v :: vs)
  let this = freshRegister1 (rest ++ as)
  let accu = freshRegister2 (rest ++ as)
  storeBlock (vars ++ rest ++ as) heap (pred FieldsPerBlock) ++
  storeValues vars (rest ++ as) heap (pred FieldsPerBlock) ++
  acquireBlock accu this ++
  storeRest rest as

store : (vs : Arguments d) -> (as : Arguments d) -> List (Instruction n)
store [] as =
  ADD (freshRegister1 as) zero zero ::
  []
store vs as = do
  let vars = take FieldsPerBlock vs
  let rest = drop FieldsPerBlock vs
  let this = freshRegister1 (rest ++ as)
  let accu = freshRegister2 (rest ++ as)
  storeValues vars (rest ++ as) heap FieldsPerBlock ++
  acquireBlock accu this ++
  storeRest rest as


loadRest : (xs : Arguments d) -> (as : Arguments d) -> List (Instruction n)
loadRest [] as = do
  []
loadRest (x :: xs) as = do
  let vars = take (pred FieldsPerBlock) (x :: xs)
  let rest = drop (pred FieldsPerBlock) (x :: xs)
  let this = freshRegister1 (rest ++ as)
  let accu = freshRegister2 (rest ++ as)
  loadRest rest as ++
  releaseBlock accu this ++
  loadBlock (vars ++ rest ++ as) this (pred FieldsPerBlock) ++
  loadBinders vars (rest ++ as) this (pred FieldsPerBlock)

load : (xs : Arguments d) -> (as : Arguments d) -> List (Instruction n)
load [] as =
  []
load xs as = do
  let vars = take FieldsPerBlock xs
  let rest = drop FieldsPerBlock xs
  let this = freshRegister1 (rest ++ as)
  let accu = freshRegister2 (rest ++ as)
  loadRest rest as ++
  releaseBlock accu this ++
  loadBinders vars (rest ++ as) this FieldsPerBlock


codeTable : Vect n Nat -> List (Instruction p)
codeTable [] = []
codeTable (o :: os) =
  JAL zero (address (S (length os) + o)) ::
  codeTable os


isSame : Symbol as a -> Symbol as b -> Maybe (a = b)
isSame Z Z = Just Refl
isSame Z (S x) = Nothing
isSame (S x) Z = Nothing
isSame (S x) (S y) = do
  Refl <- isSame x y
  Just Refl

occurences : Substitution bs cs -> Substitution as cs -> Symbol as a -> List (Symbol bs a)
occurences [] [] z = []
occurences (x :: xs) (y :: ys) z = case isSame y z of
  Nothing => occurences xs ys z
  Just Refl => x :: occurences xs ys z

transpose : {as : Arguments d} -> {bs : Arguments d} -> Substitution as bs -> Table (\b => List (Symbol bs b)) as
transpose xs = transform (occurences identity xs) identity

updateReferenceCount : Register -> Nat -> List (Instruction n)
updateReferenceCount x Z =
  eraseBlock x
updateReferenceCount x (S Z) =
  []
updateReferenceCount x (S (S n)) =
  shareBlockN x (S n)

codeWeakeningContraction : {as : Arguments d} -> Table (\b => List (Symbol bs b)) as -> List (Instruction n)
codeWeakeningContraction [] =
  []
codeWeakeningContraction {as = Ext t :: as} (r :: rs) =
  codeWeakeningContraction {as} rs
codeWeakeningContraction {as = a :: as} (r :: rs) =
  updateReferenceCount (symbolRegister1 (a :: as) Z) (length r) ++
  codeWeakeningContraction {as} rs

connections : {as : Arguments d} -> {bs : Arguments d} -> Table (\b => List (Symbol bs b)) as -> Vect 32 (List Register)
connections [] =
  replicate 32 []
connections {as = Ext t :: as} {bs} (xs :: xss) =
  replaceAt (symbolRegister2 (Ext t :: as) Z) (map (symbolRegister2 bs) xs) (
  connections {as} {bs} xss)
connections {as = a :: as} {bs} (xs :: xss) =
  replaceAt (symbolRegister1 (a :: as) Z) (map (symbolRegister1 bs) xs) (
  replaceAt (symbolRegister2 (a :: as) Z) (map (symbolRegister2 bs) xs) (
  connections {as} {bs} xss))


mutual
  public export
  codeStatement : {ts : Vect d (Signature d)} -> Statement ts ps as -> List (Instruction (length ps))
  codeStatement (Substitute {as} {bs} xs s) = do
    let targets = transpose {as} {bs} xs
    codeWeakeningContraction targets ++
    codeExchange (connections targets) ++
    codeStatement s
  codeStatement (Jump l) = do
    JALR zero zero (Label (labelIndex l)) ::
    Nil
  codeStatement (Let {t} {as} {bs} i s) = do
    let tag = symbolRegister2 (Pro t :: as) Z
    let rest = codeStatement s
    store bs as ++
    ADDI tag zero (address (tagIndex i)) ::
    rest
  codeStatement (Switch {t} {as} ss) = do
    let tag = symbolRegister2 (Pro t :: as) Z
    let code = codeClauses as (index t ts) ss
    JALR zero tag (Add This (address (S Z))) ::
    codeTable (offsets 0 code) ++
    concat code
  codeStatement (New {t} {as} {bs} ss s) = do
    let tab = symbolRegister2 (Con t :: as) Z
    let rest = codeStatement s
    let code = codeMethods bs (index t ts) ss
    store bs as ++
    ADDI tab zero (Add This (address (S (length rest)))) ::
    rest ++
    codeTable (offsets 0 code) ++
    concat code
  codeStatement (Invoke {t} {as} i) = do
    let tab = symbolRegister2 (Con t :: as) Z
    JALR zero tab (address (tagIndex i)) ::
    Nil
  codeStatement (Extern (Restart) [] []) = do
    JALR zero zero (Literal 0) ::
    Nil
  codeStatement (Extern (Return) [x] []) = do
    ADD (symbolRegister2 {d} [Ext "Int"] Z) (symbolRegister2 as x) zero ::
    JALR zero zero (Literal 0) ::
    Nil
  codeStatement (Extern (Literal n) [] [s]) = do
    ADDI (symbolRegister2 (Ext "Int" :: as) Z) zero (Literal n) ::
    codeStatement s
  codeStatement (Extern Add [x, y] [s]) = do
    ADD (symbolRegister2 (Ext "Int" :: as) Z) (symbolRegister2 as x) (symbolRegister2 as y) ::
    codeStatement s
  codeStatement (Extern Mul [x, y] [s]) = do
    MUL (symbolRegister2 (Ext "Int" :: as) Z) (symbolRegister2 as x) (symbolRegister2 as y) ::
    codeStatement s
  codeStatement (Extern Rem [x, y] [s]) = do
    REM (symbolRegister2 (Ext "Int" :: as) Z) (symbolRegister2 as x) (symbolRegister2 as y) ::
    codeStatement s
  codeStatement (Extern IfZero [x] [z, s]) = do
    let thenBranch = codeStatement z
    let elseBranch = codeStatement s
    BEQ (symbolRegister2 as x) zero (address (S (length elseBranch))) ::
    elseBranch ++
    thenBranch
  codeStatement (Extern IfEqual [x, y] [z, s]) = do
    let thenBranch = codeStatement z
    let elseBranch = codeStatement s
    BEQ (symbolRegister2 as x) (symbolRegister2 as y) (address (S (length elseBranch))) ::
    elseBranch ++
    thenBranch
  codeStatement (Extern IfLess [x, y] [z, s]) = do
    let thenBranch = codeStatement z
    let elseBranch = codeStatement s
    BLT (symbolRegister2 as x) (symbolRegister2 as y) (address (S (length elseBranch))) ::
    elseBranch ++
    thenBranch
  codeStatement (Extern _ _ _) = do
    -- TODO how to convince Idris 2?
    believe_me "total"

  codeClause : {ts : Vect d (Signature d)} -> (as : Arguments d) -> (cs : Arguments d) -> Statement ts ps (cs ++ as) -> List (Instruction (length ps))
  codeClause as cs s = do
    load cs as ++
    codeStatement s

  codeMethod : {ts : Vect d (Signature d)} -> (as : Arguments d) -> (cs : Arguments d) -> Statement ts ps (as ++ cs) -> List (Instruction (length ps))
  codeMethod as cs s = do
    load as cs ++
    codeStatement s

  codeClauses : {ts : Vect d (Signature d)} -> (as : Arguments d) -> (us : Signature d) -> Clauses ts ps as us -> Vect (length us) (List (Instruction (length ps)))
  codeClauses as [] [] = []
  codeClauses as (cs :: us) (s :: ss) = codeClause as cs s :: codeClauses as us ss

  codeMethods : {ts : Vect d (Signature d)} -> (as : Arguments d) -> (us : Signature d) -> Methods ts ps as us -> Vect (length us) (List (Instruction (length ps)))
  codeMethods as [] [] = []
  codeMethods as (cs :: us) (s :: ss) = codeMethod as cs s :: codeMethods as us ss


public export
translate : {ts : Vect d (Signature d)} -> Table (Statement ts ps) os -> Vect (length os) (List (Instruction (length ps)))
translate [] = []
translate (s :: ls) = codeStatement s :: translate ls


public export
resolveImmediate : Vect n Nat -> Nat -> Immediate n -> Integer
resolveImmediate os o (Literal n) = n
resolveImmediate os o (Add a b) = resolveImmediate os o a + resolveImmediate os o b
resolveImmediate os o (Label l) = natToInteger (index l os * 4)
resolveImmediate os o This = natToInteger (o * 4)

public export
resolveInstruction : Vect n Nat -> Nat -> Instruction n -> Code
resolveInstruction os o (ADD x y z) = ADD x y z
resolveInstruction os o (ADDI x y c) = ADDI x y (resolveImmediate os o c)
resolveInstruction os o (JAL x c) = JAL x (resolveImmediate os o c)
resolveInstruction os o (JALR x y c) = JALR x y (resolveImmediate os o c)
resolveInstruction os o (LW x y c) = LW x y (resolveImmediate os o c)
resolveInstruction os o (SW x y c) = SW x y (resolveImmediate os o c)
resolveInstruction os o (BEQ x y c) = BEQ x y (resolveImmediate os o c)
resolveInstruction os o (MUL x y z) = MUL x y z
resolveInstruction os o (REM x y z) = REM x y z
resolveInstruction os o (BLT x y c) = BLT x y (resolveImmediate os o c)

public export
resolveSection : Vect n Nat -> Nat -> List (Instruction n) -> List Code
resolveSection os o [] = []
resolveSection os o (i :: is) = resolveInstruction os o i :: resolveSection os (S o) is

public export
assemble : Vect m Nat -> Nat -> Vect n (List (Instruction m)) -> List Code
assemble os o Nil = Nil
assemble os o (is :: ss) = resolveSection os o is ++ assemble os (o + length is) ss


public export
compile : {ts : Vect d (Signature d)} -> Program ts ps -> List Code
compile program = do
  let sections = translate program
  assemble (offsets 0 sections) 0 sections

