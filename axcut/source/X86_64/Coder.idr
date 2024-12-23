module X86_64.Coder

import Types
import X86_64.Code
import X86_64.Limited
import X86_64.Substitution

import Data.Vect
import Data.List
import Control.Monad.State


-- TODO
-- better label names

-- GOTCHAS
-- 0 must not be the address of a block
-- initially heap must point to a fresh block
-- initially free must point to enough space
-- free must be initially filled with 0


positionFromEnd : (as : Arguments d) -> Symbol as a -> Nat
positionFromEnd (b :: []) Z = Z
positionFromEnd (b :: c :: cs) Z = S (positionFromEnd (c :: cs) Z)
positionFromEnd (b :: bs) (S x) = positionFromEnd bs x

unsafeFitsOrSpill : Nat -> Temporary
unsafeFitsOrSpill n = let temporary = (reserved + n) in
  case natToFin temporary RegisterNum of
    Just r => Left r
    Nothing => case natToFin ((minus temporary RegisterNum) + reservedSpills) SpillNum of
      Just p => Right p
      Nothing => assert_total (idris_crash "out of space for variables")

symbolLocation1 : (as : Arguments d) -> Symbol as a -> Temporary
symbolLocation1 as x = unsafeFitsOrSpill (2 * positionFromEnd as x)

symbolLocation2 : (as : Arguments d) -> Symbol as a -> Temporary
symbolLocation2 as x = unsafeFitsOrSpill (S (2 * positionFromEnd as x))

freshLocation1 : (as : Arguments d) -> Temporary
freshLocation1 as = unsafeFitsOrSpill (2 * length as)

freshLocation2 : (as : Arguments d) -> Temporary
freshLocation2 as = unsafeFitsOrSpill (S (2 * length as))


symbolToNat : Symbol as a -> Nat
symbolToNat Z = 0
symbolToNat (S i) = S (symbolToNat i)


moveFromRegister : Temporary -> Register -> Code
moveFromRegister (Left registerDest) register = MOV registerDest register
moveFromRegister (Right positionDest) register = MOVS register stack (stackOffset positionDest)

moveToRegister : Register -> Temporary -> Code
moveToRegister register (Left registerSource) = MOV register registerSource
moveToRegister register (Right positionSource) = MOVL register stack (stackOffset positionSource)

addToRegister : Register -> Temporary -> Code
addToRegister register (Left registerSource) = ADD register registerSource
addToRegister register (Right positionSource) = ADDM register stack (stackOffset positionSource)

add : Temporary -> Temporary -> Temporary -> List Code
add (Left register) t1 t2 =
  moveToRegister register t1 ::
  addToRegister register t2 ::
  []
add (Right position) t1 t2 =
  moveToRegister temp t1 ::
  addToRegister temp t2 ::
  MOVS temp stack (stackOffset position) ::
  []

subFromRegister : Register -> Temporary -> Code
subFromRegister register (Left registerSource) = SUB register registerSource
subFromRegister register (Right positionSource) = SUBM register stack (stackOffset positionSource)

sub : Temporary -> Temporary -> Temporary -> List Code
sub (Left register) t1 t2 =
  moveToRegister register t1 ::
  subFromRegister register t2 ::
  []
sub (Right position) t1 t2 =
  moveToRegister temp t1 ::
  subFromRegister temp t2 ::
  MOVS temp stack (stackOffset position) ::
  []

mulToRegister : Register -> Temporary -> Code
mulToRegister register (Left registerSource) = IMUL register registerSource
mulToRegister register (Right positionSource) = IMULM register stack (stackOffset positionSource)

mul : Temporary -> Temporary -> Temporary -> List Code
mul (Left register) t1 t2 =
  moveToRegister register t1 ::
  mulToRegister register t2 ::
  []
mul (Right position) t1 t2 =
  moveToRegister temp t1 ::
  mulToRegister temp t2 ::
  MOVS temp stack (stackOffset position) ::
  []

div : Temporary -> List Code
div (Left register) =
  if register == return2
  then
    CQO ::
    IDIV temp ::
    []
  else
    CQO ::
    IDIV register ::
    []
div (Right position) =
  CQO ::
  IDIVM stack (stackOffset position) ::
  []


jump : Temporary -> List Code
jump (Left register) =
  JMP register ::
  []
jump (Right position) =
  MOVL temp stack (stackOffset position) ::
  JMP temp ::
  []

addAndJump : Temporary -> Integer -> List Code
addAndJump (Left register) imm =
  ADDI register imm ::
  JMP register ::
  []
addAndJump (Right position) imm =
  MOVL temp stack (stackOffset position) ::
  ADDI temp imm ::
  JMP temp ::
  []


compare : Temporary -> Temporary -> List Code
compare (Left register1) (Left register2) =
  CMP register1 register2 ::
  []
compare (Left register1) (Right position2) =
  CMPRM register1 stack (stackOffset position2) ::
  []
compare (Right position1) (Left register2) =
  CMPMR stack (stackOffset position1) register2 ::
  []
compare (Right position1) (Right position2) =
  MOVL temp stack (stackOffset position1) ::
  CMPRM temp stack (stackOffset position2) ::
  []

compareImmediate : Temporary -> Integer -> Code
compareImmediate (Left register) imm = CMPI register imm
compareImmediate (Right position) imm = CMPIM stack (stackOffset position) imm

loadImmediate : Temporary -> Integer -> Code
loadImmediate (Left register) imm = MOVI register imm
loadImmediate (Right position) imm = MOVIM stack (stackOffset position) imm


loadLabel : Temporary -> String -> List Code
loadLabel (Left register) lab =
  LEAL register lab :: []
loadLabel (Right position) lab =
  LEAL temp lab ::
  MOVS temp stack (stackOffset position) ::
  []


loadByte : Temporary -> Temporary -> Temporary -> List Code
loadByte (Left registerDest) source offset =
  moveToRegister temp source ::
  addToRegister temp offset ::
  MOVZX registerDest temp 0 ::
  []
loadByte (Right positionDest) (Left registerSource) offset =
  addToRegister registerSource offset ::
  MOVZX temp registerSource 0 ::
  MOVS temp stack (stackOffset positionDest) ::
  subFromRegister registerSource offset ::
  []
loadByte (Right positionDest) (Right positionSource) (Left registerOffset) =
  ADDM registerOffset stack (stackOffset positionSource) ::
  MOVZX temp registerOffset 0 ::
  MOVS temp stack (stackOffset positionDest) ::
  SUBM registerOffset stack (stackOffset positionSource) ::
  []
loadByte (Right positionDest) (Right positionSource) (Right positionOffset) =
  MOVS return1 stack (stackOffset spillTemp) ::
  MOVL return1 stack (stackOffset positionSource) ::
  ADDM return1 stack (stackOffset positionOffset) ::
  MOVZX temp return1 0 ::
  MOVS temp stack (stackOffset positionDest) ::
  MOVL return1 stack (stackOffset spillTemp) ::
  []

storeByte : Temporary -> Temporary -> Temporary -> List Code
storeByte (Left registerSource) dest offset =
  moveToRegister temp dest ::
  addToRegister temp offset ::
  MOVRB registerSource temp 0 ::
  []
storeByte (Right positionSource) (Left registerDest) offset =
  addToRegister registerDest offset ::
  MOVL temp stack (stackOffset positionSource) ::
  MOVRB temp registerDest 0 ::
  subFromRegister registerDest offset ::
  []
storeByte (Right positionSource) (Right positionDest) (Left registerOffset) =
  ADDM registerOffset stack (stackOffset positionDest) ::
  MOVL temp stack (stackOffset positionSource) ::
  MOVRB temp registerOffset 0 ::
  SUBM registerOffset stack (stackOffset positionDest) ::
  []
storeByte (Right positionSource) (Right positionDest) (Right positionOffset) =
  MOVS return1 stack (stackOffset spillTemp) ::
  MOVL return1 stack (stackOffset positionDest) ::
  ADDM return1 stack (stackOffset positionOffset) ::
  MOVL temp stack (stackOffset positionSource) ::
  MOVRB temp return1 0 ::
  MOVL return1 stack (stackOffset spillTemp) ::
  []


skipIfZero : Temporary -> List Code -> State Integer (List Code)
skipIfZero this code = do
  freshLab <- get
  _ <- put (1 + freshLab)
  pure (
    compareImmediate this 0 ::
    JEL ("lab" ++ show freshLab) ::
    code ++
    LAB ("lab" ++ show freshLab) ::
    [])

ifZeroThenElse : Register -> Maybe Integer -> List Code -> List Code -> State Integer (List Code)
ifZeroThenElse this mOffset thn els = do
  freshLabThen <- get
  _ <- put (1 + freshLabThen)
  freshLabElse <- get
  _ <- put (1 + freshLabElse)
  pure (
    (case mOffset of
      Nothing => CMPI this 0
      Just offset => CMPIM this offset 0) ::
    JEL ("lab" ++ show freshLabThen) ::
    els ++
    JMPL ("lab" ++ show freshLabElse) ::
    LAB ("lab" ++ show freshLabThen) ::
    thn ++
    LAB ("lab" ++ show freshLabElse) ::
    [])


shareBlockN : Temporary -> Nat -> State Integer (List Code)
shareBlockN (Left registerThis) k =
  skipIfZero (Left registerThis) (
    ADDIM registerThis referenceCountOffset (natToInteger k) ::
    [])
shareBlockN (Right positionThis) k =
  skipIfZero (Right positionThis) (
    MOVL temp stack (stackOffset positionThis) ::
    ADDIM temp referenceCountOffset (natToInteger k) ::
    [])

shareBlock : Temporary -> State Integer (List Code)
shareBlock this = shareBlockN this 1

eraseValidObject : Register -> State Integer (List Code)
eraseValidObject this =
  ifZeroThenElse this (Just referenceCountOffset) (
    MOVS free this nextElementOffset ::
    MOV free this ::
    []
    ) (
    ADDIM this referenceCountOffset (-1) ::
    [])

eraseBlock : Temporary -> State Integer (List Code)
eraseBlock (Left registerThis) = do
  erase <- eraseValidObject registerThis
  skipIfZero (Left registerThis) (
    erase)
eraseBlock (Right positionThis) = do
  erase <- eraseValidObject temp
  skipIfZero (Right positionThis) (
    MOVL temp stack (stackOffset positionThis) ::
    erase)


shareFields : Register -> Nat -> State Integer (List Code)
shareFields this Z = pure []
shareFields this (S i) = do
  shareCurr <- shareBlock (Left temp)
  shareRest <- shareFields this i
  pure (
    MOVL temp this (fieldOffset1 i) ::
    shareCurr ++
    shareRest)

eraseFields : Register -> Nat -> State Integer (List Code)
eraseFields this Z = pure []
eraseFields this (S i) = do
  eraseCurr <- eraseBlock (Left temp)
  eraseRest <- eraseFields this i
  pure (
    MOVL temp this (fieldOffset1 i) ::
    eraseCurr ++
    eraseRest)


acquireBlock : Temporary -> State Integer (List Code)
acquireBlock this = do
  erase <- eraseFields heap FieldsPerBlock
  adaptFree <- ifZeroThenElse free Nothing (
                 MOV free heap ::
                 ADDI free (fieldOffset1 FieldsPerBlock) ::
                 []
                 ) (
                 MOVIM heap nextElementOffset 0 ::
                 erase)
  adaptHeap <- ifZeroThenElse heap Nothing (
                 MOV heap free ::
                 MOVL free free nextElementOffset ::
                 adaptFree
                 ) (
                 (case this of
                   Left register => MOVIM register referenceCountOffset 0
                   Right position => -- this instruction would be needed if the below optimization for the
                                     -- fast path would not be made
                                     --MOVL temp stack (stackOffset position)
                                     MOVIM temp referenceCountOffset 0) ::
                 [])
  pure (
    (case this of
      Left register => MOV register heap :: []
      Right position => -- this moves the memory block both to `temp` and to its spill position for better
                        -- performance in the fast path, but executes the first instruction unnecessarily
                        -- in the slow path
                        MOV temp heap ::
                        MOVS heap stack (stackOffset position) ::
                        []) ++
    MOVL heap heap nextElementOffset ::
    adaptHeap ++
    [])

releaseBlock : Register -> State Integer (List Code)
releaseBlock this = do
  share <- shareFields this FieldsPerBlock
  ifZeroThenElse this (Just referenceCountOffset) (
    MOVS heap this nextElementOffset ::
    MOV heap this ::
    []
    ) (
    ADDIM this referenceCountOffset (-1) ::
    -- TODO only share those that are actually loaded
    share)


storeTemporary : Temporary -> Register -> Integer -> List Code
storeTemporary (Left register) this offset =
  MOVS register this offset :: []
storeTemporary (Right position) this offset =
  MOVL temp stack (stackOffset position) ::
  MOVS temp this offset ::
  []

storeValue : (v : Tpe d) -> (as : Arguments d) -> Register -> (k : Nat) -> List Code
storeValue (Ext t) as into k =
  storeTemporary (symbolLocation2 (Ext t :: as) Z) into (fieldOffset2 k) ++
  MOVIM into (fieldOffset1 k) 0 ::
  []
storeValue v as into k =
  storeTemporary (symbolLocation2 (v :: as) Z) into (fieldOffset2 k) ++
  storeTemporary (symbolLocation1 (v :: as) Z) into (fieldOffset1 k)

loadToTemporary : Temporary -> Register -> Integer -> List Code
loadToTemporary (Left register) from offset =
  MOVL register from offset :: []
loadToTemporary (Right position) from offset =
  MOVL temp from offset ::
  MOVS temp stack (stackOffset position) ::
  []

loadBinder : (x : Tpe d) -> (as : Arguments d) -> Register -> (k : Nat) -> List Code
loadBinder (Ext t) as from k =
  loadToTemporary (symbolLocation2 (Ext t :: as) Z) from (fieldOffset2 k)
loadBinder x as from k =
  loadToTemporary (symbolLocation2 (x :: as) Z) from (fieldOffset2 k) ++
  loadToTemporary (symbolLocation1 (x :: as) Z) from (fieldOffset1 k)


storeBlock : (as : Arguments d) -> Register -> Nat -> List Code
storeBlock as into k =
  storeTemporary (freshLocation1 as) into (fieldOffset1 k)

loadBlock : (as : Arguments d) -> Register -> Nat -> List Code
loadBlock as from k =
  loadToTemporary (freshLocation1 as) from (fieldOffset1 k)


storeZeroes : Register -> Nat -> List Code
storeZeroes into Z =
  []
storeZeroes into (S k) =
  MOVIM into (fieldOffset1 k) 0 ::
  storeZeroes into k

storeValues : (vs : Arguments d) -> (as : Arguments d) -> Register -> Nat -> List Code
storeValues [] as into k =
  storeZeroes into k
storeValues (v :: vs) as into Z =
  -- TODO should not happen
  []
storeValues (v :: vs) as into (S k) =
  storeValue v (vs ++ as) into k ++
  storeValues vs as into k

loadBinders : (xs : Arguments d) -> (as : Arguments d) -> Register -> Nat -> List Code
loadBinders [] as from k =
  []
loadBinders (x :: xs) as from Z =
  -- TODO should not happen
  []
loadBinders (x :: xs) as from (S k) =
  loadBinder x (xs ++ as) from k ++
  loadBinders xs as from k


storeRest : (vs : Arguments d) -> (as : Arguments d) -> State Integer (List Code)
storeRest [] as =
  pure []
storeRest (v :: vs) as = do
  let vars = take FieldsPerBlock (v :: vs)
      rest = drop FieldsPerBlock (v :: vs)
      this = freshLocation1 (rest ++ as)
  acquire <- acquireBlock this
  storesRest <- storeRest rest as
  pure (
    storeBlock (vars ++ rest ++ as) heap (pred FieldsPerBlock) ++
    storeValues vars (rest ++ as) heap (pred FieldsPerBlock) ++
    acquire ++
    storesRest)

store : (vs : Arguments d) -> (as : Arguments d) -> State Integer (List Code)
store [] as =
  pure (
    loadImmediate (freshLocation1 as) 0 ::
    [])
store vs as = do
  let vars = take FieldsPerBlock vs
      rest = drop FieldsPerBlock vs
      this = freshLocation1 (rest ++ as)
  acquire <- acquireBlock this
  storesRest <- storeRest rest as
  pure (
    storeValues vars (rest ++ as) heap FieldsPerBlock ++
    acquire ++
    storesRest)


loadRest : (xs : Arguments d) -> (as : Arguments d) -> State Integer ((List Code), Bool)
loadRest [] as =
  pure ([], False)
loadRest xs as =
  let vars = take FieldsPerBlock xs
      rest = drop FieldsPerBlock xs
      this = freshLocation1 (rest ++ as)
  in
  case this of
    Left registerThis => do
      (loadsRest, _) <- loadRest rest as
      release <- releaseBlock registerThis
      pure (
        (loadsRest ++
         release ++
         loadBlock (vars ++ rest ++ as) registerThis (pred FieldsPerBlock) ++
         loadBinders vars (rest ++ as) registerThis (pred FieldsPerBlock),
         False))
    Right positionThis => do
      (loadsRest, registerFreed) <- loadRest rest as
      release <- releaseBlock return1
      pure (
        (loadsRest ++
         -- the first time a memory block is in a spill position, we free a register for it
         -- and only restore the register after the last load in `load`, since all memory
         -- blocks after this one will also be in a spill position
         (if (not registerFreed)
           then MOVS return1 stack (stackOffset spillTemp) :: []
           else []) ++
         MOVL return1 stack (stackOffset positionThis) ::
         release ++
         loadBlock (vars ++ rest ++ as) return1 (pred FieldsPerBlock) ++
         loadBinders vars (rest ++ as) return1 (pred FieldsPerBlock),
         True))

load : (xs : Arguments d) -> (as : Arguments d) -> State Integer (List Code)
load [] as =
  pure []
load xs as =
  let vars = take FieldsPerBlock xs
      rest = drop FieldsPerBlock xs
      this = freshLocation1 (rest ++ as)
  in
  case this of
    Left registerThis => do
      (loadsRest, _) <- loadRest rest as
      release <- releaseBlock registerThis
      pure (
        loadsRest ++
        release ++
        loadBinders vars (rest ++ as) registerThis FieldsPerBlock)
    Right positionThis => do
      (loadsRest, registerFreed) <- loadRest rest as
      release <- releaseBlock return1
      pure (
        loadsRest ++
        -- free register for memory block if not already done
        (if (not registerFreed)
          then MOVS return1 stack (stackOffset spillTemp) :: []
          else []) ++
        MOVL return1 stack (stackOffset positionThis) ::
        release ++
        loadBinders vars (rest ++ as) return1 FieldsPerBlock ++
        -- restore register freed for memory block
        MOVL return1 stack (stackOffset spillTemp) ::
        [])


codeTable : Nat -> Integer -> Integer -> List Code
codeTable 0 _ _ = []
codeTable (S k) labBase labBranch =
  JMPLN ("lab" ++ show labBase ++ "b" ++ show labBranch) ::
  codeTable k labBase (1 + labBranch)


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

updateReferenceCount : Temporary -> Nat -> State Integer (List Code)
updateReferenceCount x Z =
  eraseBlock x
updateReferenceCount x (S Z) =
  pure []
updateReferenceCount x (S (S n)) =
  shareBlockN x (S n)

codeWeakeningContraction : {as : Arguments d} -> Table (\b => List (Symbol bs b)) as -> State Integer (List Code)
codeWeakeningContraction [] =
  pure []
codeWeakeningContraction {as = Ext t :: as} (r :: rs) =
  codeWeakeningContraction {as} rs
codeWeakeningContraction {as = a :: as} (r :: rs) = do
  update <- updateReferenceCount (symbolLocation1 (a :: as) Z) (length r)
  updateRest <- codeWeakeningContraction {as} rs
  pure (
    update ++
    updateRest)

connections : {as : Arguments d} -> {bs : Arguments d} -> Table (\b => List (Symbol bs b)) as -> Vect (RegisterNum + SpillNum) (List Temporary)
connections [] =
  replicate (RegisterNum + SpillNum) []
connections {as = Ext t :: as} {bs} (xs :: xss) =
  replaceAt (fuseFins (symbolLocation2 (Ext t :: as) Z)) (map (symbolLocation2 bs) xs) (
  connections {as} {bs} xss)
connections {as = a :: as} {bs} (xs :: xss) =
  replaceAt (fuseFins (symbolLocation1 (a :: as) Z)) (map (symbolLocation1 bs) xs) (
  replaceAt (fuseFins (symbolLocation2 (a :: as) Z)) (map (symbolLocation2 bs) xs) (
  connections {as} {bs} xss))


savesForSyscallThreshold : Register -> Nat
savesForSyscallThreshold r = (minus (finToNat r) reserved) + 1

savesForSyscallCond1 : Nat -> Nat -> Maybe Temporary -> Maybe Temporary
savesForSyscallCond1 firstFree offset mtemporary =
  if firstFree >= savesForSyscallThreshold clobbered
  then Just (unsafeFitsOrSpill (firstFree + offset))
  else mtemporary

savesForSyscallCond2 : Nat -> Nat -> Nat -> Temporary -> Maybe Temporary
savesForSyscallCond2 firstFree threshold argnumS temporary =
  if firstFree < threshold
  then Nothing
  else savesForSyscallCond1 firstFree argnumS (Just temporary)

savesForSyscall : (argnum : Nat) -> Nat -> Vect (S argnum) (Maybe Temporary)
savesForSyscall Z firstFree =
  savesForSyscallCond1 firstFree (S Z) Nothing :: []
savesForSyscall (S k) firstFree =
  snoc
  (savesForSyscall k firstFree)
  (savesForSyscallCond2 firstFree (savesForSyscallThreshold (arg k)) (S (S k)) (argSpill k))

saveFromRegisters : Vect n Register -> Vect n (Maybe Temporary) -> List Code
saveFromRegisters [] [] = []
saveFromRegisters (r :: rs) (Nothing :: mts) = saveFromRegisters rs mts
saveFromRegisters (r :: rs) (Just t :: mts) = moveFromRegister t r :: saveFromRegisters rs mts

restoreFromRegisters : Vect n Register -> Vect n (Maybe Temporary) -> List Code
restoreFromRegisters [] [] = []
restoreFromRegisters (r :: rs) (Nothing :: mts) = restoreFromRegisters rs mts
restoreFromRegisters (r :: rs) (Just t :: mts) = moveToRegister r t :: restoreFromRegisters rs mts


mutual
  codeStatement : {ts : Vect d (Signature d)} -> Statement ts ps as -> State Integer (List Code)
  codeStatement (Substitute {as} {bs} xs s) = do
    let targets = transpose {as} {bs} xs
    weakenContract <- codeWeakeningContraction targets
    rest <- codeStatement s
    pure (
      weakenContract ++
      codeExchange (connections targets) ++
      rest)
  codeStatement (Jump l) = pure (
    JMPL ("lab" ++ show (symbolToNat l)) ::
    [])
  codeStatement (Let {t} {as} {bs} i s) = do
    let tag = symbolLocation2 (Pro t :: as) Z
    stores <- store bs as
    rest <- codeStatement s
    pure (
      stores ++
      loadImmediate tag (jumpLength (symbolToNat i)) ::
      rest)
  codeStatement (Switch {t} {as} ss) = do
    let tag = symbolLocation2 (Pro t :: as) Z
    freshLab <- get
    _ <- put (1 + freshLab)
    code <- codeClauses as (index t ts) ss freshLab 0
    let numBranches = length code
    pure (
      LEAL temp ("lab" ++ show freshLab) ::
      (if numBranches <= 1
        then []
        else addToRegister temp tag :: []) ++
      JMP temp ::
      LAB ("lab" ++ show freshLab) ::
      (if numBranches <= 1
        then []
        else codeTable numBranches freshLab 0) ++
      concat code)
  codeStatement (New {t} {as} {bs} ss s) = do
    let tab = symbolLocation2 (Con t :: as) Z
    stores <- store bs as
    freshLab <- get
    _ <- put (1 + freshLab)
    rest <- codeStatement s
    code <- codeMethods bs (index t ts) ss freshLab 0
    let numBranches = length code
    pure (
      stores ++
      loadLabel tab ("lab" ++ show freshLab) ++
      rest ++
      LAB ("lab" ++ show freshLab) ::
      (if numBranches <= 1
        then []
        else codeTable numBranches freshLab 0) ++
      concat code)
  codeStatement (Invoke {t} {as} i) = do
    let tab = symbolLocation2 (Con t :: as) Z
    pure (
      if length (index t ts) <= 1
        then jump tab
        else addAndJump tab (jumpLength (symbolToNat i)))
  codeStatement (Extern Restart [] []) = pure (
    JMPL "lab0" ::
    [])
  codeStatement (Extern Return [x] []) = pure (
    moveToRegister return2 (symbolLocation2 as x) ::
    JMPL "cleanup" ::
    [])
  codeStatement (Extern (Literal n) [] [s]) = do
    code <- codeStatement s
    pure (
      loadImmediate (symbolLocation2 (Ext "Int" :: as) Z) n ::
      code)
  codeStatement (Extern Add [x, y] [s]) = do
    code <- codeStatement s
    pure (
      add (symbolLocation2 (Ext "Int" :: as) Z) (symbolLocation2 as x) (symbolLocation2 as y) ++
      code)
  codeStatement (Extern Sub [x, y] [s]) = do
    code <- codeStatement s
    pure (
      sub (symbolLocation2 (Ext "Int" :: as) Z) (symbolLocation2 as x) (symbolLocation2 as y) ++
      code)
  codeStatement (Extern Mul [x, y] [s]) = do
    code <- codeStatement s
    pure (
      mul (symbolLocation2 (Ext "Int" :: as) Z) (symbolLocation2 as x) (symbolLocation2 as y) ++
      code)
  codeStatement (Extern Rem [x, y] [s]) = do
    let secondFree = freshLocation2 as
    code <- codeStatement s
    pure (
      MOV temp return2 ::
      moveFromRegister secondFree return1 ::
      moveToRegister return1 (symbolLocation2 as x) ::
      div (symbolLocation2 as y) ++
      moveToRegister return1 secondFree ::
      moveFromRegister secondFree return2 ::
      MOV return2 temp ::
      code)
  codeStatement (Extern IfZero [x] [z, s]) = do
    freshLab <- get
    _ <- put (1 + freshLab)
    elseBranch <- codeStatement s
    thenBranch <- codeStatement z
    pure (
      compareImmediate (symbolLocation2 as x) 0 ::
      JEL ("lab" ++ show freshLab) ::
      elseBranch ++
      LAB ("lab" ++ show freshLab) ::
      thenBranch)
  codeStatement (Extern IfEqual [x, y] [z, s]) = do
    freshLab <- get
    _ <- put (1 + freshLab)
    elseBranch <- codeStatement s
    thenBranch <- codeStatement z
    pure (
      compare (symbolLocation2 as x) (symbolLocation2 as y) ++
      JEL ("lab" ++ show freshLab) ::
      elseBranch ++
      LAB ("lab" ++ show freshLab) ::
      thenBranch)
  codeStatement (Extern IfLess [x, y] [z, s]) = do
    freshLab <- get
    _ <- put (1 + freshLab)
    elseBranch <- codeStatement s
    thenBranch <- codeStatement z
    pure (
      compare (symbolLocation2 as x) (symbolLocation2 as y) ++
      JLTL ("lab" ++ show freshLab) ::
      elseBranch ++
      LAB ("lab" ++ show freshLab) ::
      thenBranch)
  codeStatement (Extern MMapAnonymousPage [] [s]) = do
    let firstFree = 2 * (length as)
        saveSysNR = savesForSyscallCond2 firstFree (savesForSyscallThreshold sysNR) Z sysNRSpill
        saveTo = savesForSyscall 6 firstFree
        toSave = take (S 6) registersToSave
    code <- codeStatement s
    pure (
      saveFromRegisters [sysNR] [saveSysNR] ++
      saveFromRegisters toSave saveTo ++
      MOVI sysNR mmap ::
      MOVI (arg 0) 0 ::
      MOVI (arg 1) pageSize ::
      MOVI (arg 2) (protRead + protWrite) ::
      MOVI (arg 3) (mapPrivate + mapAnonymous) ::
      MOVI (arg 4) (-1) ::
      MOVI (arg 5) 0 ::
      SYSCALL ::
      restoreFromRegisters toSave saveTo ++
      moveFromRegister (symbolLocation2 (Ext "Page" :: as) Z) return1 ::
      restoreFromRegisters [sysNR] [saveSysNR] ++
      code)
  codeStatement (Extern MUnmapPage [x] [s]) = do
    let firstFree = 2 * (length as)
        saveSysNR = savesForSyscallCond2 firstFree (savesForSyscallThreshold sysNR) Z sysNRSpill
        saveTo = savesForSyscall 2 firstFree
        toSave = take (S 2) registersToSave
    code <- codeStatement s
    pure (
      saveFromRegisters [sysNR] [saveSysNR] ++
      saveFromRegisters toSave saveTo ++
      MOVI sysNR munmap ::
      moveToRegister (arg 0) (symbolLocation2 as x) ::
      MOVI (arg 1) pageSize ::
      SYSCALL ::
      restoreFromRegisters toSave saveTo ++
      restoreFromRegisters [sysNR] [saveSysNR] ++
      code)
  codeStatement (Extern GetBufferByte [b, o] [s]) = do
    code <- codeStatement s
    pure (
      loadByte (symbolLocation2 (Ext "Int" :: as) Z) (symbolLocation2 as b) (symbolLocation2 as o) ++
      code)
  codeStatement (Extern SetBufferByte [b, o, v] [s]) = do
    code <- codeStatement s
    pure (
      storeByte (symbolLocation2 as v) (symbolLocation2 as b) (symbolLocation2 as o) ++
      code)
  codeStatement (Extern ReadStdin [b, c] [s]) = do
    let firstFree = 2 * (length as)
        saveSysNR = savesForSyscallCond2 firstFree (savesForSyscallThreshold sysNR) Z sysNRSpill
        saveTo = savesForSyscall 3 firstFree
        toSave = take (S 3) registersToSave
    code <- codeStatement s
    pure (
      saveFromRegisters [sysNR] [saveSysNR] ++
      saveFromRegisters toSave saveTo ++
      moveToRegister (arg 1) (symbolLocation2 as b) ::
      moveToRegister (arg 2) (symbolLocation2 as c) ::
      MOVI sysNR read ::
      MOVI (arg 0) stdin ::
      SYSCALL ::
      restoreFromRegisters toSave saveTo ++
      moveFromRegister (symbolLocation2 (Ext "Int" :: as) Z) return1 ::
      restoreFromRegisters [sysNR] [saveSysNR] ++
      code)
  codeStatement (Extern WriteStdout [b, c] [s]) = do
    let firstFree = 2 * (length as)
        saveSysNR = savesForSyscallCond2 firstFree (savesForSyscallThreshold sysNR) Z sysNRSpill
        saveTo = savesForSyscall 3 firstFree
        toSave = take (S 3) registersToSave
    code <- codeStatement s
    pure (
      saveFromRegisters [sysNR] [saveSysNR] ++
      saveFromRegisters toSave saveTo ++
      moveToRegister (arg 1) (symbolLocation2 as b) ::
      moveToRegister (arg 2) (symbolLocation2 as c) ::
      MOVI sysNR write ::
      MOVI (arg 0) stdout ::
      SYSCALL ::
      restoreFromRegisters toSave saveTo ++
      moveFromRegister (symbolLocation2 (Ext "Int" :: as) Z) return1 ::
      restoreFromRegisters [sysNR] [saveSysNR] ++
      code)
  codeStatement (Extern _ _ _) = do
    believe_me "total"

  codeClause : {ts : Vect d (Signature d)} -> (as : Arguments d) -> (cs : Arguments d) -> Statement ts ps (cs ++ as) -> State Integer (List Code)
  codeClause as cs s = do
    loads <- load cs as
    code <- codeStatement s
    pure (
      loads ++
      code)

  codeMethod : {ts : Vect d (Signature d)} -> (as : Arguments d) -> (cs : Arguments d) -> Statement ts ps (as ++ cs) -> State Integer (List Code)
  codeMethod as cs s = do
    loads <- load as cs
    code <- codeStatement s
    pure (
      loads ++
      code)

  codeClauses : {ts : Vect d (Signature d)} -> (as : Arguments d) -> (us : List (Arguments d)) -> Clauses ts ps as us -> Integer -> Integer -> State Integer (Vect (length us) (List Code))
  codeClauses as [] [] _ _ =
    pure []
  codeClauses as (cs :: us) (s :: ss) baseLab branchLab = do
    clause <- codeClause as cs s
    table <- codeClauses as us ss baseLab (1 + branchLab)
    pure (
      (LAB ("lab" ++ show baseLab ++ "b" ++ show branchLab) :: clause) ::
      table)

  codeMethods : {ts : Vect d (Signature d)} -> (as : Arguments d) -> (us : List (Arguments d)) -> Methods ts ps as us -> Integer -> Integer -> State Integer (Vect (length us) (List Code))
  codeMethods as [] [] _ _ =
    pure []
  codeMethods as (cs :: us) (s :: ss) baseLab branchLab = do
    method <- codeMethod as cs s
    table <- codeMethods as us ss baseLab (1 + branchLab)
    pure (
      (LAB ("lab" ++ show baseLab ++ "b" ++ show branchLab) :: method) ::
      table)


translate : {ts : Vect d (Signature d)} -> Table (Statement ts ps) os -> State Integer (Vect (length os) (List Code))
translate [] = pure []
translate (s :: ls) = do
  code <- codeStatement s
  rest <- translate ls
  pure (code :: rest)


assemble : Nat -> Vect n (List Code) -> List Code
assemble l [] = []
assemble l (is :: ss) = (LAB ("lab" ++ show l) :: is) ++ assemble (S l) ss


public export
compile : {ps : List (Arguments d)} -> {ts : Vect d (Signature d)} -> Program ts ps -> (List Code, Nat)
compile program = do
  let mainArgs = case head' ps of
                   Nothing => assert_total (idris_crash "there is no program")
                   Just mainArgs => mainArgs
      sections = evalState (natToInteger (length program)) (translate program)
  (assemble 0 sections, length mainArgs)
