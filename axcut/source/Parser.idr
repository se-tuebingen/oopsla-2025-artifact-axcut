module Parser

import Types
import Tree
import Syntax

import Data.String


parseIdentifier : Tree -> Either String Identifier
parseIdentifier (Atom x) = pure x
parseIdentifier (Node xs) = Left "invalid identifier"

parseIdentifiers : List Tree -> Either String (List Identifier)
parseIdentifiers [] = do
  pure []
parseIdentifiers (Atom x :: xs) = do
  xs <- parseIdentifiers xs
  pure (x :: xs)
parseIdentifiers _ = do
  Left "invalid identifiers"

parseTag : String -> Either String Nat
parseTag n = case parsePositive n of
  Just m => pure m
  Nothing => Left "invalid tag"

parsePolarity : String -> Either String Polarity
parsePolarity "pro" = pure Pro
parsePolarity "con" = pure Con
parsePolarity "ext" = pure Ext
parsePolarity _ = Left "invalid polarity"

parseTpes : List Tree -> Either String (List (Polarity, Identifier))
parseTpes (Atom p :: t :: rest) = do
  p <- parsePolarity p
  t <- parseIdentifier t
  rest <- parseTpes rest
  pure ((p, t) :: rest)
parseTpes [] = do
  pure []
parseTpes _ = Left "invalid types"

parseArguments : Tree -> Either String (List (Polarity, Identifier))
parseArguments (Node xs) = parseTpes xs
parseArguments (Atom _) = Left "invalid arguments"


mutual
  parseStatement : List Tree -> Either String Statement
  parseStatement (Node (Atom "do" :: a :: s) :: c) = do
    parseStatement [Node (s ++ [Node (a :: c)])]
  parseStatement [Node [Atom "substitute", Node bs, c]] = do
    vals <- parseIdentifiers bs
    cont <- parseBlock c
    pure (Substitute vals cont)
  parseStatement [Node [Atom "jump", Atom l]] = do
    pure (Jump l)
  parseStatement (Node [Atom "let", Atom x, t, Atom i, Node bs] :: s) = do
    type <- parseIdentifier t
    indx <- parseTag i
    args <- parseIdentifiers bs
    rest <- parseStatement s
    pure (Let x type indx args rest)
  parseStatement [Node (Atom "switch" :: Atom x :: cs)] = do
    cont <- for cs parseBlock
    pure (Switch x cont)
  parseStatement (Node (Atom "new" :: Atom x :: t :: Node bs :: cs) :: s) = do
    type <- parseIdentifier t
    args <- parseIdentifiers bs
    cont <- for cs parseBlock
    rest <- parseStatement s
    pure (New x type args cont rest)
  parseStatement [Node [Atom "invoke", Atom x, Atom i]] = do
    indx <- parseTag i
    pure (Invoke x indx)
  parseStatement [Node (Atom "extern" :: Atom name :: Node bs :: cs)] = do
    args <- parseIdentifiers bs
    cont <- for cs parseBlock
    pure (Extern name args cont)
  parseStatement _ = do
    Left "invalid statement"

  parseBlock : Tree -> Either String Block
  parseBlock (Node ((Node vs) :: ss)) = do
    prms <- for vs parseIdentifier
    body <- parseStatement ss
    pure (Binder prms body)
  parseBlock _ = do
    Left "invalid clause"

  parseExtern : String -> List Tree -> List Tree -> Either String Statement
  parseExtern name xs cs = do
    args <- parseIdentifiers xs
    cont <- for cs parseBlock
    pure (Extern name args cont)


public export
parse : List Tree -> Either String Program
parse (tree :: trees) = case tree of
  Node (Atom "define" :: Atom name :: Node vars :: args :: body) => do
    vars <- for vars parseIdentifier
    args <- parseArguments args
    body <- parseStatement body
    rest <- parse trees
    pure (Define name vars args body rest)
  Node (Atom "signature" :: Atom name :: sign) => do
    sign <- for sign parseArguments
    rest <- parse trees
    pure (Typ name sign rest)
  Node (Atom "import" :: Atom name :: args :: blos) => do
    args <- parseArguments args
    blos <- for blos parseArguments
    rest <- parse trees
    pure (Import name args blos rest)
  _ => do
    Left "invalid definition"
parse [] = do
  pure End

