module Reader

import Tree


public export
Parser : Type -> Type -> Type
Parser r a = (a -> List Char -> r) -> List Char -> (String -> r) -> r


public export
runParser : Parser (Either String a) a -> String -> Either String a
runParser p i = p (\a => \s => case s of
  [] => Right a
  _ => Left "expected eof") (unpack i) (\e => Left e)

public export
return : a -> Parser r a
return x = \k => \s => \_ => k x s

public export
(>>=) : Parser r a -> (a -> Parser r b) -> Parser r b
(>>=) p f = \k => \s => \e => p (\a => \t => f a k t e) s e

public export
next : Parser r Char
next = \k => \s => \e => case s of
  [] => e "no next char"
  (c :: cs) => k c cs

public export
peek : Parser r Char
peek = \k => \s => \e => case s of
  [] => k '\0' s
  (c :: cs) => k c s

public export
fail : String -> Parser r a
fail m = \_ => \_ => \e => e m

public export
spaces : Parser r Unit
spaces = do
  c <- peek
  if isSpace c
    then do
      _ <- next
      spaces
    else do
      return ()



mutual
  parseTree : Parser r Tree
  parseTree = do
    c <- next
    if c == '('
      then do
        ts <- parseTrees
        _ <- next
        _ <- spaces
        return (Node ts)
      else do
        a <- parseAlpha [c]
        _ <- spaces
        return (Atom a)

  parseTrees : Parser r (List Tree)
  parseTrees = do
    c <- peek
    if c == ')'
      then do
        return []
      else do
        t <- parseTree
        ts <- parseTrees
        return (t :: ts)

  parseAlpha : List Char -> Parser r String
  parseAlpha cs = do
    c <- peek
    if (not (isSpace c || c == '(' || c == ')'))
      then do
        _ <- next
        parseAlpha (c :: cs)
      else do
        return (pack (reverse cs))

parseProgram : Parser r (List Tree)
parseProgram = do
  _ <- spaces
  c <- peek
  if (c == '\0')
    then do
      return []
    else do
      t <- parseTree
      ts <- parseProgram
      return (t :: ts)


public export
read : String -> Either String (List Tree)
read input = runParser parseProgram input

