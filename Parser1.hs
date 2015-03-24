
import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Monad
import qualified Data.Char as Char
import qualified Data.List as List

-- | A result type for parsers
data Result a = Success a String
              | Failure String String
              deriving (Show)

-- | A parser is a mapping from String to a Result
newtype Parser a = Parser { runParser :: String -> Result a }

instance Functor Result where
  fmap f (Success a r) = Success (f a) r
  fmap _ (Failure r e) = Failure r e

instance Functor Parser where
  fmap f p = Parser $ \s -> fmap f (runParser p s)

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Success a s
  m >>= f  = Parser $ \s ->
    case runParser m s of
      Success a r -> runParser (f a) r
      Failure e r -> Failure e r

instance Alternative Parser where
  empty     = Parser $ \s -> Failure "empty: bottom" s
  p1 <|> p2 = Parser $ \s ->
    case runParser p1 s of
      Failure _ _ -> runParser p2 s
      x           -> x

-- | Accepts a specified char
char :: Char -> Parser Char
char c = Parser $ \s ->
    case s of
      (c1:cx) | c == c1   -> Success c cx
              | otherwise -> Failure "char: does not match" s
      _                   -> Failure "char: unexpected end of input" s

-- | Accepts a specified string
string :: String -> Parser String
string = mapM char

-- | Takes from input while a specified predicate is True
takeWhile :: (Char -> Bool) -> Parser String
takeWhile p = Parser $ \s -> let
    (prefix, suffix) = List.span p s
  in Success prefix suffix

between :: Parser b -> Parser a -> Parser c -> Parser a
between left middle right = left *> middle <* right

-- | Skips any amount of whitespace
skipSpaces :: Parser String
skipSpaces = takeWhile Char.isSpace

-- | Parses (f a) elements separated by (f s) (one or more)
sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p s = (:) <$> p <*> ((s *> sepBy1 p s) <|> pure [])

-- | Parses (f a) elements separated by (f s) (zero or more
sepBy :: Alternative f => f a -> f s -> f [a]
sepBy p s = sepBy1 p s <|> pure []

-- JSON

data Value = JString String
           | JObject [(String, Value)]
           | JNumber Int
            deriving (Show)

spacey :: Parser a -> Parser a
spacey p = between skipSpaces p skipSpaces

jstring :: Parser String
jstring = do
  _ <- char '\"'
  s <- takeWhile ('"' /=)
  _ <- char '\"'
  return s

jnumber :: Parser Int
jnumber = read <$> takeWhile Char.isDigit

jobject :: Parser [(String, Value)]
jobject = do
  _     <- char '{'
  pairs <- pair `sepBy` (spacey (char ','))
  _     <- char '}'
  return pairs
  where
    pair = do
      k <- spacey jstring
      _ <- char ':'
      v <- spacey jvalue
      return (k, v)

jvalue :: Parser Value
jvalue =
        (JString <$> jstring)
    <|> (JObject <$> jobject)
    <|> (JNumber <$> jnumber)

main :: IO ()
main = do
  print $ runParser jstring "\"hallo welt\""
  print $ runParser jobject "{ \"name\" : \"alex\" , \"age\" : 12345 }"
  return ()
