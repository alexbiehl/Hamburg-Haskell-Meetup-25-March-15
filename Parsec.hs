module Parse where

import Text.Parsec
import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Data.List

type Parse = Parsec String ()

------------------------------------------------------------------------------

data Email = Email
  { emailName :: String
  , emailDomain :: String
  } deriving (Show)

name :: Parse String
name = many1 alphaNum

domain :: Parse [String]
domain = (:) <$> (name <* char '.') <*> name `sepBy1` char '.'

email :: Parse Email
email = do
  n <- name
  _ <- char '@'
  d <- domain
  return $ Email n $ intercalate "." d

------------------------------------------------------------------------------

data HTML
  = Text String
  | Tag String [HTML]

instance Show HTML where
  show = unlines . showHTML

showHTML :: HTML -> [String]
showHTML (Text s) = [show s]
showHTML (Tag t ch) =
    (t ++ ":") : map indent (concatMap showHTML ch)
  where
    indent = ("  " ++)

text :: Parse HTML
text = Text <$> many1 (noneOf "<> ")

tagName :: Parse String
tagName = char '<' *> many1 letter <* char '>'

tag :: Parse HTML
tag = do
    n <- try $ lookAhead tagName
    children <- between (tagOpen n) (tagClose n) $ many html
    return $ Tag n children
  where
    tagOpen  s = void $ string $ "<" ++ s ++ ">"
    tagClose s = void $ string $ "</" ++ s ++ ">"

html :: Parse HTML
html = try text <|> try tag <?> "text or tag"

document :: Parse HTML
document = html <* eof
