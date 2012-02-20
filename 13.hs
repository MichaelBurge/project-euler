import Control.Applicative
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Digits

dataFile = endBy line eol
line :: Parser Integer
line = read <$> many1 digit
eol = char '\n'