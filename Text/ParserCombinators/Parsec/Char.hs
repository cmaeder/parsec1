{- |
Module      :  Text.ParserCombinators.Parsec.Char
Copyright   :  (c) Daan Leijen 1999-2001
License     :  BSD-style (see the file LICENSE)

Maintainer  :  Christian Maeder <chr.maeder@web.de>
Stability   :  provisional
Portability :  portable

Commonly used character parsers
-}

module Text.ParserCombinators.Parsec.Char
  ( CharParser
  , alphaNum
  , anyChar
  , char
  , digit
  , hexDigit
  , letter
  , lower
  , newline
  , noneOf
  , octDigit
  , oneOf
  , satisfy
  , space
  , spaces
  , string
  , tab
  , upper
  ) where

import Data.Char

import Text.ParserCombinators.Parsec.Pos (updatePosChar, updatePosString)
import Text.ParserCombinators.Parsec.Prim

{- ---------------------------------------------------------
Type of character parsers
--------------------------------------------------------- -}
type CharParser st a = GenParser Char st a

{- ---------------------------------------------------------
Character parsers
--------------------------------------------------------- -}

{- | @oneOf cs@ succeeds if the current character is in the supplied
list of characters @cs@. Returns the parsed character. See also
'satisfy'.

>   vowel  = oneOf "aeiou" -}
oneOf :: [Char] -> CharParser st Char
oneOf cs = satisfy (`elem` cs)

{- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
character /not/ in the supplied list of characters @cs@. Returns the
parsed character.

>  consonant = noneOf "aeiou" -}
noneOf :: [Char] -> CharParser st Char
noneOf cs = satisfy (`notElem` cs)

-- | Skips /zero/ or more white space characters. See also 'skipMany'.
spaces :: CharParser st ()
spaces = skipMany space <?> "white space"

{- | Parses a white space character (any character which satisfies 'isSpace')
Returns the parsed character. -}
space :: CharParser st Char
space = satisfy isSpace <?> "space"

-- | Parses a newline character (\'\\n\'). Returns a newline character.
newline :: CharParser st Char
newline = char '\n' <?> "new-line"

-- | Parses a tab character (\'\\t\'). Returns a tab character.
tab :: CharParser st Char
tab = char '\t' <?> "tab"

{- | Parses an upper case letter  according to 'isUpper'.
Returns the parsed character. -}
upper :: CharParser st Char
upper = satisfy isUpper <?> "uppercase letter"

{- | Parses a lower case character according to 'isLower'.
Returns the parsed character. -}
lower :: CharParser st Char
lower = satisfy isLower <?> "lowercase letter"

{- | Parses an alphabetic or numeric Unicode characters according to
'isAlphaNum'. Returns the parsed character. -}
alphaNum :: CharParser st Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"

{- | Parses an alphabetic Unicode characters according to 'isAlpha'.
Returns the parsed character. -}
letter :: CharParser st Char
letter = satisfy isAlpha <?> "letter"

{- | Parses a digit (\'0\' ... \'9\') according to 'isDigit'.
Returns the parsed character. -}
digit :: CharParser st Char
digit = satisfy isDigit <?> "digit"

{- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
\'f\' or \'A\' and \'F\'). Returns the parsed character. -}
hexDigit :: CharParser st Char
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"

{- | Parses an octal digit (\'0\' ... \'7\') according to 'isOctDigit'.
Returns the parsed character. -}
octDigit :: CharParser st Char
octDigit = satisfy isOctDigit <?> "octal digit"

{- | @char c@ parses a single character @c@. Returns the parsed
character (i.e. @c@).

>  semiColon  = char ';' -}
char :: Char -> CharParser st Char
char c = satisfy (== c) <?> show [c]

-- | This parser succeeds for any character. Returns the parsed character.
anyChar :: CharParser st Char
anyChar = satisfy (const True)

{- ---------------------------------------------------------
Primitive character parsers
--------------------------------------------------------- -}

{- | The parser @satisfy f@ succeeds for any character for which the
supplied function @f@ returns 'True'. Returns the character that is
actually parsed.

>  digit     = satisfy isDigit
>  oneOf cs  = satisfy (`elem` cs) -}
satisfy :: (Char -> Bool) -> CharParser st Char
satisfy f = tokenPrim (\ c -> show [c])
                                (\ pos c _ -> updatePosChar pos c)
                                (\ c -> if f c then Just c else Nothing)

{- | @string s@ parses a sequence of characters given by @s@. Returns
the parsed string (i.e. @s@).

>  divOrMod    =   string "div"
>              <|> string "mod" -}
string :: String -> CharParser st String
string = tokens show updatePosString
