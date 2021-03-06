{- |
Module      :  Text.ParserCombinators.Parsec.Combinator
Copyright   :  (c) Daan Leijen 1999-2001
License     :  BSD-style (see the file LICENSE)

Maintainer  :  Christian Maeder <chr.maeder@web.de>
Stability   :  provisional
Portability :  portable

Commonly used generic combinators
-}

module Text.ParserCombinators.Parsec.Combinator
  ( between
  , chainl
  , chainl1
  , chainr
  , chainr1
  , choice
  , count
  , endBy
  , endBy1
  , eof
  , many1
  , notFollowedBy
  , option
  , optionMaybe
  , optional
  , sepBy
  , sepBy1
  , sepEndBy
  , sepEndBy1
  , skipMany1
    -- tricky combinators
  , anyToken
  , lookAhead
  , manyTill
  ) where

import Control.Monad
import Text.ParserCombinators.Parsec.Prim


{- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
until one of them succeeds. Returns the value of the succeeding
parser. -}
choice :: [GenParser tok st a] -> GenParser tok st a
choice = foldr (<|>) mzero

{- | @option x p@ tries to apply parser @p@. If @p@ fails without
consuming input, it returns the value @x@, otherwise the value
returned by @p@.

>  priority  = option 0 (digitToInt <$> digit) -}
option :: a -> GenParser tok st a -> GenParser tok st a
option x p = p <|> return x

{- | @optionMaybe p@ tries to apply parser @p@.  If @p@ fails without
consuming input, it return 'Nothing', otherwise it returns
'Just' the value returned by @p@. -}
optionMaybe :: GenParser tok st a -> GenParser tok st (Maybe a)
optionMaybe p = option Nothing (fmap Just p)

{- | @optional p@ tries to apply parser @p@.  It will parse @p@ or nothing.
It only fails if @p@ fails after consuming input. It discards the result
of @p@. -}
optional :: GenParser tok st a -> GenParser tok st ()
optional p = () <$ p <|> return ()

{- | @between open close p@ parses @open@, followed by @p@ and @close@.
Returns the value returned by @p@.

>  braces  = between (symbol "{") (symbol "}") -}
between :: GenParser tok st open -> GenParser tok st close
            -> GenParser tok st a -> GenParser tok st a
between open close p = open *> p <* close

{- | @skipMany1 p@ applies the parser @p@ /one/ or more times, skipping
its result. -}
skipMany1 :: GenParser tok st a -> GenParser tok st ()
skipMany1 p = p *> skipMany p

{- | @many1 p@ applies the parser @p@ /one/ or more times. Returns a
list of the returned values of @p@.

>  word  = many1 letter -}
many1 :: GenParser tok st a -> GenParser tok st [a]
many1 p = do
  x <- p
  xs <- many p
  return (x : xs)

{- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
by @sep@. Returns a list of values returned by @p@.

>  commaSep p  = p `sepBy` (symbol ",") -}
sepBy :: GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
sepBy p sep = sepBy1 p sep <|> return []

{- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
by @sep@. Returns a list of values returned by @p@. -}
sepBy1 :: GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep *> p)
  return (x : xs)

{- | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
separated and optionally ended by @sep@. Returns a list of values
returned by @p@. -}
sepEndBy1 :: GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
sepEndBy1 p sep = do
  x <- p
  do
      xs <- sep *> sepEndBy p sep
      return (x : xs)
    <|> return [x]

{- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
separated and optionally ended by @sep@, ie. haskell style
statements. Returns a list of values returned by @p@.

>  haskellStatements  = haskellStatement `sepEndBy` semi -}
sepEndBy :: GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
sepEndBy p sep = sepEndBy1 p sep <|> return []


{- | @endBy1 p sep@ parses /one/ or more occurrences of @p@, seperated
and ended by @sep@. Returns a list of values returned by @p@. -}
endBy1 :: GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
endBy1 p sep = many1 (p <* sep)

{- | @endBy p sep@ parses /zero/ or more occurrences of @p@, seperated
and ended by @sep@. Returns a list of values returned by @p@.

>   cStatements  = cStatement `endBy` semi -}
endBy :: GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
endBy p sep = many (p <* sep)

{- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
equal to zero, the parser equals to @return []@. Returns a list of
@n@ values returned by @p@. -}
count :: Int -> GenParser tok st a -> GenParser tok st [a]
count n p | n <= 0 = return []
                    | otherwise = replicateM n p


{- | @chainr p op x@ parser /zero/ or more occurrences of @p@,
separated by @op@ Returns a value obtained by a /right/ associative
application of all functions returned by @op@ to the values returned
by @p@. If there are no occurrences of @p@, the value @x@ is
returned. -}
chainr :: GenParser tok st a -> GenParser tok st (a -> a -> a) -> a
  -> GenParser tok st a
chainr p op x = chainr1 p op <|> return x

{- | @chainl p op x@ parser /zero/ or more occurrences of @p@,
separated by @op@. Returns a value obtained by a /left/ associative
application of all functions returned by @op@ to the values returned
by @p@. If there are zero occurrences of @p@, the value @x@ is
returned. -}
chainl :: GenParser tok st a -> GenParser tok st (a -> a -> a) -> a
  -> GenParser tok st a
chainl p op x = chainl1 p op <|> return x

{- | @chainl1 p op x@ parser /one/ or more occurrences of @p@,
separated by @op@ Returns a value obtained by a /left/ associative
application of all functions returned by @op@ to the values returned
by @p@. This parser can for example be used to eliminate left
recursion which typically occurs in expression grammars.

>  expr    = term   `chainl1` addop
>  term    = factor `chainl1` mulop
>  factor  = parens expr <|> integer
>
>  mulop   =   symbol "*" *> return (*)
>          <|> symbol "/" *> return (div)
>
>  addop   =   symbol "+" *> return (+)
>          <|> symbol "-" *> return (-) -}
chainl1 :: GenParser tok st a -> GenParser tok st (a -> a -> a)
  -> GenParser tok st a
chainl1 p op = do
  x <- p
  rest x
    where
      rest x = do
          f <- op
          y <- p
          rest (f x y)
        <|> return x

{- | @chainr1 p op x@ parser /one/ or more occurrences of |p|,
separated by @op@ Returns a value obtained by a /right/ associative
application of all functions returned by @op@ to the values returned
by @p@. -}
chainr1 :: GenParser tok st a -> GenParser tok st (a -> a -> a)
  -> GenParser tok st a
chainr1 p op = scan
                    where
                      scan = do
                        x <- p
                        rest x

                      rest x = do
                          f <- op
                          f x <$> scan
                        <|> return x

{- ---------------------------------------------------------
Tricky combinators
--------------------------------------------------------- -}

{- | The parser @anyToken@ accepts any kind of token. It is for example
used to implement 'eof'. Returns the accepted token. -}
anyToken :: Show tok => GenParser tok st tok
anyToken = tokenPrim show (\ pos _tok _toks -> pos) Just

{- | This parser only succeeds at the end of the input. This is not a
primitive parser but it is defined using 'notFollowedBy'.

>  eof  = notFollowedBy anyToken <?> "end of input" -}
eof :: Show tok => GenParser tok st ()
eof = notFollowedBy anyToken <?> "end of input"

{- | @notFollowedBy p@ only succeeds when parser @p@ fails. This parser
does not consume any input. This parser can be used to implement the
\'longest match\' rule. For example, when recognizing keywords (for
example @let@), we want to make sure that a keyword is not followed
by a legal identifier character, in which case the keyword is
actually an identifier (for example @lets@). We can program this
behaviour as follows:

>  keywordLet  = try (string "let" <* notFollowedBy alphaNum) -}
notFollowedBy :: Show a => GenParser tok st a -> GenParser tok st ()
notFollowedBy p = try $ do
    c <- p
    unexpected (show c)
  <|> return ()

{- | @manyTill p end@ applies parser @p@ /zero/ or more times until
parser @end@ succeeds. Returns the list of values returned by @p@.
This parser can be used to scan comments:

>  simpleComment   = string "<!--" *> manyTill anyChar (try (string "-->"))

Note the overlapping parsers @anyChar@ and @string \"-->\"@, and
therefore the use of the 'try' combinator. -}
manyTill :: GenParser tok st a -> GenParser tok st end -> GenParser tok st [a]
manyTill p end = scan where
  scan = [] <$ end <|> do
    x <- p
    xs <- scan
    return (x : xs)
