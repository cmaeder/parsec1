{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Prim
-- Copyright   :  (c) Daan Leijen 1999-2001
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
--
-- Maintainer  :  Antoine Latter <aslatter@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The primitive parser combinators.
--
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Prim
                   ( -- operators: label a parser, alternative
                     (<?>), (<|>), (*>), (<*), (<*>), (<$>), (<$)

                   -- basic types
                   , Parser, GenParser
                   , runParser, parse, parseFromFile, parseTest

                   -- primitive parsers:
                   -- instance Functor Parser     : fmap
                   -- instance Monad Parser       : return, >>=, fail
                   -- instance MonadPlus Parser   : mzero (pzero), mplus (<|>)
                   , token, tokens, tokenPrim, tokenPrimEx
                   , try, label, labels, lookAhead, unexpected, pzero

                   -- primitive because of space behaviour
                   , many, skipMany

                   -- user state manipulation
                   , getState, setState, updateState

                   -- state manipulation
                   , getPosition, setPosition
                   , getInput, setInput
                   , State(..), getParserState, setParserState
                 ) where

import Prelude
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import Control.Applicative
import Control.Monad
#if __GLASGOW_HASKELL__ >= 801
import Control.Monad.Fail
#endif
-----------------------------------------------------------
-- Operators:
-- <?>  gives a name to a parser (which is used in error messages)
-- <|>  is the choice operator
-----------------------------------------------------------
infix  0 <?>

-- | The parser @p <?> msg@ behaves as parser @p@, but whenever the
-- parser @p@ fails /without consuming any input/, it replaces expect
-- error messages with the expect error message @msg@.
--
-- This is normally used at the end of a set alternatives where we want
-- to return an error message in terms of a higher level construct
-- rather than returning all possible characters. For example, if the
-- @expr@ parser from the 'try' example would fail, the error
-- message is: '...: expecting expression'. Without the @(\<?>)@
-- combinator, the message would be like '...: expecting \"let\" or
-- letter', which is less friendly.
(<?>) :: GenParser tok st a -> String -> GenParser tok st a
p <?> msg           = label p msg

-----------------------------------------------------------
-- User state combinators
-----------------------------------------------------------

-- | Returns the current user state.
getState :: GenParser tok st st
getState = stateUser <$> getParserState

-- | @setState st@ set the user state to @st@.
setState :: st -> GenParser tok st ()
setState st     =
  () <$ updateParserState (\(State input pos _) -> State input pos st)

-- | @updateState f@ applies function @f@ to the user state. Suppose
-- that we want to count identifiers in a source, we could use the user
-- state as:
--
-- >  expr  = do{ x <- identifier
-- >            ; updateState (+1)
-- >            ; return (Id x)
-- >            }
updateState :: (st -> st) -> GenParser tok st ()
updateState f   =
  () <$ updateParserState (\(State input pos user) -> State input pos (f user))

-----------------------------------------------------------
-- Parser state combinators
-----------------------------------------------------------

-- | Returns the current source position. See also 'SourcePos'.
getPosition :: GenParser tok st SourcePos
getPosition = statePos <$> getParserState

-- | Returns the current input
getInput :: GenParser tok st [tok]
getInput = stateInput <$> getParserState

-- | @setPosition pos@ sets the current source position to @pos@.
setPosition :: SourcePos -> GenParser tok st ()
setPosition pos     =
  () <$ updateParserState (\(State input _ user) -> State input pos user)

-- | @setInput input@ continues parsing with @input@.
setInput :: [tok] -> GenParser tok st ()
setInput input      =
  () <$ updateParserState (\(State _ pos user) -> State input pos user)

-- | Returns the full parser state as a 'State' record.
getParserState      :: GenParser tok st (State tok st)
getParserState      =  updateParserState id

-- | @setParserState st@ set the full parser state to @st@.
setParserState      :: State tok st -> GenParser tok st (State tok st)
setParserState st   = updateParserState (const st)


-----------------------------------------------------------
-- Parser definition.
-- GenParser tok st a:
--  General parser for tokens of type "tok",
--  a user state "st" and a result type "a"
-----------------------------------------------------------
type Parser a           = GenParser Char () a

newtype GenParser tok st a = Parser
  { runP :: State tok st -> Consumed (Reply tok st a) }

data Consumed a         = Consumed a                --input is consumed
                        | Empty !a                  --no input is consumed

data Reply tok st a     = Ok !a !(State tok st) ParseError    --parsing succeeded with "a"
                        | Error ParseError                    --parsing failed

data State tok st       = State { stateInput :: [tok]
                                , statePos   :: !SourcePos
                                , stateUser  :: !st
                                }


-----------------------------------------------------------
-- run a parser
-----------------------------------------------------------
parseFromFile :: Parser a -> SourceName -> IO (Either ParseError a)
parseFromFile p fname = do
  input <- readFile fname
  return (parse p fname input)

-- | The expression @parseTest p input@ applies a parser @p@ against
-- input @input@ and prints the result to stdout. Used for testing
-- parsers.
parseTest :: Show a => GenParser tok () a -> [tok] -> IO ()
parseTest p input = case runParser p () "" input of
  Left err -> putStr "parse error at " >> print err
  Right x  -> print x

-- | @parse p filePath input@ runs a parser @p@ without user
-- state. The @filePath@ is only used in error messages and may be the
-- empty string. Returns either a 'ParseError' ('Left')
-- or a value of type @a@ ('Right').
--
-- >  main    = case parse numbers "" "11, 2, 43" of
-- >             Left err  -> print err
-- >             Right xs  -> print (sum xs)
-- >
-- >  numbers = commaSep integer
parse :: GenParser tok () a -> SourceName -> [tok] -> Either ParseError a
parse p = runParser p ()

-- | The most general way to run a parser. @runParser p state filePath
-- input@ runs parser @p@ on the input list of tokens @input@,
-- obtained from source @filePath@ with the initial user state @st@.
-- The @filePath@ is only used in error messages and may be the empty
-- string. Returns either a 'ParseError' ('Left') or a
-- value of type @a@ ('Right').
--
-- >  parseFromFile p fname
-- >    = do{ input <- readFile fname
-- >        ; return (runParser p () fname input)
-- >        }
runParser :: GenParser tok st a -> st -> SourceName -> [tok]
  -> Either ParseError a
runParser p st name input
    = case parserReply . runP p $ State input (initialPos name) st of
        Ok x _ _    -> Right x
        Error err   -> Left err

parserReply :: Consumed p -> p
parserReply result = case result of
  Consumed reply -> reply
  Empty reply    -> reply


-----------------------------------------------------------
-- Functor: fmap
-----------------------------------------------------------
instance Functor (GenParser tok st) where
  fmap f p  = parsecMap f p

parsecMap :: (a -> b) -> GenParser tok st a -> GenParser tok st b
parsecMap f (Parser p) = Parser $ \state -> case p state of
    Consumed reply -> Consumed $ mapReply reply
    Empty    reply -> Empty $ mapReply reply
  where
    mapReply reply = case reply of
      Ok x state err -> let fx = f x in seq fx $ Ok fx state err
      Error err -> Error err

-----------------------------------------------------------
-- Monad: return, sequence (>>=) and fail
-----------------------------------------------------------
instance Monad (GenParser tok st) where
  return = parsecReturn
  (>>=) = parsecBind
  (>>) = (*>)

#if __GLASGOW_HASKELL__ >= 801
instance MonadFail (GenParser tok st) where
#endif
  fail = parsecFail

instance Applicative (GenParser tok st) where
  pure = parsecReturn
  (<*>) = ap
  p1 *> p2 = p1 >>= const p2
  p1 <* p2 = p1 >>= (<$ p2)

parsecReturn :: a -> GenParser tok st a
parsecReturn x = Parser $ \state -> Empty . Ok x state $unknownError state

parsecBind :: GenParser tok st a -> (a -> GenParser tok st b)
  -> GenParser tok st b
parsecBind (Parser p) f = Parser $ \state -> case p state of
  Consumed reply1 -> Consumed $ case reply1 of
    Ok x state1 err1 -> case runP (f x) state1 of
      Empty reply2    -> mergeErrorReply err1 reply2
      Consumed reply2 -> reply2
    Error err1       -> Error err1
  Empty reply1 -> case reply1 of
    Ok x state1 err1 -> case runP (f x) state1 of
      Empty reply2 -> Empty (mergeErrorReply err1 reply2)
      other        -> other
    Error err1       -> Empty (Error err1)

mergeErrorReply :: ParseError -> Reply tok st a -> Reply tok st a
mergeErrorReply err1 reply = case reply of
  Ok x state err2 -> Ok x state (mergeError err1 err2)
  Error err2      -> Error (mergeError err1 err2)

parsecFail :: String -> GenParser tok st a
parsecFail msg =
  Parser $ Empty . Error . newErrorMessage (Message msg) . statePos

-----------------------------------------------------------
-- MonadPlus: alternative (mplus) and mzero
-----------------------------------------------------------
instance MonadPlus (GenParser tok st) where
  mzero         = parsecZero
  mplus p1 p2   = parsecPlus p1 p2

instance Alternative (GenParser tok st) where
  (<|>) = mplus
  empty = mzero
  many = manyAux

pzero :: GenParser tok st a
pzero = parsecZero

-- | @parsecZero@ always fails without consuming any input. @parsecZero@ is defined
-- equal to the 'mzero' member of the 'MonadPlus' class and to the 'Control.Applicative.empty' member
-- of the 'Control.Applicative.Applicative' class.
parsecZero :: GenParser tok st a
parsecZero = Parser $ Empty . Error . unknownError

parsecPlus :: GenParser tok st a -> GenParser tok st a -> GenParser tok st a
parsecPlus (Parser p1) (Parser p2) = Parser $ \state -> case p1 state of
  Empty (Error err) -> case p2 state of
    Empty reply -> Empty $ mergeErrorReply err reply
    consumed -> consumed
  other -> other

-- | The parser @try p@ behaves like parser @p@, except that it
-- pretends that it hasn't consumed any input when an error occurs.
--
-- This combinator is used whenever arbitrary look ahead is needed.
-- Since it pretends that it hasn't consumed any input when @p@ fails,
-- the ('<|>') combinator will try its second alternative even when the
-- first parser failed while consuming input.
--
-- The @try@ combinator can for example be used to distinguish
-- identifiers and reserved words. Both reserved words and identifiers
-- are a sequence of letters. Whenever we expect a certain reserved
-- word where we can also expect an identifier we have to use the @try@
-- combinator. Suppose we write:
--
-- >  expr        = letExpr <|> identifier <?> "expression"
-- >
-- >  letExpr     = do{ string "let"; ... }
-- >  identifier  = many1 letter
--
-- If the user writes \"lexical\", the parser fails with: @unexpected
-- \'x\', expecting \'t\' in \"let\"@. Indeed, since the ('<|>') combinator
-- only tries alternatives when the first alternative hasn't consumed
-- input, the @identifier@ parser is never tried (because the prefix
-- \"le\" of the @string \"let\"@ parser is already consumed). The
-- right behaviour can be obtained by adding the @try@ combinator:
--
-- >  expr        = letExpr <|> identifier <?> "expression"
-- >
-- >  letExpr     = do{ try (string "let"); ... }
-- >  identifier  = many1 letter
try :: GenParser tok st a -> GenParser tok st a
try (Parser p) = Parser $ \state -> case p state of
  Consumed (Error err)  -> Empty (Error err)
  Consumed ok           -> Consumed ok    -- was: Empty ok
  mty                   -> mty

-- | @lookAhead p@ parses @p@ without consuming any input.
lookAhead :: GenParser tok st a -> GenParser tok st a
lookAhead p = do
    state <- getParserState
    x <- p'
    _ <- setParserState state
    return x
  where p' = Parser $ \ state -> case runP p state of
          Consumed ok@Ok {} -> Empty ok
          reply -> reply

-- | The parser @token showTok posFromTok testTok@ accepts a token @t@
-- with result @x@ when the function @testTok t@ returns @'Just' x@. The
-- source position of the @t@ should be returned by @posFromTok t@ and
-- the token can be shown using @showTok t@.
--
-- This combinator is expressed in terms of 'tokenPrim'.
-- It is used to accept user defined token streams. For example,
-- suppose that we have a stream of basic tokens tupled with source
-- positions. We can than define a parser that accepts single tokens as:
--
-- >  mytoken x
-- >    = token showTok posFromTok testTok
-- >    where
-- >      showTok (pos,t)     = show t
-- >      posFromTok (pos,t)  = pos
-- >      testTok (pos,t)     = if x == t then Just t else Nothing
token :: (tok -> String) -> (tok -> SourcePos) -> (tok -> Maybe a)
  -> GenParser tok st a
token shw tokpos = tokenPrim shw nextpos where
    nextpos _ tok ts = case ts of
      t : _ -> tokpos t
      _ -> tokpos tok

-- | The parser @token showTok nextPos testTok@ accepts a token @t@
-- with result @x@ when the function @testTok t@ returns @'Just' x@. The
-- token can be shown using @showTok t@. The position of the /next/
-- token should be returned when @nextPos@ is called with the current
-- source position @pos@, the current token @t@ and the rest of the
-- tokens @toks@, @nextPos pos t toks@.
--
-- This is the most primitive combinator for accepting tokens. For
-- example, the 'Text.Parsec.Char.char' parser could be implemented as:
--
-- >  char c
-- >    = tokenPrim showChar nextPos testChar
-- >    where
-- >      showChar x        = "'" ++ x ++ "'"
-- >      testChar x        = if x == c then Just x else Nothing
-- >      nextPos pos x xs  = updatePosChar pos x
tokenPrim :: (tok -> String) -> (SourcePos -> tok -> [tok] -> SourcePos)
  -> (tok -> Maybe a) -> GenParser tok st a
tokenPrim shw nextpos = tokenPrimEx shw nextpos Nothing

-- | The most primitive token recogniser. The expression @tokenPrimEx show nextpos mbnextstate test@,
-- recognises tokens when @test@ returns @Just x@ (and returns the value @x@). Tokens are shown in
-- error messages using @show@. The position is calculated using @nextpos@, and finally, @mbnextstate@,
-- can hold a function that updates the user state on every token recognised (nice to count tokens :-).
-- The function is packed into a 'Maybe' type for performance reasons.
tokenPrimEx :: (tok -> String) -> (SourcePos -> tok -> [tok] -> SourcePos)
  -> Maybe (SourcePos -> tok -> [tok] -> st -> st) -> (tok -> Maybe a)
  -> GenParser tok st a
tokenPrimEx shw nextpos mbNextState test = case mbNextState of
  Nothing -> Parser $ \ (State input pos user) -> case input of
    c : cs -> case test c of
      Just x  -> let
        newpos   = nextpos pos c cs
        newstate = State cs newpos user
        in seq newpos . seq newstate .
              Consumed . Ok x newstate $ newErrorUnknown newpos
      Nothing -> Empty (sysUnExpectError (shw c) pos)
    [] -> Empty (sysUnExpectError "" pos)
  Just nextState -> Parser $ \ (State input pos user) -> case input of
    c : cs -> case test c of
      Just x  -> let
        newpos   = nextpos pos c cs
        newuser  = nextState pos c cs user
        newstate = State cs newpos newuser
        in seq newpos . seq newstate .
              Consumed . Ok x newstate $ newErrorUnknown newpos
      Nothing -> Empty $ sysUnExpectError (shw c) pos
    [] -> Empty $ sysUnExpectError "" pos

label :: GenParser tok st a -> String -> GenParser tok st a
label p msg = labels p [msg]

labels :: GenParser tok st a -> [String] -> GenParser tok st a
labels (Parser p) msgs = Parser $ \state -> case p state of
  Empty reply -> Empty $ case reply of
    Error err        -> Error $ setExpectErrors err msgs
    Ok x state1 err  | errorIsUnknown err -> reply
                     | otherwise -> Ok x state1 $ setExpectErrors err msgs
  other -> other

-- | @updateParserState f@ applies function @f@ to the parser state.
updateParserState :: (State tok st -> State tok st)
  -> GenParser tok st (State tok st)
updateParserState f = Parser $ \state -> let newstate = f state in
  Empty . Ok state newstate $ unknownError newstate

-- | The parser @unexpected msg@ always fails with an unexpected error
-- message @msg@ without consuming any input.
--
-- The parsers 'fail', ('<?>') and @unexpected@ are the three parsers
-- used to generate error messages. Of these, only ('<?>') is commonly
-- used. For an example of the use of @unexpected@, see the definition
-- of 'Text.Parsec.Combinator.notFollowedBy'.
unexpected :: String -> GenParser tok st a
unexpected msg =
  Parser $ Empty . Error . newErrorMessage (UnExpect msg) . statePos

setExpectErrors :: ParseError -> [String] -> ParseError
setExpectErrors err ms = case ms of
  [] -> setErrorMessage (Expect "") err
  [msg] -> setErrorMessage (Expect msg) err
  msg : msgs -> foldr (addErrorMessage . Expect)
    (setErrorMessage (Expect msg) err) msgs

sysUnExpectError :: String -> SourcePos -> Reply tok st a
sysUnExpectError msg = Error . newErrorMessage (SysUnExpect msg)

unknownError :: State tok st -> ParseError
unknownError = newErrorUnknown . statePos

-----------------------------------------------------------
-- Parsers unfolded for space:
-- if many and skipMany are not defined as primitives,
-- they will overflow the stack on large inputs
-----------------------------------------------------------

-- | @manyAux p@ applies the parser @p@ /zero/ or more times. Returns a
--    list of the returned values of @p@.
--
-- >  identifier  = do{ c  <- letter
-- >                  ; cs <- many (alphaNum <|> char '_')
-- >                  ; return (c:cs)
-- >                  }
manyAux :: GenParser tok st a -> GenParser tok st [a]
manyAux p = do
  xs <- manyAccum (:) p
  return (reverse xs)

-- | @skipMany p@ applies the parser @p@ /zero/ or more times, skipping
-- its result.
--
-- >  spaces  = skipMany space
skipMany :: GenParser tok st a -> GenParser tok st ()
skipMany p = () <$ manyAccum (\ _ _ -> []) p

manyAccum :: (a -> [a] -> [a]) -> GenParser tok st a -> GenParser tok st [a]
manyAccum accum (Parser p) = Parser $ \state -> let
  walk xs st r = case r of
    Empty (Error err)          -> Ok xs st err
    Empty _                    -> error "Text.ParserCombinators.Parsec.Prim.many: combinator 'many' is applied to a parser that accepts an empty string."
    Consumed (Error err)       -> Error err
    Consumed (Ok x state' _)   -> let ys = accum x xs in
      seq ys . walk ys state' $ p state'
  in case p state of
  Empty reply  -> case reply of
    Ok {} -> error "Text.ParserCombinators.Parsec.Prim.many: combinator 'many' is applied to a parser that accepts an empty string."
    Error err       -> Empty (Ok [] state err)
  consumed     -> Consumed $ walk [] state consumed

-----------------------------------------------------------
-- Parsers unfolded for speed:
--  tokens
-----------------------------------------------------------

{- specification of @tokens@:
tokens showss nextposs s
  = scan s
  where
    scan []       = return s
    scan (c:cs)   = do{ token show nextpos c <?> shows s; scan cs }

    show c        = shows [c]
    nextpos pos c = nextposs pos [c]
-}

tokens :: Eq tok => ([tok] -> String) -> (SourcePos -> [tok] -> SourcePos)
  -> [tok] -> GenParser tok st [tok]
tokens shws nextposs s = Parser $ \ (State input pos user) -> let
  ok cs = let
    newpos   = nextposs pos s
    newstate = State cs newpos user
    in seq newpos . seq newstate . Ok s newstate $ newErrorUnknown newpos
  errMsg m = Error . setErrorMessage (Expect (shws s)) $ newErrorMessage
    (SysUnExpect m) pos
  errEof = errMsg ""
  errExpect = errMsg . shws . reverse
  walk r xs cs = case xs of
    [] -> ok cs
    x : rs -> case cs of
      [] -> errExpect r
      c : ss -> if x == c then walk (x : r) rs ss else errExpect $ c : r

  walk1 xs cs = case xs of
    [] -> Empty $ ok cs
    x : rs -> case cs of
      [] ->  Empty errEof
      c : ss ->
        if x == c then Consumed $ walk [x] rs ss else Empty $ errExpect [c]
  in walk1 s input
