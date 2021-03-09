-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Error
-- Copyright   :  (c) Daan Leijen 1999-2001
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
--
-- Maintainer  :  Antoine Latter <aslatter@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Parse errors
--
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Error
                  ( Message(SysUnExpect,UnExpect,Expect,Message)
                  , messageString, messageCompare, messageEq

                  , ParseError, errorPos, errorMessages, errorIsUnknown
                  , showErrorMessages

                  , newErrorMessage, newErrorUnknown
                  , addErrorMessage, setErrorPos, setErrorMessage
                  , mergeError
                  )
                  where


import Prelude
import Data.List (nub,sortBy)
import Text.ParserCombinators.Parsec.Pos

-- | This abstract data type represents parse error messages. There are
-- four kinds of messages:
--
-- >  data Message = SysUnExpect String
-- >               | UnExpect String
-- >               | Expect String
-- >               | Message String
--
-- The fine distinction between different kinds of parse errors allows
-- the system to generate quite good error messages for the user. It
-- also allows error messages that are formatted in different
-- languages. Each kind of message is generated by different combinators:
--
--     * A 'SysUnExpect' message is automatically generated by the
--       'Text.Parsec.Combinator.satisfy' combinator. The argument is the
--       unexpected input.
--
--     * A 'UnExpect' message is generated by the 'Text.Parsec.Prim.unexpected'
--       combinator. The argument describes the
--       unexpected item.
--
--     * A 'Expect' message is generated by the 'Text.Parsec.Prim.<?>'
--       combinator. The argument describes the expected item.
--
--     * A 'Message' message is generated by the 'fail'
--       combinator. The argument is some general parser message.
data Message        = SysUnExpect !String   --library generated unexpect
                    | UnExpect    !String   --unexpected something
                    | Expect      !String   --expecting something
                    | Message     !String   --raw message

messageToEnum msg
    = case msg of SysUnExpect _ -> 0
                  UnExpect _    -> 1
                  Expect _      -> 2
                  Message _     -> 3

messageCompare :: Message -> Message -> Ordering
messageCompare msg1 msg2
    = compare (messageToEnum msg1) (messageToEnum msg2)

-- | Extract the message string from an error message
messageString :: Message -> String
messageString msg
    = case msg of SysUnExpect s -> s
                  UnExpect s    -> s
                  Expect s      -> s
                  Message s     -> s

messageEq :: Message -> Message -> Bool
messageEq msg1 msg2
    = (messageCompare msg1 msg2 == EQ)


-- | The abstract data type @ParseError@ represents parse errors. It
-- provides the source position ('SourcePos') of the error
-- and a list of error messages ('Message'). A @ParseError@
-- can be returned by the function 'Text.Parsec.Prim.parse'. @ParseError@ is an
-- instance of the 'Show' class.
data ParseError     = ParseError !SourcePos [Message]

-- | Extracts the source position from the parse error
errorPos :: ParseError -> SourcePos
errorPos (ParseError pos msgs)
    = pos

-- | Extracts the list of error messages from the parse error
errorMessages :: ParseError -> [Message]
errorMessages (ParseError pos msgs)
    = sortBy messageCompare msgs

errorIsUnknown :: ParseError -> Bool
errorIsUnknown (ParseError pos msgs)
    = null msgs


-----------------------------------------------------------
-- Create parse errors
-----------------------------------------------------------
newErrorUnknown :: SourcePos -> ParseError
newErrorUnknown pos
    = ParseError pos []

newErrorMessage :: Message -> SourcePos -> ParseError
newErrorMessage msg pos
    = ParseError pos [msg]

addErrorMessage :: Message -> ParseError -> ParseError
addErrorMessage msg (ParseError pos msgs)
    = ParseError pos (msg:msgs)

setErrorPos :: SourcePos -> ParseError -> ParseError
setErrorPos pos (ParseError _ msgs)
    = ParseError pos msgs

setErrorMessage :: Message -> ParseError -> ParseError
setErrorMessage msg (ParseError pos msgs)
    = ParseError pos (msg:filter (not . messageEq msg) msgs)


mergeError :: ParseError -> ParseError -> ParseError
mergeError e1@(ParseError pos1 msgs1) e2@(ParseError pos2 msgs2)
    -- prefer meaningful errors
    | null msgs2 && not (null msgs1) = e1
    | null msgs1 && not (null msgs2) = e2
    | otherwise
    = case pos1 `compare` pos2 of
        -- select the longest match
        EQ -> ParseError pos1 (msgs1 ++ msgs2)
        GT -> e1
        LT -> e2

-----------------------------------------------------------
-- Show Parse Errors
-----------------------------------------------------------
instance Show ParseError where
  show err
    = show (errorPos err) ++ ":" ++
      showErrorMessages "or" "unknown parse error"
                        "expecting" "unexpected" "end of input"
                       (errorMessages err)


-- | Language independent show function
showErrorMessages ::
    String -> String -> String -> String -> String -> [Message] -> String
showErrorMessages msgOr msgUnknown msgExpecting msgUnExpected msgEndOfInput msgs
    | null msgs = msgUnknown
    | otherwise = concat $ map ("\n"++) $ clean $
                 [showSysUnExpect,showUnExpect,showExpect,showMessages]
    where
      (sysUnExpect,msgs1)   = span (messageEq (SysUnExpect "")) msgs
      (unExpect,msgs2)      = span (messageEq (UnExpect "")) msgs1
      (expect,messages)     = span (messageEq (Expect "")) msgs2

      showExpect        = showMany msgExpecting expect
      showUnExpect      = showMany msgUnExpected unExpect
      showSysUnExpect   | not (null unExpect) ||
                          null sysUnExpect       = ""
                        | null firstMsg          = msgUnExpected ++ " " ++ msgEndOfInput
                        | otherwise              = msgUnExpected ++ " " ++ firstMsg
                        where
                          firstMsg  = messageString (head sysUnExpect)

      showMessages      = showMany "" messages


      --helpers
      showMany pre msgs = case (clean (map messageString msgs)) of
                            [] -> ""
                            ms | null pre  -> commasOr ms
                               | otherwise -> pre ++ " " ++ commasOr ms

      commasOr []       = ""
      commasOr [m]      = m
      commasOr ms       = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms

      commaSep          = seperate ", " . clean
      semiSep           = seperate "; " . clean

      seperate sep []   = ""
      seperate sep [m]  = m
      seperate sep (m:ms) = m ++ sep ++ seperate sep ms

      clean             = nub . filter (not.null)

