{- |
Module      :  Text.ParserCombinators.Parsec.Pos
Copyright   :  (c) Daan Leijen 1999-2001
License     :  BSD-style (see the file LICENSE)

Maintainer  :  Christian Maeder <chr.maeder@web.de>
Stability   :  provisional
Portability :  portable

Textual source positions
-}

module Text.ParserCombinators.Parsec.Pos
  ( Column
  , Line
  , SourceName
  , SourcePos
  , incSourceColumn
  , incSourceLine
  , initialPos
  , newPos
  , setSourceColumn
  , setSourceLine
  , setSourceName
  , sourceColumn
  , sourceLine
  , sourceName
  , updatePosChar
  , updatePosString
  ) where

{- ---------------------------------------------------------
Source Positions, a file name, a line and a column.
upper left is (1,1)
--------------------------------------------------------- -}
type SourceName = String
type Line = Int
type Column = Int

{- | The abstract data type @SourcePos@ represents source positions. It
contains the name of the source (i.e. file name), a line number and
a column number. @SourcePos@ is an instance of the 'Show', 'Eq' and
'Ord' class. -}
data SourcePos = SourcePos
  { sourceName :: SourceName -- ^ the name of the source from a position
  , sourceLine :: !Line      -- ^ the line number from a source position
  , sourceColumn :: !Column  -- ^ the column number from a source position
  } deriving (Eq, Ord)

{- | Create a new 'SourcePos' with the given source name,
line number and column number. -}
newPos :: SourceName -> Line -> Column -> SourcePos
newPos = SourcePos

{- | Create a new 'SourcePos' with the given source name,
and line number and column number set to 1, the upper left. -}
initialPos :: SourceName -> SourcePos
initialPos name = newPos name 1 1

-- | Increments the line number of a source position.
incSourceLine :: SourcePos -> Line -> SourcePos
incSourceLine (SourcePos name line column) n
    = SourcePos name (line + n) column

-- | Increments the column number of a source position.
incSourceColumn :: SourcePos -> Column -> SourcePos
incSourceColumn (SourcePos name line column) n
    = SourcePos name line (column + n)

-- | Set the name of the source.
setSourceName :: SourcePos -> SourceName -> SourcePos
setSourceName (SourcePos _name line column) n = SourcePos n line column

-- | Set the line number of a source position.
setSourceLine :: SourcePos -> Line -> SourcePos
setSourceLine (SourcePos name _line column) n = SourcePos name n column

-- | Set the column number of a source position.
setSourceColumn :: SourcePos -> Column -> SourcePos
setSourceColumn (SourcePos name line _column) = SourcePos name line

{- | The expression @updatePosString pos s@ updates the source position
@pos@ by calling 'updatePosChar' on every character in @s@, ie.
@foldl updatePosChar pos string@. -}
updatePosString :: SourcePos -> String -> SourcePos
updatePosString pos string = forcePos (foldl updatePosChar pos string)

updatePosChar :: SourcePos -> Char -> SourcePos
updatePosChar (SourcePos name line column) c = forcePos $ case c of
  '\n' -> SourcePos name (line + 1) 1
  '\t' -> SourcePos name line (column + 8 - ((column - 1) `mod` 8))
  _ -> SourcePos name line (column + 1)

forcePos :: SourcePos -> SourcePos
forcePos pos@(SourcePos _name line column) = seq line (seq column pos)

{- ---------------------------------------------------------
Show positions
--------------------------------------------------------- -}
instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumn
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumn = "(line " ++ show line ++
                          ", column " ++ show column ++
                          ")"
