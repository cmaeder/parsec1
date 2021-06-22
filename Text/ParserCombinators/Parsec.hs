{- |
Module      :  Text.ParserCombinators.Parsec
Copyright   :  (c) Daan Leijen 1999-2001
License     :  BSD-style (see the file LICENSE)

Maintainer  :  Christian Maeder <chr.maeder@web.de>
Stability   :  provisional
Portability :  portable

Parsec, the Fast Monadic Parser combinator library.

Inspired by:

Graham Hutton and Erik Meijer:
Monadic Parser Combinators.
Technical report NOTTCS-TR-96-4.
Department of Computer Science, University of Nottingham, 1996.
<https://www.cs.nott.ac.uk/~gmh/monparsing.ps>

Andrew Partridge, David Wright:
Predictive parser combinators need four values to report errors.
Journal of Functional Programming 6(2): 355-364, 1996

This helper module exports elements from the basic libraries.
-}

module Text.ParserCombinators.Parsec
  ( -- complete modules
    module Text.ParserCombinators.Parsec.Char
  , module Text.ParserCombinators.Parsec.Combinator
  , module Text.ParserCombinators.Parsec.Prim
    -- module Text.ParserCombinators.Parsec.Error
  , ParseError
  , errorPos
    -- module Text.ParserCombinators.Parsec.Pos
  , Column
  , Line
  , SourceName
  , SourcePos
  , incSourceColumn
  , incSourceLine
  , setSourceColumn
  , setSourceLine
  , setSourceName
  , sourceColumn
  , sourceLine
  , sourceName
  ) where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Prim
