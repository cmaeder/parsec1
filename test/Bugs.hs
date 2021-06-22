
module Bugs
       ( bugs
       ) where

import Test.Framework

import qualified Bugs.Bug6

bugs :: [Test]
bugs = [ Bugs.Bug6.main
       ]
