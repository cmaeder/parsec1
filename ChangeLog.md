### 1.0.0.8

- reexported the operators (*>), (<$), (<$>), (<*), (<*>) to avoid
  import of Control.Applicative for versions below ghc-7.10

- fixed parsec bug 6 for lookAhead

- added ChangeLog.md

- readded fail definition for older Monad instances (removed in 1.0.0.7)

- refactored code (hlint, scan, stylish, -Wall)

- added Eq instance to ParseError

- changed Maintainer from Antoine Latter <aslatter@gmail.com> to
  Christian Maeder <chr.maeder@web.de>

### 1.0.0.7

- added MonadFail instance for ghc-8.8

### 1.0.0.6 and earlier

- has no known history. It was uploaded on 2015-02-16 after
  parsec-3.1.8 on 2015-01-10 but only mentions parsec-3.1.2 from 2011-10-08
