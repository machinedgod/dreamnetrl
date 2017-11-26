{-# LANGUAGE UnicodeSyntax, TupleSections, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE FlexibleInstances #-}


module UI.NCurses.Class
where


import Control.Monad.Reader
import Control.Monad.State
import qualified UI.NCurses  as Curses

--------------------------------------------------------------------------------

class (Monad m) ⇒ MonadCurses m where
    liftCurses ∷ Curses.Curses a → m a

