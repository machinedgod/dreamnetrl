{-# LANGUAGE UnicodeSyntax, TupleSections, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE FlexibleInstances #-}


module UI.NCurses.Class
where


import Control.Monad.Reader
import Control.Monad.State
import qualified UI.NCurses  as Curses

--------------------------------------------------------------------------------

class (Monad m) ⇒ MonadCurses m where
    liftCurses    ∷ Curses.Curses a → m a

instance MonadCurses (ReaderT a Curses.Curses) where
    liftCurses = lift

instance MonadCurses (StateT a Curses.Curses) where
    liftCurses = lift

