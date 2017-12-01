{-# LANGUAGE UnicodeSyntax, TupleSections, OverloadedStrings, NegativeLiterals #-}
{-# LANGUAGE FlexibleInstances #-}

module UI.NCurses.Class
where


import qualified UI.NCurses  as Curses

--------------------------------------------------------------------------------

class (Monad m) ⇒ MonadCurses m where
    liftCurses ∷ Curses.Curses a → m a

