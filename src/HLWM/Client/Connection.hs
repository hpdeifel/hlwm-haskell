module HLWM.Client.Connection where

import Graphics.X11.Xlib
import System.Posix.Types

-- | Opaque type representing the connection to the herbstluftwm server
--
-- See 'connect' and 'disconnect'.
data HerbstConnection = HerbstConnection {
  display :: Display,
  atomArgs :: Atom,
  atomOutput :: Atom,
  atomStatus :: Atom,
  root :: Window,
  hooksWin :: Window,
  clientWin :: Window
}

connectionFd :: HerbstConnection -> Fd
connectionFd = Fd . connectionNumber . display
