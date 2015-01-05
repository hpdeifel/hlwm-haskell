module Graphics.X11.Xlib.Herbst where

#include <X11/Xlib.h>
#include <X11/Xutil.h>

import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib
import System.Environment

import Foreign
import Foreign.C

-- FIXME Some functions shouldn't return IO () but IO Bool or something
-- or throw an exception if the X-functions doesn't return success

setClassHint :: Display -> Window -> ClassHint -> IO ()
setClassHint d w ch = allocaBytes (#{size XClassHint}) $ \p ->
  withCString (resName ch) $ \resName' ->
  withCString (resClass ch) $ \resClass' -> do
    #{poke XClassHint, res_name  } p $ resName'
    #{poke XClassHint, res_class } p $ resClass'
    xSetClassHint d w p


foreign import ccall unsafe "X11/Xlib.h XSetClassHint"
  xSetClassHint :: Display -> Window -> Ptr ClassHint -> IO ()

utf8TextListToTextProperty :: Display -> [String] -> IO TextProperty
utf8TextListToTextProperty d strs =
  allocaBytes (#{size XTextProperty}) $ \p -> do
    cstrs <- mapM newCString strs
    let len = fromIntegral $ length strs

    withArray cstrs $ \array ->
      xUtf8TextListToTextProperty d array len uTF8StringStyle p

    mapM_ free cstrs

    peek p

newtype ICCEncodingStyle = ICCEncodingStyle CInt
#{enum ICCEncodingStyle, ICCEncodingStyle,
  stringStyle = XStringStyle,
  compoundTextStyle = XCompoundTextStyle,
  textStyle = XTextStyle,
  stdICCTextStyle = XStdICCTextStyle,
  uTF8StringStyle = XUTF8StringStyle
}

foreign import ccall unsafe "X11/Xlib.h Xutf8TextListToTextProperty"
  xUtf8TextListToTextProperty :: Display -> Ptr (CString) -> CInt -> ICCEncodingStyle -> (Ptr TextProperty) -> IO ()

utf8TextPropertyToTextList :: Display -> TextProperty -> IO [String]
utf8TextPropertyToTextList d tp =
  alloca $ \intPtr ->
  alloca $ \tpPtr ->
  alloca $ \ptrPtr -> do
    poke tpPtr tp
    xUtf8TextPropertyToTextList d tpPtr ptrPtr intPtr
    strArr <- peek ptrPtr
    num <- peek intPtr
    cstrs <- peekArray (fromIntegral num) strArr
    strs <- mapM peekCString cstrs
    freeStringList strArr
    return strs

foreign import ccall unsafe "X11/Xlib.h Xutf8TextPropertyToTextList"
  xUtf8TextPropertyToTextList :: Display -> Ptr TextProperty -> (Ptr (Ptr CString)) -> (Ptr CInt) -> IO ()

openDefaultDisplay :: IO Display
openDefaultDisplay = getEnv "DISPLAY" >>= openDisplay

setTextProperty' :: Display -> Window -> TextProperty -> Atom -> IO ()
setTextProperty' d w tp a = allocaBytes (sizeOf tp) $ \p -> do
  poke p tp
  xSetTextProperty d w p a

foreign import ccall unsafe "X11/Xlib.h XSetTextProperty"
        xSetTextProperty :: Display -> Window -> Ptr TextProperty -> Atom -> IO ()
