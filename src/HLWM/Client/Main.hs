{-# LANGUAGE LambdaCase,TemplateHaskell, MultiWayIf #-}
module Main where

import HLWM.Client.Concurrent
import System.Console.GetOpt
import Control.Lens
import Data.List
import System.Environment
import System.Exit
import System.IO

data HCOptions = HCOpt {
  _newline :: Bool,
  _print0  :: Bool,
  _lastArg :: Bool,
  _idle    :: Bool,
  _wait    :: Bool,
  _count   :: Int,
  _quiet   :: Bool,
  _version :: Bool,
  _help    :: Bool
}
makeLenses ''HCOptions

defOpts :: HCOptions
defOpts = HCOpt {
  _newline  = True,
  _print0   = False,
  _lastArg  = False,
  _idle     = False,
  _wait     = False,
  _count    = 1,
  _quiet    = False,
  _version  = False,
  _help     = False
}

options :: [OptDescr (HCOptions -> HCOptions)]
options =
  [ Option ['n'] ["no-newline"] (NoArg $ set newline False)
           "Do not print a newline if output does not end with a newline."
  , Option ['0'] ["print0"] (NoArg $ set print0 True)
           "Use the null character as delimiter between the output of hooks."
  , Option ['l'] ["last-arg"] (NoArg $ set lastArg True)
           "Print only the last argument of a hook."
  , Option ['i'] ["idle"] (NoArg $ set idle True)
           "Wait for hooks instead of executing commands."
  , Option ['w'] ["wait"] (NoArg $ set wait True)
           "Same as --idle but exit after first --count hooks."
  , Option ['c'] ["count"] (ReqArg (set count . read) "COUNT")
           "Let --wait exit after COUNT hooks were received and printed. The default of COUNT is 1."
  , Option ['q'] ["quiet"] (NoArg $ set quiet True)
           "Do not print error messages if herbstclient cannot connect to the running herbstluftwm instance."
  , Option ['v'] ["version"] (NoArg $ set version True)
           "Print the herbstclient version. To get the herbstluftwm version, use 'herbstclient version'."
  , Option ['h'] ["help"] (NoArg $ set wait True)
           "Print this help."
  ]

usage :: String -> String
usage name = "Usage: " ++ name ++ " [OPTION...] files..."

hcOpts :: [String] -> IO (HCOptions, [String])
hcOpts argv = do
  case getOpt Permute options argv of
   (o,n,[]  ) -> return (foldl (flip id) defOpts o, n)
   (_,_,errs) -> ioError (userError (concat errs))

putStrMaybeLn :: String -> IO ()
putStrMaybeLn str
  | "\n" `isSuffixOf` str = putStr str
  | otherwise = putStrLn str

helpString :: String -> String
helpString name = unlines $
  [ "Usage: " ++ name ++ " [OPTION...] files..."
  , "       " ++ name ++ " [OPTIONS] [--wait|--idle] [FILTER ...]"
  , "Send a COMMAND with optional arguments ARGS to a running herbstluftwm instance."
  , ""
  , usageInfo "Options:" options
  , "See the man page (herbstclient(1)) for more details."
  ]

data Wait = Infinite
          | Wait Int

newtype NullPolicy = Null Bool
newtype NLPolicy = NL Bool
newtype Quiet = Quiet Bool
newtype LastArg = LastArg Bool

withQConnection :: Quiet -> a -> (HerbstConnection -> IO a) -> IO a
withQConnection q x f = withConnection f >>= \case
  Nothing -> case q of
    Quiet True  -> return x
    Quiet False -> hPutStrLn stderr "Could not connect to server" >> return x
  Just y -> return y

waitForHooks :: Wait -> NullPolicy -> Quiet -> LastArg -> IO ()
waitForHooks w nl q la = withQConnection q () (doWait w)
  where doWait (Wait 0) _ = return () -- TODO handle negative values
        doWait w' con = do
          h <- nextHook con
          case la of
           LastArg True  | not (null h) -> putStr (last h)
           _                            -> putStr $ unwords h
          case nl of
           Null True  -> putStr "\0"
           Null False -> putStr "\n"
          case w' of
           Infinite -> doWait Infinite con
           Wait x   -> doWait (Wait (x-1)) con

send :: [String] -> NLPolicy -> Quiet -> IO ExitCode
send args nl q = withQConnection q (ExitFailure 1)$ \con -> do
  (stat, ret) <- sendCommand con args
  case nl of
   NL False -> putStr ret
   NL True  -> if null ret || last ret == '\n'
               then putStr ret else putStrLn ret
  return $ if stat == 0 then ExitSuccess else ExitFailure stat

main :: IO ()
main = do
  name <- getProgName
  (opts, args) <- getArgs >>= hcOpts
  if | opts^.help -> putStr $ helpString name
     | opts^.version -> putStrLn "A friendly haskell implementation of herbstclient"
     | opts^.idle -> waitForHooks Infinite (Null (opts^.print0))
                                  (Quiet (opts^.quiet)) (LastArg (opts^.lastArg))
     | opts^.wait -> waitForHooks (Wait (opts^.count)) (Null (opts^.print0))
                                  (Quiet (opts^.quiet)) (LastArg (opts^.lastArg))
     | otherwise  -> send args (NL (opts^.newline)) (Quiet (opts^.quiet))
                     >>= exitWith
