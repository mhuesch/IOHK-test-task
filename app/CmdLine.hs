module CmdLine
  ( getOptions
  , PosArgs(..)
  , Options(..)
  ) where


import           Control.Monad
import           Data.Word
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO


--------------------------------------------------------------------------------
-- Options data types
--------------------------------------------------------------------------------
data PosArgs = PosArgs { role :: String
                       , host :: String
                       , port :: String }

data Options = Options
  { optSeed    :: Word32
  , optSendFor :: Int
  , optWaitFor :: Int
  }

defaultOptions = Options
  { optSeed    = 0
  , optSendFor = 0
  , optWaitFor = 0
  }

--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------
getOptions :: IO (PosArgs, Options)
getOptions = do
  -- Parse command line --------------------------------------------------
  (actions, positionals, _) <- getOpt Permute options <$> getArgs
  --                     ^ we ignore error messages here, because they are
  --                     handled in the option parsers via IO

  -- fold the options over the list of actions
  opts <- foldl (>>=) (return defaultOptions) actions

  -- check positional arguments
  posArgs <- case positionals of
    [role, host, port] -> return (PosArgs role host port)
    _                  -> showUsage >> exitFailure

  return (posArgs, opts)

--------------------------------------------------------------------------------
-- Usage printout
--------------------------------------------------------------------------------
showUsage :: IO ()
showUsage = do
  prg <- getProgName
  let header = unlines [ "usage: " ++ prg ++ " ROLE HOST PORT [OPTIONS..]"
                       , ""
                       , "ROLE: master | slave"
                       , "HOST: IP (e.g. 198.51.100.1) or localhost"
                       , "PORT: INT (e.g. 8080)"
                       , ""
                       , "OPTIONS:"
                       ]
  hPutStrLn stderr (usageInfo header options)

--------------------------------------------------------------------------------
-- Options to handle
--------------------------------------------------------------------------------
options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "" ["send-for"]
        (ReqArg
            (\arg opt -> do
              val <- readOrError arg
              return opt { optSendFor = val })
            "INT")
        (unwords [ "seconds to send for"
                 , "\n", "[default ="
                 , show (optSendFor defaultOptions), "]"])

  , Option "" ["wait-for"]
        (ReqArg
            (\arg opt -> do
              val <- readOrError arg
              return opt { optWaitFor = val })
            "INT")
        (unwords [ "duration of grace period (after sends stop)"
                 , "\n", "[default ="
                 , show (optWaitFor defaultOptions), "]"])

  , Option "" ["with-seed"]
        (ReqArg
            (\arg opt -> do
              val <- readOrError arg
              return opt { optSeed = val })
            "WORD32")
        (unwords [ "seed for random generator"
                 , "(will be combined with MWC-random's default seed)"
                 , "\n", "[default ="
                 , show (optSeed defaultOptions), "]"])

  , Option "h" ["help"]
        (NoArg
            (\_ -> do
              showUsage
              exitSuccess))
        "Show help"
  ]

-- read helper
readOrError :: Read a => String -> IO a
readOrError str = case reads str of
                    [(val, "")] -> return val
                    _           -> do
                      hPutStrLn stderr $ "read: no parse: " ++ str ++ "\n"
                      showUsage
                      exitFailure
