{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

import Colog (cmap, logStringStdout)
import Colog.Actions (logTextStdout)
import Colog.Core.Action ((<&), LogAction, unLogAction)
import Colog.Core.Severity
  ( pattern E, pattern I
  , Severity(Debug, Info, Warning, Error)
  , filterBySeverity
  )
import Colog.Message
  ( Message, Msg(..)
  , fmtMessage
  , log, logDebug, logWarning
  , msgSeverity
  , showSourceLoc
  )
import Colog.Monad (LoggerT, usingLoggerT)
import Data.Text (Text, pack)
import Prelude hiding (log)


-- This is what the LogAction type looks like

-- newtype LogAction m msg = LogAction
--   { unlogAction :: msg -> m ()
--   }


example0 :: IO ()
example0 = do
  putStrLn "Example 0: Simple putStrLn, no logging API at all"

  putStrLn "A simple message"
  putStrLn "Another message"


example1 :: LogAction IO String -> IO ()
example1 logger = do
  putStrLn "\nExample 1: using `unLogAction logger msg`"

  unLogAction logger "A simple message"
  unLogAction logger "Another message"


-- (<&) is an operator version of unLogAction

example2 :: LogAction IO String -> IO ()
example2 logger = do
  putStrLn "\nExample 2: using `logger <& msg`"

  logger <& "A simple message"
  logger <& "Another message"


-- Here's a more practical example with log severities and even call stack info
-- that's in the LoggerT monad transformer.

-- First we use the stock fmtMessage and then use the following two functions
-- to construct a custom logger with these features:
--   - No color output
--   - Only Debug shows the call stack info

showSeverity' :: Severity -> Text
showSeverity' Debug   = "[Debug]   "
showSeverity' Info    = "[Info]    "
showSeverity' Warning = "[Warning] "
showSeverity' Error   = "[Error]   "

fmtCustom :: Message -> Text
fmtCustom (Msg sev@Debug stack msg) = showSeverity' sev <> showSourceLoc stack <> msg
fmtCustom (Msg sev       _     msg) = showSeverity' sev <> msg

-- A simple helper function that should probably be in the text package already!
showText :: Show a => a -> Text
showText = pack . show

-- Note our pattern matching above in showSeverity' works whether we use the PatternSynonym or not

exampleN :: Int -> LoggerT Message IO ()
exampleN exNum = do
  logDebug $ "example" <> showText exNum <> ": A debug message"
  log I $ "example" <> showText exNum <> ": This is an info message"
  logWarning $ "example" <> showText exNum <> ": A warning message"
  log E $ "example" <> showText exNum <> ": An error message"


example3 :: LogAction IO Message -> IO ()
example3 logger = do
  putStrLn "\nExample 3: With usingLoggerT and the stock fmtMessage"

  usingLoggerT logger $ exampleN 3


example4 :: LogAction IO Message -> IO ()
example4 logger = do
  putStrLn "\nExample 4: With usingLoggerT and a custom message formatter"

  usingLoggerT logger $ exampleN 4


example5 :: LogAction IO Message -> IO ()
example5 logger = do
  putStrLn "\nExample 5: With usingLoggerT and a logger which filters for Warning or higher"

  usingLoggerT logger $ exampleN 5


main :: IO ()
main = do
  example0

  -- In each of these cases, we're passing the LogAction (the logger) in

  example1 logStringStdout
  example2 logStringStdout
  example3 $ cmap fmtMessage logTextStdout
  example4 $ cmap fmtCustom logTextStdout

  -- Beware, more recent docs show examples using filterBySeverity with the
  -- WithSeverity type but I had problems composing this type with the
  -- formatting code from earlier examples which contain a simple
  -- `type Message = Msg Severity` instead. My solution for this exercise was
  -- to not use the WithSeverity data type here.

  let logger5 = filterBySeverity Warning msgSeverity . cmap fmtMessage $ logTextStdout
  example5 logger5
