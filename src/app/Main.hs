import Colog (cmap, logStringStdout)
import Colog.Actions (logTextStdout)
import Colog.Core.Action ((<&), LogAction, unLogAction)
import Colog.Core.Severity (Severity(..))
import Colog.Message (Message, Msg(..), fmtMessage, logDebug, logError, logInfo, logWarning, showSourceLoc)
import Colog.Monad (LoggerT, usingLoggerT)
import Data.Text (Text, pack)


-- This is what the LogAction type looks like

-- newtype LogAction m msg = LogAction
--   { unlogAction :: msg -> m ()
--   }


example0 :: IO ()
example0 = do
  putStrLn "Example 0: First message, simple putStrLn"
  putStrLn "Example 0: Second message, simple putStrLn"


example1 :: LogAction IO String -> IO ()
example1 logger' = do
  unLogAction logger' "Example 1: First message, using unLogAction"
  unLogAction logger' "Example 1: Second message, using unLogAction"

-- (<&) is an operator version of unLogAction

example2 :: LogAction IO String -> IO ()
example2 logger' = do
  logger' <& "Example 2: First message, using the (<&) operator on the logger"
  logger' <& "Example 2: Second message, using the (<&) operator on the logger"


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

showText :: Show a => a -> Text
showText = pack . show

exampleN :: Int -> LoggerT Message IO ()
exampleN exNum = do
  logDebug $ "example" <> showText exNum <> ": A debug message"
  logInfo $ "example" <> showText exNum <> ": This is an info message"
  logWarning $ "example" <> showText exNum <> ": A warning message"
  logError $ "example" <> showText exNum <> ": An error message"


main :: IO ()
main = do
  example0

  let logger1 = logStringStdout

  example1 logger1
  example2 logger1

  let logger2 = cmap fmtMessage logTextStdout
  usingLoggerT logger2 $ exampleN 3

  let logger3 = cmap fmtCustom logTextStdout
  usingLoggerT logger3 $ exampleN 4
