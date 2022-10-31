import Colog.Core (LogAction (..), (<&), logStringStdout)


-- This is what the LogAction type looks like

-- newtype LogAction m msg = LogAction
--   { unlogAction :: msg -> m ()
--   }


example0 :: IO ()
example0 = do
  putStrLn "Example 0: First message"
  putStrLn "Example 0: Second message"


example1 :: LogAction IO String -> IO ()
example1 logger' = do
  unLogAction logger' "Example 1: First message"
  unLogAction logger' "Example 1: Second message"

-- (<&) is an operator version of unLogAction

example2 :: LogAction IO String -> IO ()
example2 logger' = do
  logger' <& "Example 2: First message"
  logger' <& "Example 2: Second message"


main :: IO ()
main = do
  example0
  let logger = logStringStdout
  example1 logger
  example2 logger
