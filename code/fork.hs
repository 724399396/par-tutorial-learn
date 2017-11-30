import Control.Concurrent
import Control.Monad
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  _ <- forkIO $ forever $ putChar 'A'
  _ <- forkIO $ forever $ putChar 'B'
  threadDelay (10^(6::Int))
