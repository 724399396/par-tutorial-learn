import Control.Concurrent
import Control.Exception

data Timeout = Timeout deriving (Show, Eq)

instance Exception Timeout

timeout :: Int -> IO a -> IO (Maybe a)
timeout t m
  | t < 0 = fmap Just m
  | t == 0 = return Nothing
  | otherwise = do
      pid <- myThreadId
      let ex = Timeout
      handleJust
        (\e -> if (e == ex) then Just () else Nothing)
        (\_ -> return Nothing)
        (bracket (forkIO $ do threadDelay t
                              throwTo pid ex)
                 (\cid -> throwTo cid ThreadKilled)
                 (\_ -> fmap Just m))
