import           Control.Concurrent hiding (Chan, dupChan, newChan, readChan,
                                     writeChan)
import           Control.Monad

data Chan a = Chan (MVar (Stream a)) (MVar (Stream a))

type Stream a = MVar (Item a)

data Item a = Item a (Stream a)

newChan :: IO (Chan a)
newChan = do
  hole <- newEmptyMVar
  readEnd <- newMVar hole
  writeEnd <- newMVar hole
  return $ Chan readEnd writeEnd

readChan :: Chan a -> IO a
readChan (Chan readEnd _) = do
  hole <- takeMVar readEnd
  Item val newHole <- readMVar hole
  putMVar readEnd newHole
  return val

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeEnd) val = do
  newHole <- newEmptyMVar
  oldHole <- takeMVar writeEnd
  putMVar oldHole (Item val newHole)
  putMVar writeEnd newHole


dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeEnd) = do
  writeHole <- readMVar writeEnd
  readEnd <- newMVar writeHole
  return $ Chan readEnd writeEnd

unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan readEnd _) val = do
  hole <- takeMVar readEnd
  newHole <- newMVar (Item val hole)
  putMVar readEnd newHole

main :: IO ()
main = do
  c1 <- newChan
  writeChan c1 (1 :: Int)
  c2 <- dupChan c1
  writeChan c1 2
  writeChan c2 3
  _ <- forkIO $ replicateM_ 3 (readChan c1 >>= (putStrLn . show))
  _ <- forkIO $ replicateM_ 2 (readChan c2 >>= (putStrLn . show))
  return ()
