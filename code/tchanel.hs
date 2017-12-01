import Control.Concurrent.STM hiding (TChan, newTChan, readTChan, writeTChan, dupTChan)

data TChan a = TChan (TVar (TVarList a))
                     (TVar (TVarList a))

type TVarList a= TVar (TList a)
data TList a = TNil | TCons a (TVarList a)

newTChan :: STM (TChan a)
newTChan = do
  hole <- newTVar TNil
  readEnd <- newTVar hole
  writeEnd <- newTVar hole
  return $ TChan readEnd writeEnd

readTChan :: TChan a -> STM a
readTChan (TChan readEnd _) = do
  hole <- readTVar readEnd
  tl <- readTVar hole
  case tl of
        TNil -> retry
        TCons val nHole -> do writeTVar readEnd nHole
                              return val

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan _ writeEnd) val = do
  hole <- readTVar writeEnd
  nHole <- newTVar TNil
  writeTVar hole (TCons val nHole)
  writeTVar writeEnd nHole

dupTChan :: TChan a -> STM (TChan a)
dupTChan (TChan _ writeEnd) = do
  hole <- readTVar writeEnd
  readEnd <- newTVar hole
  return $ TChan readEnd writeEnd

unGetTChan :: TChan a -> a -> STM ()
unGetTChan (TChan readEnd _) val = do
  hole <- readTVar readEnd
  nHole <- newTVar (TCons val hole)
  writeTVar readEnd nHole

isEmptyTChan :: TChan a -> STM Bool
isEmptyTChan (TChan readEnd _) = do
  hole <- readTVar readEnd
  tl <- readTVar hole
  case tl of
    TNil -> return True
    _ -> return False

readEitherTChan :: TChan a -> TChan b -> STM (Either a b)
readEitherTChan a b =
  fmap Left (readTChan a)
  `orElse`
  fmap Right (readTChan b)
