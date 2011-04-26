module Sound.SC3.Server.Connection.ListenerMap.HashTable where


import qualified Data.HashTable as Hash
import           Sound.OpenSoundControl (OSC)

type ListenerId  = Int
type Listener    = OSC -> IO ()
data ListenerMap = ListenerMap !(Hash.HashTable ListenerId Listener) !ListenerId

broadcast :: OSC -> ListenerMap -> IO ()
broadcast osc (ListenerMap h _) = mapM_ (\(_, l) -> l osc) =<< Hash.toList h

empty :: IO ListenerMap
empty = do
    h  <- Hash.new (==) Hash.hashInt
    return $ ListenerMap h 0

-- | Add a listener.
--
-- Listeners are entered in a hash table, although the allocation behavior may be more stack-like.
add :: Listener -> ListenerMap -> IO (ListenerId, ListenerMap)
add l (ListenerMap h lid) = do
    Hash.insert h lid l
    -- xs <- Hash.toList h
    -- lc <- Hash.longestChain h
    -- putStrLn $ "addListener: n=" ++ show (length xs) ++ " lc=" ++ show (length lc)
    return (lid, ListenerMap h (lid+1))

-- | Remove a listener.
delete :: ListenerId -> ListenerMap -> IO ListenerMap
delete uid lm@(ListenerMap h _) = do
    Hash.delete h uid
    return lm
